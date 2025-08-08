use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json as json;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::time::Instant;

#[derive(Clone)]
struct Target {
    name: &'static str,
    cmd: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone)]
struct Case {
    name: String,
    files: json::Value,
    root: String,
    expr: String,
}

#[derive(Serialize)]
struct RunResult {
    target: String,
    cold_ms: f64,
    p50_ms: f64,
    p95_ms: f64,
    p99_ms: f64,
    samples: Vec<f64>,
    errors: Vec<String>,
}

fn main() -> Result<()> {
    // --- configure targets --- adjust paths if needed
    let targets = vec![
        Target {
            name: "A-Node",
            cmd: vec!["node".into(), "../poc-a/ts_host.js".into()],
        },
        Target {
            name: "B-QuickJS",
            cmd: vec!["../poc-b/target/debug/poc-b".into()],
        },
        Target {
            name: "C-V8",
            cmd: vec!["../poc-c/target/debug/poc-c".into()],
        },
    ];

    // --- build default cases (add more as you wish) ---
    let cases = default_cases();

    // --- ensure B/C are built ---
    build("./../poc-b")?;
    build("./../poc-c")?;

    // --- run each target ---
    let warm_loops = 100; // each case repeated this many times
    let mut all = vec![];
    for t in targets {
        println!("==> {}", t.name);
        let mut res =
            run_target(&t, &cases, warm_loops).with_context(|| format!("running {}", t.name))?;
        print_summary(&res);
        all.push(res);
    }

    // write artifact
    std::fs::create_dir_all("out")?;
    let ts = chrono_like_epoch_ms();
    std::fs::write(
        format!("out/results-{}.json", ts),
        json::to_string_pretty(&all)?,
    )?;
    Ok(())
}

fn default_cases() -> Vec<Case> {
    let mut cases = vec![];

    let user = r#"interface User { id: string; name: string }"#;

    // 1) Partial<User>
    cases.push(Case {
        name: "Partial<User>".into(),
        files: json::json!({"/a.d.ts": format!("{user}\n type Partial<T> = {{ [K in keyof T]?: T[K] }};")}),
        root: "/a.d.ts".into(),
        expr: "Partial<User>".into(),
    });

    // 2) Pick<User,"id">
    cases.push(Case {
        name: r#"Pick<User,"id">"#.into(),
        files: json::json!({"/a.d.ts": format!("{user}\n type Pick<T,K extends keyof T> = {{ [P in K]: T[P] }};")}),
        root: "/a.d.ts".into(),
        expr: r#"Pick<User,"id">"#.into(),
    });

    // 3) literal union
    cases.push(Case {
        name: r#""A"|"B"|"C""#.into(),
        files: json::json!({"/a.d.ts": ""}),
        root: "/a.d.ts".into(),
        expr: r#""A" | "B" | "C""#.into(),
    });

    // 4) conditional resolves
    cases.push(Case {
        name: "Unbox<Box<string>>".into(),
        files: json::json!({"/a.d.ts": r#"
            type Box<T> = { value: T };
            type Unbox<T> = T extends Box<infer U> ? U : T;
        "#}),
        root: "/a.d.ts".into(),
        expr: "Unbox<Box<string>>".into(),
    });

    // 5) conditional generic (opaque)
    cases.push(Case {
        name: "Unbox<T>".into(),
        files: json::json!({"/a.d.ts": r#"
            type Box<T> = { value: T };
            type Unbox<T> = T extends Box<infer U> ? U : T;
        "#}),
        root: "/a.d.ts".into(),
        expr: "Unbox<T>".into(),
    });

    // 6) big mapped
    let keys: String = (1..=100)
        .map(|i| format!(r#""k{}"|"#, i))
        .collect::<String>()
        + r#""k101""#;
    cases.push(Case {
        name: "Partial<Record<k1..k101, number>>".into(),
        files: json::json!({"/a.d.ts": format!("type Keys = {keys}; type Record<K extends string, T> = {{ [P in K]: T }};")}),
        root: "/a.d.ts".into(),
        expr: "Partial<Record<Keys, number>>".into(),
    });

    cases
}

fn build(path: &str) -> Result<()> {
    if std::path::Path::new(&format!("{path}/target/debug")).exists() {
        return Ok(());
    }
    let status = Command::new("cargo")
        .arg("build")
        .current_dir(path)
        .status()
        .context("building target")?;
    if !status.success() {
        bail!("cargo build failed in {}", path);
    }
    Ok(())
}

fn spawn_target(t: &Target) -> Result<Child> {
    let mut cmd = Command::new(&t.cmd[0]);
    for arg in &t.cmd[1..] {
        cmd.arg(arg);
    }
    let child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .with_context(|| format!("spawning {:?}", t.cmd))?;
    Ok(child)
}

fn run_target(t: &Target, cases: &[Case], loops: usize) -> Result<RunResult> {
    let mut child = spawn_target(t)?;
    let mut stdin = child.stdin.take().context("stdin")?;
    let stdout = child.stdout.take().context("stdout")?;
    let mut rdr = BufReader::new(stdout);

    // cold timing: spawn -> first successful response
    let cold_start = Instant::now();
    let cold = send_req(&mut stdin, &mut rdr, &cases[0]).context("cold request")?;
    let cold_ms_total = cold_start.elapsed().as_secs_f64() * 1000.0;

    let mut samples = Vec::with_capacity(loops * cases.len());
    let mut errors = vec![];

    for _ in 0..loops {
        for c in cases {
            match send_req(&mut stdin, &mut rdr, c) {
                Ok(ms) => samples.push(ms),
                Err(e) => errors.push(format!("{}: {}", c.name, e)),
            }
        }
    }

    // compute stats
    let mut v = samples.clone();
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let p50 = percentile(&v, 0.50);
    let p95 = percentile(&v, 0.95);
    let p99 = percentile(&v, 0.99);

    Ok(RunResult {
        target: t.name.into(),
        cold_ms: cold_ms_total,
        p50_ms: p50,
        p95_ms: p95,
        p99_ms: p99,
        samples,
        errors,
    })
}

fn send_req<R: BufRead>(stdin: &mut impl Write, rdr: &mut R, case: &Case) -> Result<f64> {
    let req = json::json!({
        "method":"evalType",
        "files": case.files,
        "root": case.root,
        "expr": case.expr,
    });
    writeln!(stdin, "{}", json::to_string(&req)?)?;
    stdin.flush()?;

    let mut line = String::new();
    line.clear();
    rdr.read_line(&mut line).context("reading line")?;
    let v: json::Value = json::from_str(&line).context("parsing json")?;
    if v.get("ok") != Some(&json::Value::Bool(true)) {
        let err = v
            .get("error")
            .and_then(|e| e.as_str())
            .unwrap_or("unknown error");
        anyhow::bail!(err.to_string());
    }
    let ms = v
        .get("time_ms")
        .and_then(|x| x.as_f64())
        .unwrap_or_default();
    Ok(ms)
}

fn percentile(sorted: &[f64], p: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    let idx = ((sorted.len() as f64 - 1.0) * p).round() as usize;
    sorted[idx]
}

fn chrono_like_epoch_ms() -> u128 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis()
}

fn print_summary(r: &RunResult) {
    println!(
        "cold={:.1} ms | p50={:.2} ms | p95={:.2} ms | p99={:.2} ms | samples={}",
        r.cold_ms,
        r.p50_ms,
        r.p95_ms,
        r.p99_ms,
        r.samples.len()
    );
    if !r.errors.is_empty() {
        eprintln!("errors ({}):", r.errors.len());
        for e in r.errors.iter().take(5) {
            eprintln!("  - {e}");
        }
    }
}
