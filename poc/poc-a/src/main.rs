use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

#[derive(Serialize)]
struct Req<'a, T> {
    id: u64,
    method: &'a str,
    params: T,
}

#[derive(Deserialize, Debug)]
struct Prop {
    name: String,
    r#type: String,
    optional: bool,
}

#[derive(Deserialize, Debug)]
struct Resp {
    id: u64,
    result: Option<ResultOk>,
    error: Option<String>,
}
#[derive(Deserialize, Debug)]
struct ResultOk {
    kind: String,
    props: Vec<Prop>,
}

fn main() -> anyhow::Result<()> {
    let mut child = Command::new("node")
        .arg("ts_host.js")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let source = r#"
      interface User { id: string; name: string }
      type Partial<T> = { [K in keyof T]?: T[K] };
    "#;

    let req = Req {
        id: 1,
        method: "resolvePartialUser",
        params: serde_json::json!({ "source": source }),
    };
    let line = serde_json::to_string(&req)? + "\n";
    child.stdin.as_mut().unwrap().write_all(line.as_bytes())?;

    let stdout = child.stdout.take().unwrap();
    let mut rdr = BufReader::new(stdout);
    let mut buf = String::new();
    rdr.read_line(&mut buf)?;
    let resp: Resp = serde_json::from_str(&buf)?;
    if let Some(err) = resp.error {
        anyhow::bail!(err)
    }

    println!("{:#?}", resp.result.unwrap().props);
    Ok(())
}
