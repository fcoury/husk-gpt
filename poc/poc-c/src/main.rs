use anyhow::{Context as _, Result};
use deno_core::v8::Local;
use deno_core::{JsRuntime, RuntimeOptions, v8};
use serde_json as json;
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::time::Instant;

// Copy from node_modules/typescript/lib/typescript.js
static TS_BUNDLE: &str = include_str!("../vendor/typescript.js");

fn main() -> Result<()> {
    let mut rt = JsRuntime::new(RuntimeOptions::default());

    // Shims
    rt.execute_script("<shims>", r#"
        if (!globalThis.performance) globalThis.performance = { now: () => Date.now() };
        if (!globalThis.console) globalThis.console = { log(){}, warn(){}, error(){} };
        if (!globalThis.setTimeout) globalThis.setTimeout = function(){};
        if (!globalThis.clearTimeout) globalThis.clearTimeout = function(){};
        if (!globalThis.queueMicrotask) globalThis.queueMicrotask = (fn)=>Promise.resolve().then(fn);
        if (!globalThis.self) globalThis.self = globalThis;
    "#).context("installing shims")?;

    // Load TypeScript bundle
    rt.execute_script("<ts>", TS_BUNDLE)
        .context("evaluating TypeScript bundle")?;
    rt.execute_script(
        "<check>",
        r#"if (!globalThis.ts) throw new Error("ts not found");"#,
    )
    .context("checking ts global")?;

    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin.lock());
    let mut stdout = BufWriter::new(io::stdout().lock());
    let mut line = String::new();

    loop {
        line.clear();
        if reader.read_line(&mut line)? == 0 {
            break;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let req: json::Value = match json::from_str(trimmed) {
            Ok(v) => v,
            Err(e) => {
                writeln!(
                    stdout,
                    "{}",
                    json::json!({"ok": false, "error": format!("bad json: {e}")})
                )?;
                stdout.flush()?;
                continue;
            }
        };

        let method = req.get("method").and_then(|v| v.as_str()).unwrap_or("");
        if method != "evalType" {
            writeln!(
                stdout,
                "{}",
                json::json!({"ok": false, "error": format!("unknown method: {method}")})
            )?;
            stdout.flush()?;
            continue;
        }

        let files = req.get("files").cloned().unwrap_or(json::json!({}));
        let root = req
            .get("root")
            .and_then(|v| v.as_str())
            .unwrap_or("/a.d.ts");
        let expr = req
            .get("expr")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown");

        let js = format!(
            r#"(function () {{
                try {{
                    const ts = globalThis.ts;
                    const files = {files_json};
                    const root = {root_json};
                    const expr = {expr_json};

                    const host = {{
                        fileExists: f => !!files[f],
                        readFile: f => files[f],
                        getSourceFile: (f, lang) => {{
                            const src = files[f];
                            if (!src) return undefined;
                            return ts.createSourceFile(f, src, ts.ScriptTarget.ES2020, true, ts.ScriptKind.TS);
                        }},
                        getDefaultLibFileName: () => "lib.d.ts",
                        writeFile: () => {{}},
                        getCurrentDirectory: () => "",
                        getDirectories: () => [],
                        getCanonicalFileName: f => f,
                        useCaseSensitiveFileNames: () => true,
                        getNewLine: () => "\n",
                    }};

                    files[root] = (files[root] || "") + "\\n type __HUSK_EVAL__ = " + expr + ";\\n";

                    const program = ts.createProgram(Object.keys(files), {{ noLib: true, target: ts.ScriptTarget.ES2020 }}, host);
                    const checker = program.getTypeChecker();
                    const sf = program.getSourceFile(root);
                    if (!sf) throw new Error("root source not found: " + root);
                    const alias = sf.statements.find(s => ts.isTypeAliasDeclaration(s) && s.name?.text === "__HUSK_EVAL__");
                    if (!alias) throw new Error("alias not found");

                    const ty = checker.getTypeFromTypeNode(alias.type);
                    function normalize(t) {{
                        const props = checker.getPropertiesOfType(t).map(sym => {{
                            const tt = checker.getTypeOfSymbolAtLocation(sym, sf);
                            const optional = (sym.flags & ts.SymbolFlags.Optional) !== 0;
                            return {{ name: String(sym.escapedName), type: checker.typeToString(tt), optional }};
                        }});
                        const members = (t.isUnion() ? t.types : [t])
                          .filter(tt => (tt.flags & ts.TypeFlags.StringLiteral) !== 0)
                          .map(tt => tt.value);
                        return {{ kind: "object_like", props, stringLiterals: members }};
                    }}
                    return JSON.stringify({{ ok: true, result: normalize(ty) }});
                }} catch (e) {{
                    return JSON.stringify({{ ok: false, error: String(e && e.stack || e) }});
                }}
            }})();"#,
            files_json = json::to_string(&files).unwrap(),
            root_json = json::to_string(root).unwrap(),
            expr_json = json::to_string(expr).unwrap()
        );

        let t0 = Instant::now();
        let js_val = rt
            .execute_script("<eval>", js)
            .unwrap_or_else(|e| v8_string(&mut rt, &format!(r#"{{"ok":false,"error":"{}"}}"#, e)));
        let s = v8_to_string(&mut rt, js_val);
        let mut v: json::Value =
            json::from_str(&s).unwrap_or(json::json!({"ok": false, "error": "bad js json"}));

        if v.get("ok") == Some(&json::Value::Bool(true)) {
            let ms = t0.elapsed().as_secs_f64() * 1000.0;
            v.as_object_mut()
                .unwrap()
                .insert("time_ms".into(), json::json!(ms));
        }

        writeln!(stdout, "{}", json::to_string(&v).unwrap())?;
        stdout.flush()?;
    }

    Ok(())
}

fn v8_string(rt: &mut JsRuntime, s: &str) -> v8::Global<v8::Value> {
    let scope = &mut rt.handle_scope();
    let str_local: Local<v8::String> = v8::String::new(scope, s).unwrap();
    let val_local: Local<v8::Value> = str_local.into();
    v8::Global::new(scope, val_local)
}

fn v8_to_string(rt: &mut JsRuntime, val: v8::Global<v8::Value>) -> String {
    let scope = &mut rt.handle_scope();
    let local: v8::Local<v8::Value> = v8::Local::new(scope, val);
    local.to_rust_string_lossy(scope)
}
