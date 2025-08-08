use anyhow::{Context as _, Result};
use rquickjs::{Context, Runtime};
use serde_json as json;
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::time::Instant;

// Copy from node_modules/typescript/lib/typescript.js
static TS_BUNDLE: &str = include_str!("../vendor/typescript.js");

fn main() -> Result<()> {
    // QuickJS runtime + context
    let rt = Runtime::new()?;
    let ctx = Context::full(&rt)?;

    ctx.with(|ctx| -> Result<()> {
        // Minimal shims TS sometimes expects
        ctx.eval::<(), _>(r#"
          if (!globalThis.performance) globalThis.performance = { now: () => Date.now() };
          if (!globalThis.console) globalThis.console = { log(){}, warn(){}, error(){} };
          if (!globalThis.setTimeout) globalThis.setTimeout = function(){};
          if (!globalThis.clearTimeout) globalThis.clearTimeout = function(){};
          if (!globalThis.queueMicrotask) globalThis.queueMicrotask = (fn)=>Promise.resolve().then(fn);
          if (!globalThis.self) globalThis.self = globalThis;
        "#).context("installing shims")?;

        // Load TS UMD bundle (defines globalThis.ts)
        ctx.eval::<(), _>(TS_BUNDLE).context("evaluating TypeScript bundle")?;
        ctx.eval::<(), _>(r#"if (!globalThis.ts) { throw new Error("TypeScript bundle didn't define globalThis.ts"); }"#)
            .context("checking ts global")?;

        // Server loop: read JSON lines on stdin, write JSON lines on stdout
        let stdin = io::stdin();
        let mut reader = BufReader::new(stdin.lock());
        let mut stdout = BufWriter::new(io::stdout().lock());
        let mut line = String::new();

        loop {
            line.clear();
            if reader.read_line(&mut line)? == 0 {
                break; // EOF
            }
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }

            let req: json::Value = match json::from_str(trimmed) {
                Ok(v) => v,
                Err(e) => {
                    writeln!(stdout, "{}", json::json!({"ok": false, "error": format!("bad json: {e}")}))?;
                    stdout.flush()?;
                    continue;
                }
            };

            let method = req.get("method").and_then(|v| v.as_str()).unwrap_or("");
            if method != "evalType" {
                writeln!(stdout, "{}", json::json!({"ok": false, "error": format!("unknown method: {method}")}))?;
                stdout.flush()?;
                continue;
            }

            let files = req.get("files").cloned().unwrap_or(json::json!({}));
            let root = req.get("root").and_then(|v| v.as_str()).unwrap_or("/a.d.ts");
            let expr = req.get("expr").and_then(|v| v.as_str()).unwrap_or("unknown");

            // Build the JS that runs TS on the provided files/expr
            let js = format!(
                r#"
                (function () {{
                  try {{
                    const ts = globalThis.ts;
                    const files = {files_json};
                    const root = {root_json};
                    const expr = {expr_json};

                    // host over virtual files
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

                    // add eval alias into root
                    files[root] = (files[root] || "") + "\\n type __HUSK_EVAL__ = " + expr + ";\\n";

                    const program = ts.createProgram(Object.keys(files), {{ noLib: true, target: ts.ScriptTarget.ES2020 }}, host);
                    const checker = program.getTypeChecker();
                    const sf = program.getSourceFile(root);
                    if (!sf) throw new Error("root source not found: " + root);
                    const alias = sf.statements.find(s => ts.isTypeAliasDeclaration(s) && s.name?.text === "__HUSK_EVAL__");
                    if (!alias) throw new Error("alias not found");

                    const ty = checker.getTypeFromTypeNode(alias.type);

                    function normalize(t) {{
                      // props
                      const props = checker.getPropertiesOfType(t).map(sym => {{
                        const tt = checker.getTypeOfSymbolAtLocation(sym, sf);
                        const optional = (sym.flags & ts.SymbolFlags.Optional) !== 0;
                        return {{ name: String(sym.escapedName), type: checker.typeToString(tt), optional }};
                      }});
                      // string literal union members (handy for exhaustiveness)
                      const members = (t.isUnion() ? t.types : [t])
                        .filter(tt => (tt.flags & ts.TypeFlags.StringLiteral) !== 0)
                        .map(tt => tt.value);
                      return {{ kind: "object_like", props, stringLiterals: members }};
                    }}

                    return JSON.stringify({{ ok: true, result: normalize(ty) }});
                  }} catch (e) {{
                    return JSON.stringify({{ ok: false, error: String(e && e.stack || e) }});
                  }}
                }})();
                "#,
                files_json = json::to_string(&files).unwrap(),
                root_json = json::to_string(root).unwrap(),
                expr_json = json::to_string(expr).unwrap()
            );

            let t0 = Instant::now();
            let out: String = ctx.eval(js).unwrap_or_else(|e| {
                // If JS throws at the embedding level, surface it as JSON
                format!(r#"{{"ok":false,"error":"rquickjs eval error: {}"}}"#, e)
            });
            let mut v: json::Value = json::from_str(&out).unwrap_or(json::json!({"ok":false,"error":"bad js json"}));

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
    })
}
