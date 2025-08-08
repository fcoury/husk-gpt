#!/usr/bin/env node
import * as ts from "typescript";

process.stdin.setEncoding("utf8");
let buf = "";

// line-oriented JSON input
process.stdin.on("data", (chunk) => {
  buf += chunk;
  let idx;
  while ((idx = buf.indexOf("\n")) >= 0) {
    const line = buf.slice(0, idx).trim();
    buf = buf.slice(idx + 1);
    if (!line) continue;
    let req;
    try {
      req = JSON.parse(line);
    } catch (e) {
      writeJSON({ ok: false, error: `bad json: ${e}` });
      continue;
    }

    const method = req.method;
    if (method === "evalType") {
      const files = req.files || {};
      const root = req.root || "/a.d.ts";
      const expr = req.expr || "unknown";
      const t0 = performance.now();
      const { result, error } = evalType(files, root, expr);
      const time_ms = performance.now() - t0;
      if (error) writeJSON({ ok: false, error, time_ms });
      else writeJSON({ ok: true, result, time_ms });
    } else {
      writeJSON({ ok: false, error: `unknown method: ${method}` });
    }
  }
});

function writeJSON(obj) {
  process.stdout.write(JSON.stringify(obj) + "\n");
}

function evalType(files, root, expr) {
  try {
    const host = {
      fileExists: (f) => !!files[f],
      readFile: (f) => files[f],
      getSourceFile: (f, lang) => {
        const src = files[f];
        if (!src) return undefined;
        return ts.createSourceFile(
          f,
          src,
          ts.ScriptTarget.ES2020,
          true,
          ts.ScriptKind.TS,
        );
      },
      getDefaultLibFileName: () => "lib.d.ts",
      writeFile: () => {},
      getCurrentDirectory: () => "",
      getDirectories: () => [],
      getCanonicalFileName: (f) => f,
      useCaseSensitiveFileNames: () => true,
      getNewLine: () => "\n",
    };

    // append type alias to root
    files[root] = (files[root] || "") + `\n type __HUSK_EVAL__ = ${expr};\n`;

    const program = ts.createProgram(Object.keys(files), {
      noLib: true,
      target: ts.ScriptTarget.ES2020,
    }, host);
    const checker = program.getTypeChecker();
    const sf = program.getSourceFile(root);
    if (!sf) throw new Error(`root source not found: ${root}`);
    const alias = sf.statements.find((s) =>
      ts.isTypeAliasDeclaration(s) && s.name?.text === "__HUSK_EVAL__"
    );
    if (!alias) throw new Error("alias not found");

    const ty = checker.getTypeFromTypeNode(alias.type);
    const result = normalizeType(ts, checker, ty, sf);
    return { result };
  } catch (e) {
    return { error: String(e && e.stack || e) };
  }
}

function normalizeType(ts, checker, t, sf) {
  const props = checker.getPropertiesOfType(t).map((sym) => {
    const tt = checker.getTypeOfSymbolAtLocation(sym, sf);
    const optional = (sym.flags & ts.SymbolFlags.Optional) !== 0;
    return {
      name: String(sym.escapedName),
      type: checker.typeToString(tt),
      optional,
    };
  });
  const members = (t.isUnion() ? t.types : [t])
    .filter((tt) => (tt.flags & ts.TypeFlags.StringLiteral) !== 0)
    .map((tt) => tt.value);
  return { kind: "object_like", props, stringLiterals: members };
}
