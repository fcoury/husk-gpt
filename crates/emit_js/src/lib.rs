use syntax::ast::*;

pub fn emit(module: &Module) -> String {
    let mut output = String::new();
    
    for item in &module.items {
        match &item.value {
            Item::Function(func) => {
                output.push_str(&emit_function(func));
                output.push('\n');
            }
            Item::Enum(enm) => {
                output.push_str(&emit_enum(enm));
                output.push('\n');
            }
            Item::Struct(strct) => {
                output.push_str(&emit_struct(strct));
                output.push('\n');
            }
            _ => {
                // TODO: Handle other item types
                output.push_str(&format!("// TODO: Handle {:?}\n", item.value));
            }
        }
    }
    
    output
}

fn emit_function(func: &Function) -> String {
    let vis = match func.vis {
        Visibility::Public => "export ",
        Visibility::Private => "",
    };
    
    let params = func.params
        .iter()
        .map(|p| p.name.clone())
        .collect::<Vec<_>>()
        .join(", ");
    
    let body = if let Some(ref body) = func.body {
        emit_block(body)
    } else {
        "{ /* external function */ }".to_string()
    };
    
    format!("{}function {}({}) {}", vis, func.name, params, body)
}

fn emit_enum(enm: &Enum) -> String {
    let mut output = String::new();
    
    // Generate factory functions for each variant
    for variant in &enm.variants {
        let params = variant.fields
            .iter()
            .enumerate()
            .map(|(i, _)| format!("arg{}", i))
            .collect::<Vec<_>>()
            .join(", ");
        
        if variant.fields.is_empty() {
            output.push_str(&format!(
                "export const {} = {{ tag: \"{}\" }};\n",
                format!("{}_{}", enm.name, variant.name),
                variant.name
            ));
        } else {
            let fields = variant.fields
                .iter()
                .enumerate()
                .map(|(i, _)| format!("{}: arg{}", i, i))
                .collect::<Vec<_>>()
                .join(", ");
                
            output.push_str(&format!(
                "export function {}({}) {{ return {{ tag: \"{}\", {} }}; }}\n",
                format!("{}_{}", enm.name, variant.name),
                params,
                variant.name,
                fields
            ));
        }
    }
    
    // Generate namespace object
    output.push_str(&format!(
        "export const {} = {{ {} }};\n",
        enm.name,
        enm.variants
            .iter()
            .map(|v| format!("{}: {}", v.name, format!("{}_{}", enm.name, v.name)))
            .collect::<Vec<_>>()
            .join(", ")
    ));
    
    output
}

fn emit_struct(_strct: &Struct) -> String {
    // For now, just emit a comment
    "// TODO: Struct emission not implemented".to_string()
}

fn emit_block(block: &Block) -> String {
    let mut output = String::from(" {\n");
    
    for stmt in &block.statements {
        output.push_str("  ");
        output.push_str(&emit_statement(&stmt.value));
        output.push('\n');
    }
    
    output.push('}');
    output
}

fn emit_statement(stmt: &Statement) -> String {
    match stmt {
        Statement::Expression(expr) => {
            format!("{};", emit_expression(expr))
        }
        Statement::Let(let_stmt) => {
            let keyword = if let_stmt.mutable { "let" } else { "const" };
            format!("{} {} = {};", keyword, let_stmt.name, emit_expression(&let_stmt.value))
        }
    }
}

fn emit_expression(expr: &Expression) -> String {
    match expr {
        Expression::Literal(lit) => emit_literal(lit),
        Expression::Identifier(name) => name.clone(),
        Expression::Call(call) => {
            let callee = emit_expression(&call.callee);
            let args = call.args
                .iter()
                .map(emit_expression)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", callee, args)
        }
        Expression::Member(member) => {
            let object = emit_expression(&member.object);
            format!("{}.{}", object, member.property)
        }
        Expression::Match(match_expr) => {
            emit_match_expression(match_expr)
        }
        Expression::Block(block) => {
            emit_block(block)
        }
    }
}

fn emit_match_expression(match_expr: &MatchExpression) -> String {
    let scrutinee = emit_expression(&match_expr.scrutinee);
    
    let mut output = format!("(() => {{\n  switch ({}.tag) {{\n", scrutinee);
    
    for arm in &match_expr.arms {
        match &arm.pattern {
            Pattern::Variant(variant_name, patterns) => {
                output.push_str(&format!("    case \"{}\":\n", variant_name));
                
                // TODO: Handle pattern destructuring
                if !patterns.is_empty() {
                    output.push_str("      // TODO: Pattern destructuring\n");
                }
                
                output.push_str(&format!("      return {};\n", emit_expression(&arm.body)));
            }
            _ => {
                output.push_str("    default:\n");
                output.push_str(&format!("      return {};\n", emit_expression(&arm.body)));
            }
        }
    }
    
    output.push_str("  }\n})()");
    output
}

fn emit_literal(lit: &Literal) -> String {
    match lit {
        Literal::String(s) => format!("\"{}\"", s.replace('"', "\\\"")),
        Literal::Number(n) => n.clone(),
        Literal::Bool(b) => b.to_string(),
    }
}
