use syntax::ast::*;
use syntax::ast::{ImportClause, Export, ExportItem};

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
            Item::Import(import) => {
                output.push_str(&emit_import(import));
                output.push('\n');
            }
            Item::Export(export) => {
                output.push_str(&emit_export(export));
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
        if variant.fields.is_empty() {
            // Unit variant
            output.push_str(&format!(
                "export const {} = {{ tag: \"{}\" }};\n",
                format!("{}_{}", enm.name, variant.name),
                variant.name
            ));
        } else {
            // Tuple variant (simplified - all variants with fields are treated as tuples)
            let params = variant.fields
                .iter()
                .enumerate()
                .map(|(i, _)| format!("arg{}", i))
                .collect::<Vec<_>>()
                .join(", ");
            
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

fn emit_struct(strct: &Struct) -> String {
    let vis = match strct.vis {
        Visibility::Public => "export ",
        Visibility::Private => "",
    };
    
    // Generate constructor function that returns plain object
    let field_params = strct.fields
        .iter()
        .filter(|f| matches!(f.vis, Visibility::Public) || strct.vis == Visibility::Private)
        .map(|f| f.name.clone())
        .collect::<Vec<_>>()
        .join(", ");
    
    let field_returns = strct.fields
        .iter()
        .filter(|f| matches!(f.vis, Visibility::Public) || strct.vis == Visibility::Private)
        .map(|f| f.name.clone())
        .collect::<Vec<_>>()
        .join(", ");
    
    format!(
        "{}function {}({{ {} }}) {{ return {{ {} }}; }}",
        vis, strct.name, field_params, field_returns
    )
}

fn emit_block(block: &Block) -> String {
    let mut output = String::from(" {\n");
    
    for (i, stmt) in block.statements.iter().enumerate() {
        output.push_str("  ");
        let is_last = i == block.statements.len() - 1;
        output.push_str(&emit_statement(&stmt.value, is_last));
        output.push('\n');
    }
    
    output.push('}');
    output
}

fn emit_statement(stmt: &Statement, is_last: bool) -> String {
    match stmt {
        Statement::Expression(expr) => {
            if is_last {
                format!("return {};", emit_expression(expr))
            } else {
                format!("{};", emit_expression(expr))
            }
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
        Expression::VariantCtor { enum_name, variant, args } => {
            let fname = format!("{}_{}", enum_name, variant);
            let args = args.iter().map(emit_expression).collect::<Vec<_>>().join(", ");
            format!("{}({})", fname, args)
        }
        Expression::Binary(binary) => {
            let left = emit_expression(&binary.left);
            let right = emit_expression(&binary.right);
            let op = match binary.operator {
                BinaryOperator::Add => "+",
                BinaryOperator::Subtract => "-",
                BinaryOperator::Multiply => "*",
                BinaryOperator::Divide => "/",
                BinaryOperator::Modulo => "%",
                BinaryOperator::Equal => "===",
                BinaryOperator::NotEqual => "!==",
                BinaryOperator::Less => "<",
                BinaryOperator::LessEqual => "<=",
                BinaryOperator::Greater => ">",
                BinaryOperator::GreaterEqual => ">=",
                BinaryOperator::And => "&&",
                BinaryOperator::Or => "||",
            };
            format!("({} {} {})", left, op, right)
        }
        Expression::ObjectLiteral(fields) => {
            let fields_str = fields
                .iter()
                .map(|field| format!("{}: {}", field.name, emit_expression(&field.value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {} }}", fields_str)
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
                
                // Handle basic pattern destructuring for tuple variants
                for (i, pattern) in patterns.iter().enumerate() {
                    match pattern {
                        Pattern::Identifier(name) => {
                            output.push_str(&format!("      const {} = {}[{}];\n", name, scrutinee, i));
                        }
                        _ => {
                            output.push_str("      // TODO: Complex pattern destructuring\n");
                        }
                    }
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

fn emit_import(import: &Import) -> String {
    let import_clause = match &import.items {
        ImportClause::Named(items) => {
            let named = items
                .iter()
                .map(|item| {
                    if let Some(ref alias) = item.alias {
                        format!("{} as {}", item.name, alias)
                    } else {
                        item.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {} }}", named)
        }
        ImportClause::Namespace(name) => {
            format!("* as {}", name)
        }
        ImportClause::Default(name) => {
            name.clone()
        }
        ImportClause::Mixed { default, named } => {
            let named_str = named
                .iter()
                .map(|item| {
                    if let Some(ref alias) = item.alias {
                        format!("{} as {}", item.name, alias)
                    } else {
                        item.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}, {{ {} }}", default, named_str)
        }
    };
    
    format!("import {} from \"{}\";", import_clause, import.path)
}

fn emit_export(export: &Export) -> String {
    match export {
        Export::Item(item) => {
            // Generate the export by prefixing "export " to the item
            match &item.value {
                Item::Function(func) => {
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
                    
                    format!("export function {}({}) {}", func.name, params, body)
                }
                Item::Enum(enm) => {
                    format!("export {}", emit_enum(enm).trim_end())
                }
                Item::Struct(strct) => {
                    format!("export {}", emit_struct(strct).trim_start())
                }
                _ => {
                    format!("// TODO: Export for {:?}", item.value)
                }
            }
        }
        Export::Default(item) => {
            match &item.value {
                Item::Function(func) => {
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
                    
                    format!("export default function {}({}) {}", func.name, params, body)
                }
                _ => {
                    format!("// TODO: Default export for {:?}", item.value)
                }
            }
        }
        Export::Named(items) => {
            let names = items
                .iter()
                .map(|item| {
                    if let Some(ref alias) = item.alias {
                        format!("{} as {}", item.name, alias)
                    } else {
                        item.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("export {{ {} }};", names)
        }
        Export::NamedFrom { items, path } => {
            let names = items
                .iter()
                .map(|item| {
                    if let Some(ref alias) = item.alias {
                        format!("{} as {}", item.name, alias)
                    } else {
                        item.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("export {{ {} }} from \"{}\";", names, path)
        }
        Export::All(path) => {
            format!("export * from \"{}\";", path)
        }
    }
}
