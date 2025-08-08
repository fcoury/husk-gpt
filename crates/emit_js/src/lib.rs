use syntax::ast::*;
use syntax::ast::{ImportClause, Export, ExportItem};

/// Represents how a pattern should be checked and what bindings it creates
#[derive(Debug, Clone)]
struct PatternCheck {
    /// JavaScript condition to test if the pattern matches
    test_condition: String,
    /// Variable bindings this pattern creates
    bindings: Vec<PatternBinding>,
}

#[derive(Debug, Clone)]
struct PatternBinding {
    /// Name of the variable to bind
    name: String,
    /// JavaScript expression to get the value
    value_expr: String,
}

impl PatternCheck {
    /// Convert a pattern to a PatternCheck that explicitly separates test and bind logic
    fn from_pattern(pattern: &Pattern, scrutinee_var: &str) -> Self {
        match pattern {
            Pattern::Wildcard => PatternCheck {
                test_condition: "true".to_string(),
                bindings: vec![],
            },
            Pattern::Identifier(name) => {
                // Post-normalization, identifiers are always bindings (unit variants converted to Pattern::Variant)
                PatternCheck {
                    test_condition: "true".to_string(),
                    bindings: vec![PatternBinding {
                        name: name.clone(),
                        value_expr: scrutinee_var.to_string(),
                    }],
                }
            },
            Pattern::Literal(lit) => {
                let literal_value = emit_literal(lit);
                PatternCheck {
                    test_condition: format!("{} === {}", scrutinee_var, literal_value),
                    bindings: vec![],
                }
            },
            Pattern::Variant(variant_name, patterns) => {
                let mut bindings = Vec::new();
                
                // Bind variant fields
                for (i, pattern) in patterns.iter().enumerate() {
                    match pattern {
                        Pattern::Identifier(name) => {
                            bindings.push(PatternBinding {
                                name: name.clone(),
                                value_expr: format!("{}[{}]", scrutinee_var, i),
                            });
                        },
                        Pattern::Wildcard => {
                            // Wildcard patterns don't create bindings
                        },
                        _ => {
                            // TODO: Handle nested patterns recursively
                        }
                    }
                }
                
                PatternCheck {
                    test_condition: format!("{}.tag === \"{}\"", scrutinee_var, variant_name),
                    bindings,
                }
            },
            Pattern::Tuple(patterns) => {
                let mut bindings = Vec::new();
                
                for (i, pattern) in patterns.iter().enumerate() {
                    match pattern {
                        Pattern::Identifier(name) => {
                            bindings.push(PatternBinding {
                                name: name.clone(),
                                value_expr: format!("{}[{}]", scrutinee_var, i),
                            });
                        },
                        Pattern::Wildcard => {
                            // Wildcard patterns don't create bindings
                        },
                        _ => {
                            // TODO: Handle nested patterns recursively
                        }
                    }
                }
                
                PatternCheck {
                    test_condition: format!("Array.isArray({})", scrutinee_var),
                    bindings,
                }
            },
        }
    }
}

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
    let arms = &match_expr.arms;
    
    // Choose lowering strategy based on pattern types
    // Check if all patterns are variant patterns (including unit variants like Inactive)
    let all_variant_patterns = arms.iter().all(|arm| {
        match &arm.pattern {
            Pattern::Variant(_, _) => true,
            Pattern::Identifier(name) => {
                // Check if this identifier is actually a unit variant
                // For now, we'll be conservative and use sequential matching
                // TODO: Improve this by checking symbol table
                false
            }
            _ => false,
        }
    });
    
    if all_variant_patterns && !arms.is_empty() {
        // Switch-based lowering for all-variant matches
        emit_variant_switch_match(&scrutinee, arms)
    } else {
        // Sequential if-chain for mixed or non-variant patterns
        emit_sequential_match(&scrutinee, arms)
    }
}

fn emit_variant_switch_match(scrutinee: &str, arms: &[MatchArm]) -> String {
    let mut output = format!("(() => {{\n  const _s = {};\n", scrutinee);
    output.push_str("  switch (_s.tag) {\n");
    
    for arm in arms {
        if let Pattern::Variant(variant_name, _) = &arm.pattern {
            output.push_str(&format!("    case \"{}\":\n", variant_name));
            
            // Use PatternCheck abstraction for consistency
            let pattern_check = PatternCheck::from_pattern(&arm.pattern, "_s");
            
            // Emit pattern bindings
            for binding in &pattern_check.bindings {
                output.push_str(&format!("      const {} = {};\n", binding.name, binding.value_expr));
            }
            
            // Handle optional guard
            if let Some(ref guard) = arm.guard {
                output.push_str(&format!("      if (!({}) ) break;\n", emit_expression(guard)));
            }
            
            output.push_str(&format!("      return {};\n", emit_expression(&arm.body)));
        }
    }
    
    output.push_str("    default:\n");
    output.push_str("      throw new Error('Non-exhaustive match');\n");
    output.push_str("  }\n})()");
    
    output
}

fn emit_sequential_match(scrutinee: &str, arms: &[MatchArm]) -> String {
    let mut output = format!("(() => {{\n  const _s = {};\n", scrutinee);
    
    for arm in arms {
        // Use PatternCheck abstraction for explicit test vs bind separation
        let pattern_check = PatternCheck::from_pattern(&arm.pattern, "_s");
        
        // Emit the test condition
        output.push_str(&format!("  if ({}) {{\n", pattern_check.test_condition));
        
        // Emit pattern bindings
        for binding in &pattern_check.bindings {
            output.push_str(&format!("    const {} = {};\n", binding.name, binding.value_expr));
        }
        
        // Handle optional guard - now that bindings are in scope
        if let Some(ref guard) = arm.guard {
            output.push_str(&format!("    if ({}) {{\n", emit_expression(guard)));
            output.push_str(&format!("      return {};\n", emit_expression(&arm.body)));
            output.push_str("    }\n");
        } else {
            output.push_str(&format!("    return {};\n", emit_expression(&arm.body)));
        }
        
        output.push_str("  }\n");
    }
    
    output.push_str("  throw new Error('Non-exhaustive match');\n");
    output.push_str("})()");
    
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
    
    let rewritten_path = rewrite_import_path(&import.path);
    format!("import {} from \"{}\";", import_clause, rewritten_path)
}

fn rewrite_import_path(path: &str) -> String {
    // If it's a relative path (./ or ../), add .js extension
    if path.starts_with("./") || path.starts_with("../") {
        // Check if it already has an extension
        if path.ends_with(".hk") {
            // Replace .hk with .js for Husk source files
            path.replace(".hk", ".js")
        } else if path.ends_with('/') {
            // Directory import, leave as-is
            path.to_string()
        } else if path.rfind('.').map_or(true, |dot_pos| dot_pos < path.rfind('/').unwrap_or(0)) {
            // No extension after the last slash, add .js
            format!("{}.js", path)
        } else {
            // Has extension, keep as-is
            path.to_string()
        }
    } else if path.starts_with('/') {
        // Absolute paths - add .js extension if needed
        if path.ends_with(".hk") {
            path.replace(".hk", ".js")
        } else if path.ends_with('/') {
            path.to_string()
        } else if path.rfind('.').map_or(true, |dot_pos| dot_pos < path.rfind('/').unwrap_or(0)) {
            format!("{}.js", path)
        } else {
            path.to_string()
        }
    } else {
        // Package imports (no ./ or ../) - leave as-is for node_modules
        path.to_string()
    }
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
            let rewritten_path = rewrite_import_path(path);
            format!("export {{ {} }} from \"{}\";", names, rewritten_path)
        }
        Export::All(path) => {
            let rewritten_path = rewrite_import_path(path);
            format!("export * from \"{}\";", rewritten_path)
        }
    }
}
