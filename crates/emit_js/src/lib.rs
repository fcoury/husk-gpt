#![allow(clippy::uninlined_format_args, clippy::unnecessary_map_or)]

use interop::path_rewriting::rewrite_import_path_for_js;
use std::collections::HashSet;
use syntax::ast::*;
use syntax::ast::{Export, ImportClause};

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
    /// known_variants: set of unit/tuple variant names to disambiguate identifiers
    fn from_pattern(
        pattern: &Pattern,
        scrutinee_var: &str,
        known_variants: &HashSet<String>,
    ) -> Self {
        match pattern {
            Pattern::Wildcard => PatternCheck {
                test_condition: "true".to_string(),
                bindings: vec![],
            },
            Pattern::Identifier(name) => {
                // If normalization missed a unit variant, treat identifier equal to a known variant as a tag test
                if known_variants.contains(name) {
                    PatternCheck {
                        test_condition: format!("{}.tag === \"{}\"", scrutinee_var, name),
                        bindings: vec![],
                    }
                } else {
                    PatternCheck {
                        test_condition: "true".to_string(),
                        bindings: vec![PatternBinding {
                            name: name.clone(),
                            value_expr: scrutinee_var.to_string(),
                        }],
                    }
                }
            }
            Pattern::Literal(lit) => {
                let literal_value = emit_literal(lit);
                PatternCheck {
                    test_condition: format!("{} === {}", scrutinee_var, literal_value),
                    bindings: vec![],
                }
            }
            Pattern::Variant(variant_name, patterns) => {
                let mut test_conditions =
                    vec![format!("{}.tag === \"{}\"", scrutinee_var, variant_name)];
                let mut bindings = Vec::new();

                // Add exact arity check for non-empty variant patterns
                if !patterns.is_empty() {
                    // For variants, ensure object/non-null and exact structural validation:
                    // - All expected indices exist as own props
                    // - Next index must NOT exist
                    test_conditions.push(format!("typeof {} === 'object' && {} !== null", scrutinee_var, scrutinee_var));
                    let expected_indices: Vec<String> = (0..patterns.len())
                        .map(|i| format!(
                            "Object.prototype.hasOwnProperty.call({}, \"{}\")",
                            scrutinee_var, i
                        ))
                        .collect();
                    let no_extra_check = format!(
                        "!Object.prototype.hasOwnProperty.call({}, \"{}\")",
                        scrutinee_var,
                        patterns.len()
                    );
                    test_conditions.push(format!("({}) && {}", expected_indices.join(" && "), no_extra_check));
                }

                // Handle nested patterns recursively
                for (i, pattern) in patterns.iter().enumerate() {
                    let field_expr = format!("{}[{}]", scrutinee_var, i);
                    let nested_check =
                        PatternCheck::from_pattern(pattern, &field_expr, known_variants);

                    // Combine test conditions with &&
                    if nested_check.test_condition != "true" {
                        test_conditions.push(nested_check.test_condition);
                    }

                    // Add all bindings from nested patterns
                    bindings.extend(nested_check.bindings);
                }

                PatternCheck {
                    test_condition: test_conditions.join(" && "),
                    bindings,
                }
            }
            Pattern::QualifiedVariant {
                variant, patterns, ..
            } => {
                // QualifiedVariant patterns work exactly the same as Variant patterns at runtime
                // The difference is just in how they're resolved by the compiler
                let mut test_conditions =
                    vec![format!("{}.tag === \"{}\"", scrutinee_var, variant)];
                let mut bindings = Vec::new();

                // Add exact arity check for non-empty variant patterns
                if !patterns.is_empty() {
                    // Same object/non-null + hasOwnProperty checks as unqualified variants
                    test_conditions.push(format!("typeof {} === 'object' && {} !== null", scrutinee_var, scrutinee_var));
                    let expected_indices: Vec<String> = (0..patterns.len())
                        .map(|i| format!(
                            "Object.prototype.hasOwnProperty.call({}, \"{}\")",
                            scrutinee_var, i
                        ))
                        .collect();
                    let no_extra_check = format!(
                        "!Object.prototype.hasOwnProperty.call({}, \"{}\")",
                        scrutinee_var,
                        patterns.len()
                    );
                    test_conditions.push(format!("({}) && {}", expected_indices.join(" && "), no_extra_check));
                }

                // Handle nested patterns recursively
                for (i, pattern) in patterns.iter().enumerate() {
                    let field_expr = format!("{}[{}]", scrutinee_var, i);
                    let nested_check =
                        PatternCheck::from_pattern(pattern, &field_expr, known_variants);

                    // Combine test conditions with &&
                    if nested_check.test_condition != "true" {
                        test_conditions.push(nested_check.test_condition);
                    }

                    // Add all bindings from nested patterns
                    bindings.extend(nested_check.bindings);
                }

                PatternCheck {
                    test_condition: test_conditions.join(" && "),
                    bindings,
                }
            }
            Pattern::Tuple(patterns) => {
                let mut test_conditions = Vec::new();
                let mut bindings = Vec::new();

                // Test that it's an object (not an array) and has the expected arity
                test_conditions.push(format!(
                    "typeof {} === 'object' && {} !== null && !Array.isArray({})",
                    scrutinee_var, scrutinee_var, scrutinee_var
                ));

                // Add exact arity check for tuples
                if !patterns.is_empty() {
                    // Exact structural validation with hasOwnProperty
                    let expected_indices: Vec<String> = (0..patterns.len())
                        .map(|i| format!(
                            "Object.prototype.hasOwnProperty.call({}, \"{}\")",
                            scrutinee_var, i
                        ))
                        .collect();
                    let no_extra_check = format!(
                        "!Object.prototype.hasOwnProperty.call({}, \"{}\")",
                        scrutinee_var,
                        patterns.len()
                    );
                    test_conditions.push(format!("({}) && {}", expected_indices.join(" && "), no_extra_check));
                }

                // Handle nested patterns recursively
                for (i, pattern) in patterns.iter().enumerate() {
                    let field_expr = format!("{}[{}]", scrutinee_var, i);
                    let nested_check =
                        PatternCheck::from_pattern(pattern, &field_expr, known_variants);

                    // Combine test conditions with &&
                    if nested_check.test_condition != "true" {
                        test_conditions.push(nested_check.test_condition);
                    }

                    // Add all bindings from nested patterns
                    bindings.extend(nested_check.bindings);
                }

                PatternCheck {
                    test_condition: if test_conditions.is_empty() {
                        "true".to_string()
                    } else {
                        test_conditions.join(" && ")
                    },
                    bindings,
                }
            }
        }
    }
}

pub fn emit_with_known_variants(module: &Module, known_variants: &HashSet<String>) -> String {
    let mut output = String::new();

    for item in &module.items {
        match &item.value {
            Item::Function(func) => {
                output.push_str(&emit_function(func, known_variants));
                output.push('\n');
            }
            Item::Enum(enm) => {
                output.push_str(&emit_enum_definition(enm));

                // Add exports for all enum items
                let exports = get_enum_exports(enm);
                if !exports.is_empty() {
                    output.push_str(&format!("export {{ {} }};\n", exports.join(", ")));
                }
                output.push('\n');
            }
            Item::Struct(strct) => {
                output.push_str(&emit_struct_definition(strct));

                // Add exports for public structs
                let exports = get_struct_exports(strct);
                if !exports.is_empty() {
                    output.push_str(&format!("export {{ {} }};\n", exports.join(", ")));
                }
                output.push('\n');
            }
            Item::Import(import) => {
                output.push_str(&emit_import(import));
                output.push('\n');
            }
            Item::Export(export) => {
                output.push_str(&emit_export(export, known_variants));
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

// Backwards-compatible helper for tests or callers without resolver info
pub fn emit(module: &Module) -> String {
    let known_variants = collect_known_variant_names(module);
    emit_with_known_variants(module, &known_variants)
}

fn collect_known_variant_names(module: &Module) -> HashSet<String> {
    let mut set = HashSet::new();
    for item in &module.items {
        if let Item::Enum(enm) = &item.value {
            for variant in &enm.variants {
                set.insert(variant.name.clone());
            }
        }
    }
    set
}

fn emit_function(func: &Function, known_variants: &HashSet<String>) -> String {
    let vis = match func.vis {
        Visibility::Public => "export ",
        Visibility::Private => "",
    };

    let params = func
        .params
        .iter()
        .map(|p| p.name.clone())
        .collect::<Vec<_>>()
        .join(", ");

    let body = if let Some(ref body) = func.body {
        emit_block(body, known_variants)
    } else {
        "{ /* external function */ }".to_string()
    };

    format!("{}function {}({}) {}", vis, func.name, params, body)
}

fn emit_enum_definition(enm: &Enum) -> String {
    let mut output = String::new();

    // Generate factory functions for each variant
    for variant in &enm.variants {
        if variant.fields.is_empty() {
            // Unit variant
            output.push_str(&format!(
                "const {} = {{ tag: \"{}\" }};\n",
                format!("{}_{}", enm.name, variant.name),
                variant.name
            ));
        } else {
            // Tuple variant (simplified - all variants with fields are treated as tuples)
            let params = variant
                .fields
                .iter()
                .enumerate()
                .map(|(i, _)| format!("arg{}", i))
                .collect::<Vec<_>>()
                .join(", ");

            let fields = variant
                .fields
                .iter()
                .enumerate()
                .map(|(i, _)| format!("{}: arg{}", i, i))
                .collect::<Vec<_>>()
                .join(", ");

            output.push_str(&format!(
                "function {}({}) {{ return {{ tag: \"{}\", {} }}; }}\n",
                format!("{}_{}", enm.name, variant.name),
                params,
                variant.name,
                fields
            ));
        }
    }

    // Generate namespace object
    output.push_str(&format!(
        "const {} = {{ {} }};\n",
        enm.name,
        enm.variants
            .iter()
            .map(|v| format!("{}: {}", v.name, format!("{}_{}", enm.name, v.name)))
            .collect::<Vec<_>>()
            .join(", ")
    ));

    output
}

fn get_enum_exports(enm: &Enum) -> Vec<String> {
    let mut exports = Vec::new();

    // Add variant constructors/values
    for variant in &enm.variants {
        exports.push(format!("{}_{}", enm.name, variant.name));
    }

    // Add namespace object
    exports.push(enm.name.clone());

    exports
}

fn emit_struct_definition(strct: &Struct) -> String {
    // Generate constructor function that returns plain object
    let field_params = strct
        .fields
        .iter()
        .filter(|f| matches!(f.vis, Visibility::Public) || strct.vis == Visibility::Private)
        .map(|f| f.name.clone())
        .collect::<Vec<_>>()
        .join(", ");

    let field_returns = strct
        .fields
        .iter()
        .filter(|f| matches!(f.vis, Visibility::Public) || strct.vis == Visibility::Private)
        .map(|f| f.name.clone())
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "function {}({{ {} }}) {{ return {{ {} }}; }}",
        strct.name, field_params, field_returns
    )
}

fn get_struct_exports(strct: &Struct) -> Vec<String> {
    if matches!(strct.vis, Visibility::Public) {
        vec![strct.name.clone()]
    } else {
        vec![]
    }
}

fn emit_block(block: &Block, known_variants: &HashSet<String>) -> String {
    let mut output = String::from(" {\n");

    for (i, stmt) in block.statements.iter().enumerate() {
        output.push_str("  ");
        let is_last = i == block.statements.len() - 1;
        output.push_str(&emit_statement(&stmt.value, is_last, known_variants));
        output.push('\n');
    }

    output.push('}');
    output
}

fn emit_statement(stmt: &Statement, is_last: bool, known_variants: &HashSet<String>) -> String {
    match stmt {
        Statement::Expression(expr) => {
            if is_last {
                format!("return {};", emit_expression(expr, known_variants))
            } else {
                format!("{};", emit_expression(expr, known_variants))
            }
        }
        Statement::Let(let_stmt) => {
            let keyword = if let_stmt.mutable { "let" } else { "const" };
            format!(
                "{} {} = {};",
                keyword,
                let_stmt.name,
                emit_expression(&let_stmt.value, known_variants)
            )
        }
    }
}

fn emit_expression(expr: &Expression, known_variants: &HashSet<String>) -> String {
    match expr {
        Expression::Literal(lit) => emit_literal(lit),
        Expression::Identifier(name) => name.clone(),
        Expression::Call(call) => {
            let callee = emit_expression(&call.callee, known_variants);
            let args = call
                .args
                .iter()
                .map(|e| emit_expression(e, known_variants))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", callee, args)
        }
        Expression::Member(member) => {
            let object = emit_expression(&member.object, known_variants);
            format!("{}.{}", object, member.property)
        }
        Expression::Match(match_expr) => emit_match_expression(match_expr, known_variants),
        Expression::Block(block) => emit_block(block, known_variants),
        Expression::VariantCtor {
            enum_name,
            variant,
            args,
        } => {
            let fname = format!("{}_{}", enum_name, variant);
            if args.is_empty() {
                // Unit variant constructors are values (const), not functions
                fname
            } else {
                let args = args
                    .iter()
                    .map(|e| emit_expression(e, known_variants))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", fname, args)
            }
        }
        Expression::Binary(binary) => {
            let left = emit_expression(&binary.left, known_variants);
            let right = emit_expression(&binary.right, known_variants);
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
                .map(|field| {
                    format!(
                        "{}: {}",
                        field.name,
                        emit_expression(&field.value, known_variants)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {} }}", fields_str)
        }
    }
}

fn emit_match_expression(match_expr: &MatchExpression, known_variants: &HashSet<String>) -> String {
    let scrutinee = emit_expression(&match_expr.scrutinee, known_variants);
    let arms = &match_expr.arms;

    // Choose lowering strategy based on pattern types
    // Check if all patterns are variant patterns (including unit variants like Inactive)
    let all_variant_patterns = arms.iter().all(|arm| match &arm.pattern {
        Pattern::Variant(_, _) => true,
        Pattern::QualifiedVariant { .. } => true,
        Pattern::Identifier(name) => known_variants.contains(name),
        _ => false,
    });

    // Check if any arm has a guard - if so, fall back to if-chain for correctness
    let any_guard = arms.iter().any(|a| a.guard.is_some());

    // Check if any patterns have nested structure that requires complex conditions
    let has_nested_patterns = arms.iter().any(|arm| match &arm.pattern {
        Pattern::Variant(_, patterns) => !patterns.is_empty(),
        Pattern::QualifiedVariant { patterns, .. } => !patterns.is_empty(),
        _ => false,
    });

    if all_variant_patterns && !any_guard && !has_nested_patterns && !arms.is_empty() {
        // Switch-based lowering for simple variant matches without guards or nesting
        emit_variant_switch_match(&scrutinee, arms, known_variants)
    } else {
        // Sequential if-chain for complex patterns, guards, or nested patterns
        emit_sequential_match(&scrutinee, arms, known_variants)
    }
}

fn emit_variant_switch_match(
    scrutinee: &str,
    arms: &[MatchArm],
    known_variants: &HashSet<String>,
) -> String {
    let mut output = format!("(() => {{\n  const _s = {};\n", scrutinee);
    output.push_str("  switch (_s.tag) {\n");

    for arm in arms {
        match &arm.pattern {
            Pattern::Variant(variant_name, _) => {
                output.push_str(&format!("    case \"{}\":\n", variant_name));
                let pattern_check = PatternCheck::from_pattern(&arm.pattern, "_s", known_variants);
                for binding in &pattern_check.bindings {
                    output.push_str(&format!(
                        "      const {} = {};\n",
                        binding.name, binding.value_expr
                    ));
                }
                if let Some(ref guard) = arm.guard {
                    output.push_str(&format!(
                        "      if (!({}) ) break;\n",
                        emit_expression(guard, known_variants)
                    ));
                }
                output.push_str(&format!(
                    "      return {};\n",
                    emit_expression(&arm.body, known_variants)
                ));
            }
            Pattern::QualifiedVariant { variant, .. } => {
                // QualifiedVariant patterns work the same as Variant at runtime - use variant name for case
                output.push_str(&format!("    case \"{}\":\n", variant));
                let pattern_check = PatternCheck::from_pattern(&arm.pattern, "_s", known_variants);
                for binding in &pattern_check.bindings {
                    output.push_str(&format!(
                        "      const {} = {};\n",
                        binding.name, binding.value_expr
                    ));
                }
                if let Some(ref guard) = arm.guard {
                    output.push_str(&format!(
                        "      if (!({}) ) break;\n",
                        emit_expression(guard, known_variants)
                    ));
                }
                output.push_str(&format!(
                    "      return {};\n",
                    emit_expression(&arm.body, known_variants)
                ));
            }
            Pattern::Identifier(name) if known_variants.contains(name) => {
                // Treat bare identifier that is a known unit variant as a variant arm
                output.push_str(&format!("    case \"{}\":\n", name));
                if let Some(ref guard) = arm.guard {
                    output.push_str(&format!(
                        "      if (!({}) ) break;\n",
                        emit_expression(guard, known_variants)
                    ));
                }
                output.push_str(&format!(
                    "      return {};\n",
                    emit_expression(&arm.body, known_variants)
                ));
            }
            _ => {}
        }
    }

    output.push_str("    default:\n");
    output.push_str("      throw new Error('Non-exhaustive match');\n");
    output.push_str("  }\n})()");

    output
}

fn emit_sequential_match(
    scrutinee: &str,
    arms: &[MatchArm],
    known_variants: &HashSet<String>,
) -> String {
    let mut output = format!("(() => {{\n  const _s = {};\n", scrutinee);

    for arm in arms {
        // Use PatternCheck abstraction for explicit test vs bind separation
        let pattern_check = PatternCheck::from_pattern(&arm.pattern, "_s", known_variants);

        // Emit the test condition
        output.push_str(&format!("  if ({}) {{\n", pattern_check.test_condition));

        // Emit pattern bindings
        for binding in &pattern_check.bindings {
            output.push_str(&format!(
                "    const {} = {};\n",
                binding.name, binding.value_expr
            ));
        }

        // Handle optional guard - now that bindings are in scope
        if let Some(ref guard) = arm.guard {
            output.push_str(&format!(
                "    if ({}) {{\n",
                emit_expression(guard, known_variants)
            ));
            output.push_str(&format!(
                "      return {};\n",
                emit_expression(&arm.body, known_variants)
            ));
            output.push_str("    }\n");
        } else {
            output.push_str(&format!(
                "    return {};\n",
                emit_expression(&arm.body, known_variants)
            ));
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
        ImportClause::Default(name) => name.clone(),
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

    let rewritten_path = rewrite_import_path_for_js(&import.path);
    format!("import {} from \"{}\";", import_clause, rewritten_path)
}


fn emit_export(export: &Export, known_variants: &HashSet<String>) -> String {
    match export {
        Export::Item(item) => {
            // Generate the export by prefixing "export " to the item
            match &item.value {
                Item::Function(func) => {
                    let params = func
                        .params
                        .iter()
                        .map(|p| p.name.clone())
                        .collect::<Vec<_>>()
                        .join(", ");

                    let body = if let Some(ref body) = func.body {
                        emit_block(body, known_variants)
                    } else {
                        "{ /* external function */ }".to_string()
                    };

                    format!("export function {}({}) {}", func.name, params, body)
                }
                Item::Enum(enm) => {
                    let mut output = emit_enum_definition(enm);
                    let exports = get_enum_exports(enm);
                    if !exports.is_empty() {
                        output.push_str(&format!("export {{ {} }};", exports.join(", ")));
                    }
                    output
                }
                Item::Struct(strct) => {
                    let mut output = emit_struct_definition(strct);
                    let exports = get_struct_exports(strct);
                    if !exports.is_empty() {
                        output.push_str(&format!("\nexport {{ {} }};", exports.join(", ")));
                    }
                    output
                }
                _ => {
                    format!("// TODO: Export for {:?}", item.value)
                }
            }
        }
        Export::Default(item) => match &item.value {
            Item::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");

                let body = if let Some(ref body) = func.body {
                    emit_block(body, known_variants)
                } else {
                    "{ /* external function */ }".to_string()
                };

                format!("export default function {}({}) {}", func.name, params, body)
            }
            _ => {
                format!("// TODO: Default export for {:?}", item.value)
            }
        },
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
            let rewritten_path = rewrite_import_path_for_js(path);
            format!("export {{ {} }} from \"{}\";", names, rewritten_path)
        }
        Export::All(path) => {
            let rewritten_path = rewrite_import_path_for_js(path);
            format!("export * from \"{}\";", rewritten_path)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntax::{Lexer, Parser as HuskParser};

    fn parse_husk_code(input: &str) -> Module {
        let mut lexer = Lexer::new(input.to_string(), 0);
        let tokens = lexer.tokenize();
        let mut parser = HuskParser::new(tokens);
        let (module, diagnostics) = parser.parse();

        if !diagnostics.is_empty() {
            panic!("Parse errors: {:?}", diagnostics);
        }

        module
    }

    #[test]
    fn test_emit_enum_with_exports() {
        let input = r#"
            pub enum Status {
                Active,
                Inactive,
                Pending(string)
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should have separate const declarations and export statement
        assert!(output.contains("const Status_Active = { tag: \"Active\" };"));
        assert!(
            output.contains(
                "function Status_Pending(arg0) { return { tag: \"Pending\", 0: arg0 }; }"
            )
        );
        assert!(output.contains("const Status = { Active: Status_Active, Inactive: Status_Inactive, Pending: Status_Pending };"));
        assert!(
            output.contains("export { Status_Active, Status_Inactive, Status_Pending, Status };")
        );
    }

    #[test]
    fn test_emit_recursive_pattern_matching() {
        let input = r#"
            pub enum Option {
                None,
                Some(string)
            }
            
            pub enum Result {
                Ok(Option),
                Err(string)
            }
            
            fn test(x: Result) {
                match x {
                    Ok(Some(value)) => value,
                    Ok(None) => "empty",
                    Err(msg) => msg
                }
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should have nested pattern conditions with proper arity checks
        // Top-level Ok arity (exactly one field)
        assert!(output.contains(
            "_s.tag === \"Ok\" && typeof _s === 'object' && _s !== null && (Object.prototype.hasOwnProperty.call(_s, \"0\")) && !Object.prototype.hasOwnProperty.call(_s, \"1\")"
        ));
        // Nested Some arity (exactly one field)
        assert!(output.contains(
            "_s[0].tag === \"Some\" && typeof _s[0] === 'object' && _s[0] !== null && (Object.prototype.hasOwnProperty.call(_s[0], \"0\")) && !Object.prototype.hasOwnProperty.call(_s[0], \"1\")"
        ));
        // Nested None arity (unit)
        assert!(output.contains("_s[0].tag === \"None\""));
        // Err arity (exactly one field)
        assert!(output.contains(
            "_s.tag === \"Err\" && typeof _s === 'object' && _s !== null && (Object.prototype.hasOwnProperty.call(_s, \"0\")) && !Object.prototype.hasOwnProperty.call(_s, \"1\")"
        ));

        // Should bind nested values correctly
        assert!(output.contains("const value = _s[0][0];"));
    }

    #[test]
    fn test_emit_tuple_pattern_matching() {
        let input = r#"
            fn test(point: (number, string)) {
                match point {
                    (x, "center") => x,
                    (0, y) => y,
                    _ => "unknown"
                }
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Tuple patterns should check for objects, not arrays
        assert!(output.contains("typeof _s === 'object' && _s !== null && !Array.isArray(_s)"));
        // Exact arity: fields 0 and 1 exist, field 2 does not
        assert!(output.contains(
            "(Object.prototype.hasOwnProperty.call(_s, \"0\") && Object.prototype.hasOwnProperty.call(_s, \"1\")) && !Object.prototype.hasOwnProperty.call(_s, \"2\")"
        ));
        assert!(output.contains("_s[1] === \"center\""));
        assert!(output.contains("_s[0] === 0"));
    }

    #[test]
    fn test_import_path_rewriting() {
        assert_eq!(rewrite_import_path_for_js("./module.hk"), "./module.js");
        assert_eq!(rewrite_import_path_for_js("../utils.hk"), "../utils.js");
        assert_eq!(
            rewrite_import_path_for_js("./components/button"),
            "./components/button.js"
        );
        assert_eq!(rewrite_import_path_for_js("react"), "react"); // Package imports unchanged
    }

    #[test]
    fn test_switch_optimization_triggers_for_unit_variants() {
        // Switch should be used when all patterns are unit variants, no guards, no wildcards
        let input = r#"
            enum Status { Active, Inactive, Pending }
            fn get_code(status: Status) -> number {
                match status {
                    Active => 1,
                    Inactive => 2,
                    Pending => 3
                }
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should use switch statement
        assert!(output.contains("switch (_s.tag)"), "Expected switch statement but got: {}", output);
        assert!(output.contains("case \"Active\":"), "Expected case for Active variant");
        assert!(output.contains("case \"Inactive\":"), "Expected case for Inactive variant");
        assert!(output.contains("case \"Pending\":"), "Expected case for Pending variant");
        
        // Should not have if-else chains for variant matching
        assert!(!output.contains("if (_s.tag === \"Active\")"), "Should not have if-else for variants when switch is used");
    }

    #[test]
    fn test_switch_fallback_with_guards() {
        // Should fallback to if-else when guards are present
        let input = r#"
            enum Status { Active, Inactive }
            fn check_status(status: Status) -> string {
                match status {
                    Active => "running",
                    Inactive if true => "stopped"
                }
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should use if-else chain, not switch
        assert!(!output.contains("switch"), "Should not use switch when guards are present");
        assert!(output.contains("if (_s.tag === \"Active\")"), "Should use if-else chain for variants with guards");
        assert!(output.contains("if (_s.tag === \"Inactive\")"), "Should use if-else for guarded patterns");
        assert!(output.contains("if (true)"), "Should include guard condition");
    }

    #[test]
    fn test_switch_fallback_with_wildcards() {
        // Should fallback to if-else when wildcard is present
        let input = r#"
            enum Status { Active, Inactive, Pending }
            fn simplify_status(status: Status) -> string {
                match status {
                    Active => "active",
                    _ => "other"
                }
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should use if-else chain, not switch
        assert!(!output.contains("switch"), "Should not use switch when wildcard is present");
        assert!(output.contains("if (_s.tag === \"Active\")"), "Should use if-else for specific variant");
        assert!(output.contains("if (true)"), "Should use if (true) for wildcard pattern");
    }

    #[test]
    fn test_switch_fallback_with_payload_variants() {
        // Should fallback to if-else when variants have payloads
        let input = r#"
            enum Result { Ok(string), Err(string) }
            fn handle_result(result: Result) -> string {
                match result {
                    Ok(value) => value,
                    Err(error) => "failed: " + error
                }
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should use if-else chain, not switch
        assert!(!output.contains("switch"), "Should not use switch when variants have payloads");
        assert!(output.contains("if (_s.tag === \"Ok\""), "Should use if-else for payload variant");
        assert!(output.contains("if (_s.tag === \"Err\""), "Should use if-else for payload variant");
        assert!(output.contains("const value = _s[0]"), "Should extract payload");
        assert!(output.contains("const error = _s[0]"), "Should extract error payload");
    }

    #[test]
    fn test_switch_fallback_with_mixed_patterns() {
        // Should fallback to if-else when mixing variant and non-variant patterns
        let input = r#"
            fn handle_value(value: number) -> string {
                match value {
                    0 => "zero",
                    1 => "one",
                    _ => "other"
                }
            }
        "#;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should use if-else chain, not switch
        assert!(!output.contains("switch"), "Should not use switch for literal patterns");
        assert!(output.contains("if (_s === 0)"), "Should use if-else for literal matching");
        assert!(output.contains("if (_s === 1)"), "Should use if-else for literal matching");
        assert!(output.contains("if (true)"), "Should use if (true) for wildcard");
    }

    #[test]
    fn test_switch_with_qualified_variants() {
        // Switch should work with qualified variant patterns too
        let input = r##"
            enum Color { Red, Green, Blue }
            fn get_hex(color: Color) -> string {
                match color {
                    Color::Red => "#FF0000",
                    Color::Green => "#00FF00",
                    Color::Blue => "#0000FF"
                }
            }
        "##;

        let module = parse_husk_code(input);
        let output = emit(&module);

        // Should use switch statement even with qualified variants
        assert!(output.contains("switch (_s.tag)"), "Expected switch statement for qualified variants");
        assert!(output.contains("case \"Red\":"), "Expected case for Red variant");
        assert!(output.contains("case \"Green\":"), "Expected case for Green variant");
        assert!(output.contains("case \"Blue\":"), "Expected case for Blue variant");
    }
}
