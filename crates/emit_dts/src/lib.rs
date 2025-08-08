use syntax::ast::*;
use syntax::ast::{ImportClause, Export, ExportItem};

pub fn emit(module: &Module) -> String {
    let mut output = String::new();
    
    for item in &module.items {
        match &item.value {
            Item::Function(func) if matches!(func.vis, Visibility::Public) => {
                output.push_str(&emit_function(func));
                output.push('\n');
            }
            Item::Enum(enm) if matches!(enm.vis, Visibility::Public) => {
                output.push_str(&emit_enum(enm));
                output.push('\n');
            }
            Item::Struct(strct) if matches!(strct.vis, Visibility::Public) => {
                output.push_str(&emit_struct(strct));
                output.push('\n');
            }
            Item::TypeAlias(alias) if matches!(alias.vis, Visibility::Public) => {
                output.push_str(&emit_type_alias(alias));
                output.push('\n');
            }
            Item::Interface(interface) if matches!(interface.vis, Visibility::Public) => {
                output.push_str(&emit_interface(interface));
                output.push('\n');
            }
            Item::Import(import) => {
                output.push_str(&emit_import_declaration(import));
                output.push('\n');
            }
            Item::Export(export) => {
                output.push_str(&emit_export_declaration(export));
                output.push('\n');
            }
            _ => {
                // Skip private items and unimplemented items
            }
        }
    }
    
    output
}

fn emit_function(func: &Function) -> String {
    let params = func.params
        .iter()
        .map(|p| format!("{}: {}", p.name, emit_type(&p.ty)))
        .collect::<Vec<_>>()
        .join(", ");
    
    let return_type = func.return_type
        .as_ref()
        .map(emit_type)
        .unwrap_or_else(|| "void".to_string());
    
    format!("export declare function {}({}): {};", func.name, params, return_type)
}

fn emit_enum(enm: &Enum) -> String {
    let mut output = String::new();
    let mut variants = Vec::new();
    
    // Generate constructor function declarations and type variants
    for variant in &enm.variants {
        if variant.fields.is_empty() {
            // Unit variant
            variants.push(format!("{{ tag: \"{}\" }}", variant.name));
            output.push_str(&format!(
                "export declare const {}: {{ tag: \"{}\" }};\n",
                format!("{}_{}", enm.name, variant.name),
                variant.name
            ));
        } else {
            // Tuple variant
            let fields = variant.fields
                .iter()
                .enumerate()
                .map(|(i, ty)| format!("{}: {}", i, emit_type(ty)))
                .collect::<Vec<_>>()
                .join("; ");
            variants.push(format!("{{ tag: \"{}\"; {} }}", variant.name, fields));
            
            let params = variant.fields
                .iter()
                .enumerate()
                .map(|(i, ty)| format!("arg{}: {}", i, emit_type(ty)))
                .collect::<Vec<_>>()
                .join(", ");
            output.push_str(&format!(
                "export declare function {}({}): {{ tag: \"{}\"; {} }};\n",
                format!("{}_{}", enm.name, variant.name),
                params,
                variant.name,
                fields
            ));
        }
    }
    
    // Add enum type and namespace
    output.push_str(&format!("export type {} = {};\n", enm.name, variants.join(" | ")));
    output.push_str(&format!(
        "export declare const {}: {{ {} }};\n",
        enm.name,
        enm.variants
            .iter()
            .map(|v| format!("{}: typeof {}", v.name, format!("{}_{}", enm.name, v.name)))
            .collect::<Vec<_>>()
            .join(", ")
    ));
    
    output
}

fn emit_struct(strct: &Struct) -> String {
    let fields = strct.fields
        .iter()
        .filter(|f| matches!(f.vis, Visibility::Public))
        .map(|field| {
            let readonly = ""; // TODO: Handle readonly fields
            format!("  {}{}: {};", readonly, field.name, emit_type(&field.ty))
        })
        .collect::<Vec<_>>()
        .join("\n");
    
    // Generate interface for the struct type
    let interface = format!("export interface {} {{\n{}\n}}", strct.name, fields);
    
    // Generate constructor function declaration
    let constructor_params = strct.fields
        .iter()
        .filter(|f| matches!(f.vis, Visibility::Public))
        .map(|f| format!("{}: {}", f.name, emit_type(&f.ty)))
        .collect::<Vec<_>>()
        .join(", ");
    
    let constructor = format!(
        "export declare function {}(fields: {{ {} }}): {};",
        strct.name, constructor_params, strct.name
    );
    
    format!("{}\n{}", interface, constructor)
}

fn emit_type_alias(alias: &TypeAlias) -> String {
    format!("export type {} = {};", alias.name, emit_type(&alias.ty))
}

fn emit_interface(interface: &Interface) -> String {
    let methods = interface.methods
        .iter()
        .map(|method| {
            let params = method.params
                .iter()
                .map(|p| format!("{}: {}", p.name, emit_type(&p.ty)))
                .collect::<Vec<_>>()
                .join(", ");
            
            let return_type = method.return_type
                .as_ref()
                .map(emit_type)
                .unwrap_or_else(|| "void".to_string());
            
            format!("  {}({}): {};", method.name, params, return_type)
        })
        .collect::<Vec<_>>()
        .join("\n");
    
    format!("export interface {} {{\n{}\n}}", interface.name, methods)
}

fn emit_type(ty: &Type) -> String {
    match ty {
        Type::Primitive(prim) => emit_primitive_type(prim),
        Type::Path(name) => name.clone(),
        Type::Generic(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let args_str = args
                    .iter()
                    .map(emit_type)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", name, args_str)
            }
        }
        Type::Union(types) => {
            types
                .iter()
                .map(emit_type)
                .collect::<Vec<_>>()
                .join(" | ")
        }
        Type::Intersection(types) => {
            types
                .iter()
                .map(emit_type)
                .collect::<Vec<_>>()
                .join(" & ")
        }
        Type::Array(element_type) => {
            format!("{}[]", emit_type(element_type))
        }
        Type::Tuple(types) => {
            let types_str = types
                .iter()
                .map(emit_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", types_str)
        }
        Type::Option(inner) => {
            format!("{} | undefined", emit_type(inner))
        }
        Type::Function(params, return_type) => {
            let params_str = params
                .iter()
                .enumerate()
                .map(|(i, ty)| format!("arg{}: {}", i, emit_type(ty)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({}) => {}", params_str, emit_type(return_type))
        }
    }
}

fn emit_primitive_type(prim: &PrimitiveType) -> String {
    match prim {
        PrimitiveType::String => "string".to_string(),
        PrimitiveType::Number => "number".to_string(),
        PrimitiveType::Bool => "boolean".to_string(),
        PrimitiveType::Void => "void".to_string(),
    }
}

fn emit_import_declaration(import: &Import) -> String {
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

fn emit_export_declaration(export: &Export) -> String {
    match export {
        Export::Item(item) => {
            match &item.value {
        Item::Function(func) => {
            let params = func.params
                .iter()
                .map(|p| format!("{}: {}", p.name, emit_type(&p.ty)))
                .collect::<Vec<_>>()
                .join(", ");
            
            let return_type = func.return_type
                .as_ref()
                .map(emit_type)
                .unwrap_or_else(|| "void".to_string());
            
            format!("export declare function {}({}): {};", func.name, params, return_type)
        }
        Item::Enum(enm) => {
            let enum_output = emit_enum(enm);
            // Add export prefix to each line
            enum_output
                .lines()
                .map(|line| {
                    if line.trim().is_empty() {
                        line.to_string()
                    } else if line.starts_with("export") {
                        line.to_string()
                    } else {
                        format!("export {}", line.trim_start())
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
        }
        Item::Struct(strct) => {
            let struct_output = emit_struct(strct);
            // Add export prefix to each line
            struct_output
                .lines()
                .map(|line| {
                    if line.trim().is_empty() {
                        line.to_string()
                    } else if line.starts_with("export") {
                        line.to_string()
                    } else {
                        format!("export {}", line.trim_start())
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
        }
                _ => {
                    format!("// TODO: Export declaration for {:?}", item.value)
                }
            }
        }
        Export::Default(item) => {
            match &item.value {
                Item::Function(func) => {
                    let params = func.params
                        .iter()
                        .map(|p| format!("{}: {}", p.name, emit_type(&p.ty)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    
                    let return_type = func.return_type
                        .as_ref()
                        .map(emit_type)
                        .unwrap_or_else(|| "void".to_string());
                    
                    format!("export default function {}({}): {};", func.name, params, return_type)
                }
                _ => {
                    format!("// TODO: Default export declaration for {:?}", item.value)
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
