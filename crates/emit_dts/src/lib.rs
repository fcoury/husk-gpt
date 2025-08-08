use syntax::ast::*;

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
    let mut variants = Vec::new();
    
    for variant in &enm.variants {
        if variant.fields.is_empty() {
            variants.push(format!("{{ tag: \"{}\" }}", variant.name));
        } else {
            let fields = variant.fields
                .iter()
                .enumerate()
                .map(|(i, ty)| format!("{}: {}", i, emit_type(ty)))
                .collect::<Vec<_>>()
                .join("; ");
            variants.push(format!("{{ tag: \"{}\"; {} }}", variant.name, fields));
        }
    }
    
    format!("export type {} = {};", enm.name, variants.join(" | "))
}

fn emit_struct(strct: &Struct) -> String {
    let fields = strct.fields
        .iter()
        .map(|field| {
            let readonly = ""; // TODO: Handle readonly fields
            format!("  {}{}: {};", readonly, field.name, emit_type(&field.ty))
        })
        .collect::<Vec<_>>()
        .join("\n");
    
    format!("export interface {} {{\n{}\n}}", strct.name, fields)
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
