// Core library prelude for Husk
// This defines the built-in types that are automatically available

// Simplified prelude that the current parser can handle (no generics yet)
pub const PRELUDE_SOURCE: &str = r#"
pub enum Option {
    Some(string),
    None
}

pub enum Result {
    Ok(string), 
    Err(string)
}
"#;

use syntax::ast::*;

pub fn get_prelude_module() -> Module {
    use syntax::lexer::Lexer;
    use syntax::parser::Parser;
    
    let mut lexer = Lexer::new(PRELUDE_SOURCE.to_string(), 0);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let (module, diagnostics) = parser.parse();
    
    if !diagnostics.is_empty() {
        eprintln!("Warning: Prelude parsing errors: {:?}", diagnostics);
    }
    
    module
}

pub fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "Option" | "Result" | "Vec" | "Map" | "string" | "number" | "bool"
    )
}

pub fn get_builtin_type_info(name: &str) -> Option<&'static str> {
    match name {
        "Option" => Some("Generic enum with Some(T) and None variants"),
        "Result" => Some("Generic enum with Ok(T) and Err(E) variants"),
        "Vec" => Some("Dynamic array type, maps to JS Array"),
        "Map" => Some("Key-value mapping, maps to JS Map"),
        "string" => Some("String primitive type"),
        "number" => Some("Number primitive type"),
        "bool" => Some("Boolean primitive type"),
        _ => None,
    }
}
