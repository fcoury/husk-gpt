// Core library prelude for Husk
// This defines the built-in types that are automatically available

pub const PRELUDE_SOURCE: &str = r#"
// Option type - represents a value that might be present or absent
pub enum Option<T> {
    Some(T),
    None
}

impl<T> Option<T> {
    pub fn is_some(self) -> bool {
        match self {
            Option::Some(_) => true,
            Option::None => false,
        }
    }
    
    pub fn is_none(self) -> bool {
        match self {
            Option::Some(_) => false,
            Option::None => true,
        }
    }
    
    pub fn unwrap(self) -> T {
        match self {
            Option::Some(value) => value,
            Option::None => panic("called Option::unwrap on None"),
        }
    }
    
    pub fn map<U>(self, f: fn(T) -> U) -> Option<U> {
        match self {
            Option::Some(value) => Option::Some(f(value)),
            Option::None => Option::None,
        }
    }
}

// Result type - represents either success (Ok) or failure (Err)
pub enum Result<T, E> {
    Ok(T),
    Err(E)
}

impl<T, E> Result<T, E> {
    pub fn is_ok(self) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }
    
    pub fn is_err(self) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }
    
    pub fn unwrap(self) -> T {
        match self {
            Result::Ok(value) => value,
            Result::Err(_) => panic("called Result::unwrap on Err"),
        }
    }
    
    pub fn map<U>(self, f: fn(T) -> U) -> Result<U, E> {
        match self {
            Result::Ok(value) => Result::Ok(f(value)),
            Result::Err(err) => Result::Err(err),
        }
    }
}

// Vec type - dynamic array (maps to JS Array at runtime)
pub type Vec<T> = Array<T>;

// Map type - key-value mapping (maps to JS Map at runtime)  
pub type Map<K, V> = JSMap<K, V>;

// String operations
impl string {
    pub fn length(self) -> number {
        // Built-in, handled by emitter
    }
    
    pub fn push(self, other: string) -> string {
        // Built-in, handled by emitter  
    }
}

// Built-in panic function
pub fn panic(message: string) -> ! {
    // Built-in, handled by emitter
}

// Console functions for debugging
pub mod console {
    pub fn log(message: string) {
        // Built-in, handled by emitter
    }
}
"#;

use syntax::ast::*;

pub fn get_prelude_module() -> Module {
    // For now, return an empty module
    // In a full implementation, we would parse PRELUDE_SOURCE
    Module {
        items: Vec::new(),
    }
}

pub fn is_builtin_type(name: &str) -> bool {
    matches!(name, "Option" | "Result" | "Vec" | "Map" | "string" | "number" | "bool")
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
