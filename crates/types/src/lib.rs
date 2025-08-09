use std::collections::HashMap;

/// Basic Type IR for Husk language
/// Tracks primitive types, tuple widths, and enum payload arities for better diagnostics
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Primitive types (string, number, bool)
    Primitive(PrimitiveType),
    
    /// Tuple with specific field types
    Tuple(Vec<Type>),
    
    /// Enum type with variant information
    Enum {
        name: String,
        variants: Vec<EnumVariant>,
    },
    
    /// Function type (for later extension)
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    
    /// Unknown/inferred type (for gradual typing)
    Unknown,
    
    /// Array type for sequences
    Array(Box<Type>),
    
    /// Object/record type with string keys and value type
    Record(Box<Type>, Box<Type>), // Record<K, V>
    
    /// Object type with specific named fields (for interfaces)
    Object(Vec<ObjectField>),
    
    /// Union type (A | B)
    Union(Vec<Type>),
    
    /// Intersection type (A & B) 
    Intersection(Vec<Type>),
    
    /// Type reference with optional generic parameters
    TypeRef {
        name: String,
        args: Vec<Type>,
    },
    
    /// External type from .d.ts or other module
    External {
        module: String,
        name: String,
        args: Vec<Type>,
    },
    
    /// Type variable (for generics - future work)
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectField {
    pub name: String,
    pub field_type: Type,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    String,
    Number, 
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub field_types: Vec<Type>,
}

/// Type checker with basic inference capabilities
#[derive(Debug)]
pub struct TypeChecker {
    /// Symbol table mapping variable names to types
    pub symbols: HashMap<String, Type>,
    
    /// Registered enum types
    pub enums: HashMap<String, Type>,
    
    /// Current scope depth (for future scoping)
    pub scope_depth: usize,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            enums: HashMap::new(),
            scope_depth: 0,
        }
    }
    
    /// Register an enum type in the type checker
    pub fn register_enum(&mut self, name: String, variants: Vec<EnumVariant>) {
        let enum_type = Type::Enum {
            name: name.clone(),
            variants,
        };
        self.enums.insert(name, enum_type);
    }
    
    /// Get type information for a symbol
    pub fn get_symbol_type(&self, name: &str) -> Option<&Type> {
        self.symbols.get(name)
    }
    
    /// Set type information for a symbol
    pub fn set_symbol_type(&mut self, name: String, typ: Type) {
        self.symbols.insert(name, typ);
    }
    
    /// Get enum type by name
    pub fn get_enum_type(&self, name: &str) -> Option<&Type> {
        self.enums.get(name)
    }
    
    /// Get variant information from an enum
    pub fn get_variant_info(&self, enum_name: &str, variant_name: &str) -> Option<&EnumVariant> {
        if let Some(Type::Enum { variants, .. }) = self.enums.get(enum_name) {
            variants.iter().find(|v| v.name == variant_name)
        } else {
            None
        }
    }
    
    /// Check if two types are compatible (basic compatibility checking)
    pub fn types_compatible(&self, t1: &Type, t2: &Type) -> bool {
        match (t1, t2) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Primitive(p1), Type::Primitive(p2)) => p1 == p2,
            (Type::Tuple(fields1), Type::Tuple(fields2)) => {
                fields1.len() == fields2.len() &&
                fields1.iter().zip(fields2.iter()).all(|(t1, t2)| self.types_compatible(t1, t2))
            }
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            (Type::Array(t1), Type::Array(t2)) => self.types_compatible(t1, t2),
            (Type::Record(k1, v1), Type::Record(k2, v2)) => {
                self.types_compatible(k1, k2) && self.types_compatible(v1, v2)
            },
            (Type::Object(fields1), Type::Object(fields2)) => {
                // Structural compatibility for object types
                // NOTE: This is strict equality-like matching for now.
                // TODO: Consider relaxing to structural subtyping (source has at least the target's fields).
                fields1.len() == fields2.len()
                    && fields1.iter().all(|f1| {
                        fields2.iter().any(|f2| {
                            f1.name == f2.name
                                && f1.optional == f2.optional
                                && self.types_compatible(&f1.field_type, &f2.field_type)
                        })
                    })
            },
            (Type::Union(types1), Type::Union(types2)) => {
                // Union compatibility - each type in types1 must be compatible with some type in types2
                types1.iter().all(|t1| {
                    types2.iter().any(|t2| self.types_compatible(t1, t2))
                })
            },
            (Type::Intersection(types1), Type::Intersection(types2)) => {
                // Both sides must satisfy all constituents
                types1.iter().all(|t1| types2.iter().any(|t2| self.types_compatible(t1, t2)))
                    && types2.iter().all(|t2| types1.iter().any(|t1| self.types_compatible(t1, t2)))
            }
            (Type::Intersection(types), other) => {
                // A & B compatible with X iff A compatible with X AND B compatible with X
                types.iter().all(|t| self.types_compatible(t, other))
            }
            (other, Type::Intersection(types)) => {
                // X compatible with A & B iff X compatible with A AND X compatible with B
                types.iter().all(|t| self.types_compatible(other, t))
            }
            (Type::TypeRef { name: n1, args: a1 }, Type::TypeRef { name: n2, args: a2 }) => {
                n1 == n2 && a1.len() == a2.len() && 
                a1.iter().zip(a2.iter()).all(|(t1, t2)| self.types_compatible(t1, t2))
            },
            (Type::External { module: m1, name: n1, args: a1 }, Type::External { module: m2, name: n2, args: a2 }) => {
                m1 == m2 && n1 == n2 && a1.len() == a2.len() &&
                a1.iter().zip(a2.iter()).all(|(t1, t2)| self.types_compatible(t1, t2))
            },
            _ => false,
        }
    }
    
    /// Infer type from literal values (basic inference)
    pub fn infer_literal_type(&self, literal: &str) -> Type {
        if literal.starts_with('"') && literal.ends_with('"') {
            Type::Primitive(PrimitiveType::String)
        } else if literal.parse::<f64>().is_ok() {
            Type::Primitive(PrimitiveType::Number)
        } else if literal == "true" || literal == "false" {
            Type::Primitive(PrimitiveType::Bool)
        } else {
            Type::Unknown
        }
    }
    
    /// Get the expected arity (number of fields) for an enum variant
    pub fn get_variant_arity(&self, enum_name: &str, variant_name: &str) -> Option<usize> {
        self.get_variant_info(enum_name, variant_name)
            .map(|variant| variant.field_types.len())
    }
    
    /// Create a tuple type from field types
    pub fn make_tuple_type(field_types: Vec<Type>) -> Type {
        Type::Tuple(field_types)
    }
    
    /// Create a function type
    pub fn make_function_type(params: Vec<Type>, return_type: Type) -> Type {
        Type::Function {
            params,
            return_type: Box::new(return_type),
        }
    }
    
    /// Create an array type
    pub fn make_array_type(element_type: Type) -> Type {
        Type::Array(Box::new(element_type))
    }
    
    /// Create a record type
    pub fn make_record_type(key_type: Type, value_type: Type) -> Type {
        Type::Record(Box::new(key_type), Box::new(value_type))
    }
    
    /// Create an object type from fields
    pub fn make_object_type(fields: Vec<(String, Type, bool)>) -> Type {
        let object_fields = fields.into_iter().map(|(name, field_type, optional)| {
            ObjectField { name, field_type, optional }
        }).collect();
        Type::Object(object_fields)
    }
    
    /// Create a union type
    pub fn make_union_type(types: Vec<Type>) -> Type {
        Type::Union(types)
    }
    
    /// Create a type reference
    pub fn make_type_ref(name: String, args: Vec<Type>) -> Type {
        Type::TypeRef { name, args }
    }
    
    /// Create an external type reference
    pub fn make_external_type(module: String, name: String, args: Vec<Type>) -> Type {
        Type::External { module, name, args }
    }
    
    /// Register types from an external module (future: for .d.ts interop)
    pub fn register_external_types(&mut self, _module_name: &str, _types: Vec<(String, Type)>) {
        // TODO: Implement when .d.ts parsing is ready
        // This would register types from TypeScript declaration files
        // enabling seamless interop with the TypeScript ecosystem
        todo!("Implement when .d.ts parsing is available")
    }
}

impl Type {
    /// Get a human-readable string representation of the type
    pub fn to_string(&self) -> String {
        match self {
            Type::Primitive(p) => match p {
                PrimitiveType::String => "string".to_string(),
                PrimitiveType::Number => "number".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
            },
            Type::Tuple(fields) => {
                let field_strings: Vec<String> = fields.iter().map(|f| f.to_string()).collect();
                format!("({})", field_strings.join(", "))
            },
            Type::Enum { name, .. } => name.clone(),
            Type::Function { params, return_type } => {
                let param_strings: Vec<String> = params.iter().map(|p| p.to_string()).collect();
                format!("({}) -> {}", param_strings.join(", "), return_type.to_string())
            },
            Type::Array(element_type) => {
                format!("{}[]", element_type.to_string())
            },
            Type::Record(key_type, value_type) => {
                format!("Record<{}, {}>", key_type.to_string(), value_type.to_string())
            },
            Type::Object(fields) => {
                let field_strings: Vec<String> = fields.iter().map(|f| {
                    let optional_marker = if f.optional { "?" } else { "" };
                    format!("{}{}: {}", f.name, optional_marker, f.field_type.to_string())
                }).collect();
                format!("{{ {} }}", field_strings.join(", "))
            },
            Type::Union(types) => {
                let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
                type_strings.join(" | ")
            },
            Type::Intersection(types) => {
                let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
                type_strings.join(" & ")
            },
            Type::TypeRef { name, args } => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let arg_strings: Vec<String> = args.iter().map(|t| t.to_string()).collect();
                    format!("{}<{}>", name, arg_strings.join(", "))
                }
            },
            Type::External { module, name, args } => {
                if args.is_empty() {
                    format!("{}::{}", module, name)
                } else {
                    let arg_strings: Vec<String> = args.iter().map(|t| t.to_string()).collect();
                    format!("{}::{}<{}>", module, name, arg_strings.join(", "))
                }
            },
            Type::Unknown => "?".to_string(),
            Type::Variable(name) => name.clone(),
        }
    }
    
    /// Check if this type is a primitive
    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Primitive(_))
    }
    
    /// Check if this type is a tuple
    pub fn is_tuple(&self) -> bool {
        matches!(self, Type::Tuple(_))
    }
    
    /// Check if this type is an enum
    pub fn is_enum(&self) -> bool {
        matches!(self, Type::Enum { .. })
    }
    
    /// Get tuple field count if this is a tuple
    pub fn tuple_field_count(&self) -> Option<usize> {
        if let Type::Tuple(fields) = self {
            Some(fields.len())
        } else {
            None
        }
    }
    
    /// Check if this type is an array
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }
    
    /// Check if this type is a union
    pub fn is_union(&self) -> bool {
        matches!(self, Type::Union(_))
    }
    
    /// Check if this type is an external type
    pub fn is_external(&self) -> bool {
        matches!(self, Type::External { .. })
    }
    
    /// Get the element type if this is an array
    pub fn array_element_type(&self) -> Option<&Type> {
        if let Type::Array(element) = self {
            Some(element)
        } else {
            None
        }
    }
    
    /// Get union types if this is a union
    pub fn union_types(&self) -> Option<&Vec<Type>> {
        if let Type::Union(types) = self {
            Some(types)
        } else {
            None
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_types() {
        let checker = TypeChecker::new();
        
        // Test literal inference
        assert_eq!(
            checker.infer_literal_type("\"hello\""),
            Type::Primitive(PrimitiveType::String)
        );
        assert_eq!(
            checker.infer_literal_type("42"),
            Type::Primitive(PrimitiveType::Number)
        );
        assert_eq!(
            checker.infer_literal_type("true"),
            Type::Primitive(PrimitiveType::Bool)
        );
    }

    #[test]
    fn test_enum_registration() {
        let mut checker = TypeChecker::new();
        
        // Register Option enum
        let variants = vec![
            EnumVariant {
                name: "Some".to_string(),
                field_types: vec![Type::Primitive(PrimitiveType::String)],
            },
            EnumVariant {
                name: "None".to_string(),
                field_types: vec![],
            },
        ];
        
        checker.register_enum("Option".to_string(), variants);
        
        // Check variant arity
        assert_eq!(checker.get_variant_arity("Option", "Some"), Some(1));
        assert_eq!(checker.get_variant_arity("Option", "None"), Some(0));
    }

    #[test]
    fn test_type_compatibility() {
        let checker = TypeChecker::new();
        
        let string_type = Type::Primitive(PrimitiveType::String);
        let number_type = Type::Primitive(PrimitiveType::Number);
        let unknown_type = Type::Unknown;
        
        // Same types are compatible
        assert!(checker.types_compatible(&string_type, &string_type));
        
        // Different primitives are not compatible
        assert!(!checker.types_compatible(&string_type, &number_type));
        
        // Unknown is compatible with anything
        assert!(checker.types_compatible(&unknown_type, &string_type));
        assert!(checker.types_compatible(&string_type, &unknown_type));
    }

    #[test]
    fn test_tuple_types() {
        let checker = TypeChecker::new();
        
        let tuple1 = TypeChecker::make_tuple_type(vec![
            Type::Primitive(PrimitiveType::String),
            Type::Primitive(PrimitiveType::Number),
        ]);
        
        let tuple2 = TypeChecker::make_tuple_type(vec![
            Type::Primitive(PrimitiveType::String),
            Type::Primitive(PrimitiveType::Number),
        ]);
        
        let tuple3 = TypeChecker::make_tuple_type(vec![
            Type::Primitive(PrimitiveType::String),
        ]);
        
        // Same tuples are compatible
        assert!(checker.types_compatible(&tuple1, &tuple2));
        
        // Different arity tuples are not compatible
        assert!(!checker.types_compatible(&tuple1, &tuple3));
        
        // Check tuple properties
        assert!(tuple1.is_tuple());
        assert_eq!(tuple1.tuple_field_count(), Some(2));
    }

    #[test]
    fn test_array_types() {
        let string_array = TypeChecker::make_array_type(Type::Primitive(PrimitiveType::String));
        
        assert!(string_array.is_array());
        assert_eq!(string_array.array_element_type(), Some(&Type::Primitive(PrimitiveType::String)));
        assert_eq!(string_array.to_string(), "string[]");
    }

    #[test]
    fn test_object_types() {
        let user_type = TypeChecker::make_object_type(vec![
            ("id".to_string(), Type::Primitive(PrimitiveType::Number), false),
            ("name".to_string(), Type::Primitive(PrimitiveType::String), false),
            ("email".to_string(), Type::Primitive(PrimitiveType::String), true),
        ]);
        
        assert_eq!(user_type.to_string(), "{ id: number, name: string, email?: string }");
    }

    #[test]
    fn test_union_types() {
        let string_or_number = TypeChecker::make_union_type(vec![
            Type::Primitive(PrimitiveType::String),
            Type::Primitive(PrimitiveType::Number),
        ]);
        
        assert!(string_or_number.is_union());
        assert_eq!(string_or_number.to_string(), "string | number");
    }

    #[test]
    fn test_external_types() {
        let react_component = TypeChecker::make_external_type(
            "react".to_string(),
            "Component".to_string(),
            vec![
                TypeChecker::make_object_type(vec![
                    ("children".to_string(), Type::Unknown, true)
                ])
            ]
        );
        
        assert!(react_component.is_external());
        assert_eq!(react_component.to_string(), "react::Component<{ children?: ? }>");
    }

    #[test]
    fn test_type_compatibility_extended() {
        let checker = TypeChecker::new();
        
        // Array compatibility
        let string_array1 = TypeChecker::make_array_type(Type::Primitive(PrimitiveType::String));
        let string_array2 = TypeChecker::make_array_type(Type::Primitive(PrimitiveType::String));
        let number_array = TypeChecker::make_array_type(Type::Primitive(PrimitiveType::Number));
        
        assert!(checker.types_compatible(&string_array1, &string_array2));
        assert!(!checker.types_compatible(&string_array1, &number_array));
        
        // Union compatibility
        let union1 = TypeChecker::make_union_type(vec![
            Type::Primitive(PrimitiveType::String),
            Type::Primitive(PrimitiveType::Number),
        ]);
        let union2 = TypeChecker::make_union_type(vec![
            Type::Primitive(PrimitiveType::String),
            Type::Primitive(PrimitiveType::Number),
        ]);
        
        assert!(checker.types_compatible(&union1, &union2));
    }
}
