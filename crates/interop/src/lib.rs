pub mod path_rewriting;

// Future: .d.ts parsing and interop functionality
// This module will contain the foundation for parsing TypeScript declaration files
// and converting them to Husk Type IR for seamless module composition.

use std::collections::HashMap;

/// Represents an external TypeScript module's type information
/// This is the future structure for holding parsed .d.ts data
#[derive(Debug, Clone)]
pub struct ExternalModule {
    pub name: String,
    pub interfaces: HashMap<String, InterfaceInfo>,
    pub type_aliases: HashMap<String, TypeAliasInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub enums: HashMap<String, EnumInfo>,
    pub exports: Vec<ExportInfo>,
}

#[derive(Debug, Clone)]
pub struct InterfaceInfo {
    pub name: String,
    pub fields: Vec<FieldInfo>,
}

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub type_annotation: String, // Will be parsed into proper Type IR
    pub optional: bool,
}

#[derive(Debug, Clone)]
pub struct TypeAliasInfo {
    pub name: String,
    pub target: String, // Will be parsed into proper Type IR
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub parameters: Vec<ParameterInfo>,
    pub return_type: String, // Will be parsed into proper Type IR
}

#[derive(Debug, Clone)]
pub struct ParameterInfo {
    pub name: String,
    pub type_annotation: String,
    pub optional: bool,
}

#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: String,
    pub variants: Vec<EnumVariantInfo>,
}

#[derive(Debug, Clone)]
pub struct EnumVariantInfo {
    pub name: String,
    pub value: Option<String>, // For string/number enum values
}

#[derive(Debug, Clone)]
pub enum ExportInfo {
    Named(String),
    Default(String),
    All,
}

impl ExternalModule {
    /// Create a new empty external module
    pub fn new(name: String) -> Self {
        Self {
            name,
            interfaces: HashMap::new(),
            type_aliases: HashMap::new(),
            functions: HashMap::new(),
            enums: HashMap::new(),
            exports: Vec::new(),
        }
    }
    
    /// Future: Parse a .d.ts file and populate this structure
    pub fn from_dts_file(_path: &str) -> Result<Self, Box<dyn std::error::Error>> {
        // TODO: Implement actual .d.ts parsing using swc_ecma_parser or similar
        // This would:
        // 1. Parse the TypeScript declaration file
        // 2. Traverse the AST to extract type information
        // 3. Convert TypeScript types to our intermediate representation
        // 4. Handle complex cases like generics, unions, intersections
        
        todo!("Implement .d.ts parsing - see INTEROP_RESEARCH.md for details")
    }
    
    /// Check if this module exports a specific symbol
    pub fn exports_symbol(&self, symbol: &str) -> bool {
        self.exports.iter().any(|export| match export {
            ExportInfo::Named(name) => name == symbol,
            ExportInfo::Default(name) => name == symbol,
            ExportInfo::All => true,
        })
    }
}
