use std::collections::HashMap;
use syntax::ast::*;
use syntax::ast::{ImportClause, Export};
use diagnostics::{Diagnostic, ErrorCode, Span};

pub struct Resolver {
    scopes: Vec<Scope>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
struct Scope {
    symbols: HashMap<String, Symbol>,
}

#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    kind: SymbolKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum SymbolKind {
    Function { params: Vec<Type>, return_type: Option<Type> },
    Variable { ty: Option<Type>, mutable: bool },
    Parameter { ty: Type },
    EnumVariant { enum_name: String, fields: Vec<Type> },
    Type { kind: TypeKind },
    Import { path: String },
}

#[derive(Debug, Clone)]
enum TypeKind {
    Struct(Vec<Field>),
    Enum(Vec<Variant>),
    Alias(Type),
    Primitive,
}

impl Resolver {
    pub fn new() -> Self {
        let mut resolver = Self {
            scopes: vec![Scope::new()], // Global scope
            diagnostics: Vec::new(),
        };
        
        // Seed global scope with built-in symbols
        resolver.add_built_ins();
        resolver
    }

    fn add_built_ins(&mut self) {
        // Console object for JavaScript compatibility
        let console_symbol = Symbol {
            name: "console".to_string(),
            kind: SymbolKind::Variable { ty: None, mutable: false },
            span: Span::new(0, 0, 0),
        };
        self.declare_symbol(console_symbol);

        // Built-in panic function  
        let panic_symbol = Symbol {
            name: "panic".to_string(),
            kind: SymbolKind::Function { params: vec![], return_type: None },
            span: Span::new(0, 0, 0),
        };
        self.declare_symbol(panic_symbol);

        // Common global types
        let number_symbol = Symbol {
            name: "number".to_string(),
            kind: SymbolKind::Type { kind: TypeKind::Primitive },
            span: Span::new(0, 0, 0),
        };
        self.declare_symbol(number_symbol);

        let string_symbol = Symbol {
            name: "string".to_string(),
            kind: SymbolKind::Type { kind: TypeKind::Primitive },
            span: Span::new(0, 0, 0),
        };
        self.declare_symbol(string_symbol);

        let bool_symbol = Symbol {
            name: "bool".to_string(),
            kind: SymbolKind::Type { kind: TypeKind::Primitive },
            span: Span::new(0, 0, 0),
        };
        self.declare_symbol(bool_symbol);
    }

    pub fn resolve(&mut self, module: &Module) -> Vec<Diagnostic> {
        self.resolve_module(module);
        std::mem::take(&mut self.diagnostics)
    }

    fn resolve_module(&mut self, module: &Module) {
        // First pass: collect all top-level declarations
        for item in &module.items {
            self.collect_item_declaration(&item.value, item.span);
        }

        // Second pass: resolve expressions and function bodies
        for item in &module.items {
            self.resolve_item(&item.value, item.span);
        }
    }

    fn collect_item_declaration(&mut self, item: &Item, span: Span) {
        match item {
            Item::Function(func) => {
                let symbol = Symbol {
                    name: func.name.clone(),
                    kind: SymbolKind::Function {
                        params: func.params.iter().map(|p| p.ty.clone()).collect(),
                        return_type: func.return_type.clone(),
                    },
                    span,
                };
                self.declare_symbol(symbol);
            }
            Item::Struct(strct) => {
                let symbol = Symbol {
                    name: strct.name.clone(),
                    kind: SymbolKind::Type {
                        kind: TypeKind::Struct(strct.fields.clone()),
                    },
                    span,
                };
                self.declare_symbol(symbol);
            }
            Item::Enum(enm) => {
                // Declare the enum type
                let enum_symbol = Symbol {
                    name: enm.name.clone(),
                    kind: SymbolKind::Type {
                        kind: TypeKind::Enum(enm.variants.clone()),
                    },
                    span,
                };
                self.declare_symbol(enum_symbol);

                // Declare each variant as a constructor function
                for variant in &enm.variants {
                    let variant_symbol = Symbol {
                        name: format!("{}_{}", enm.name, variant.name),
                        kind: SymbolKind::EnumVariant {
                            enum_name: enm.name.clone(),
                            fields: variant.fields.clone(),
                        },
                        span,
                    };
                    self.declare_symbol(variant_symbol);
                }
            }
            Item::Import(import) => {
                match &import.items {
                    ImportClause::Named(items) => {
                        for item in items {
                            let name = item.alias.as_ref().unwrap_or(&item.name).clone();
                            let symbol = Symbol {
                                name,
                                kind: SymbolKind::Import { path: import.path.clone() },
                                span,
                            };
                            self.declare_symbol(symbol);
                        }
                    }
                    ImportClause::Namespace(name) => {
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Import { path: import.path.clone() },
                            span,
                        };
                        self.declare_symbol(symbol);
                    }
                    ImportClause::Default(name) => {
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Import { path: import.path.clone() },
                            span,
                        };
                        self.declare_symbol(symbol);
                    }
                    ImportClause::Mixed { default, named } => {
                        // Declare default import
                        let default_symbol = Symbol {
                            name: default.clone(),
                            kind: SymbolKind::Import { path: import.path.clone() },
                            span,
                        };
                        self.declare_symbol(default_symbol);
                        
                        // Declare named imports
                        for item in named {
                            let name = item.alias.as_ref().unwrap_or(&item.name).clone();
                            let symbol = Symbol {
                                name,
                                kind: SymbolKind::Import { path: import.path.clone() },
                                span,
                            };
                            self.declare_symbol(symbol);
                        }
                    }
                }
            }
            Item::Export(export) => {
                match export {
                    Export::Item(item) | Export::Default(item) => {
                        // Export declares the item as well
                        self.collect_item_declaration(&item.value, item.span);
                    }
                    Export::Named(_) | Export::NamedFrom { .. } | Export::All(_) => {
                        // These don't declare new symbols, they re-export existing ones
                    }
                }
            }
            _ => {
                // TODO: Handle other item types
            }
        }
    }

    fn resolve_item(&mut self, item: &Item, _span: Span) {
        match item {
            Item::Function(func) => {
                if let Some(ref body) = func.body {
                    self.push_scope();
                    
                    // Add parameters to scope
                    for param in &func.params {
                        let symbol = Symbol {
                            name: param.name.clone(),
                            kind: SymbolKind::Parameter { ty: param.ty.clone() },
                            span: _span, // Would need span from parameter in real implementation
                        };
                        self.declare_symbol(symbol);
                    }

                    self.resolve_block(body);
                    self.pop_scope();
                }
            }
            Item::Export(export) => {
                match export {
                    Export::Item(item) | Export::Default(item) => {
                        self.resolve_item(&item.value, item.span);
                    }
                    Export::Named(_) | Export::NamedFrom { .. } | Export::All(_) => {
                        // These don't have bodies to resolve
                    }
                }
            }
            _ => {
                // Other items don't have bodies to resolve
            }
        }
    }

    fn resolve_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.resolve_statement(&stmt.value, stmt.span);
        }
    }

    fn resolve_statement(&mut self, stmt: &Statement, _span: Span) {
        match stmt {
            Statement::Let(let_stmt) => {
                // First resolve the value expression
                self.resolve_expression(&let_stmt.value, _span);

                // Then declare the variable in current scope
                let symbol = Symbol {
                    name: let_stmt.name.clone(),
                    kind: SymbolKind::Variable {
                        ty: let_stmt.ty.clone(),
                        mutable: let_stmt.mutable,
                    },
                    span: _span,
                };
                self.declare_symbol(symbol);
            }
            Statement::Expression(expr) => {
                self.resolve_expression(expr, _span);
            }
        }
    }

    fn resolve_expression(&mut self, expr: &Expression, span: Span) {
        // Use the provided span as the default, but try to be more specific for identifiers
        match expr {
            Expression::Identifier(name) => {
                if !self.resolve_identifier(name) {
                    self.error_at(span, format!("Undefined identifier: {}", name));
                }
            }
            Expression::Call(call) => {
                self.resolve_expression(&call.callee, span);
                for arg in &call.args {
                    self.resolve_expression(arg, span);
                }
            }
            Expression::Member(member) => {
                self.resolve_expression(&member.object, span);
                // Property access doesn't need resolution
            }
            Expression::Match(match_expr) => {
                self.resolve_expression(&match_expr.scrutinee, span);
                for arm in &match_expr.arms {
                    self.push_scope();
                    self.resolve_pattern(&arm.pattern, span);
                    if let Some(ref guard) = arm.guard {
                        self.resolve_expression(guard, span);
                    }
                    self.resolve_expression(&arm.body, span);
                    self.pop_scope();
                }
            }
            Expression::Block(block) => {
                self.push_scope();
                self.resolve_block(block);
                self.pop_scope();
            }
            Expression::VariantCtor { enum_name, variant, args } => {
                // Verify variant exists and has correct arity
                let ctor_name = format!("{}_{}", enum_name, variant);
                if let Some(symbol) = self.lookup_symbol(&ctor_name) {
                    match &symbol.kind {
                        SymbolKind::EnumVariant { fields, .. } => {
                            let expected_arity = fields.len();
                            
                            if args.len() != expected_arity {
                                self.error_at(span, format!(
                                    "Variant constructor '{}::{}' expects {} arguments, got {}",
                                    enum_name, variant, expected_arity, args.len()
                                ));
                            }
                        }
                        _ => {
                            self.error_at(span, format!(
                                "'{}' is not a variant constructor",
                                ctor_name
                            ));
                        }
                    }
                } else {
                    self.error_at(span, format!(
                        "Unknown variant constructor '{}::{}'",
                        enum_name, variant
                    ));
                }
                
                // Resolve argument expressions with better span context
                for arg in args {
                    self.resolve_expression(arg, span);
                }
            }
            Expression::Binary(binary) => {
                self.resolve_expression(&binary.left, span);
                self.resolve_expression(&binary.right, span);
            }
            Expression::ObjectLiteral(fields) => {
                for field in fields {
                    self.resolve_expression(&field.value, span);
                }
            }
            Expression::Literal(_) => {
                // Literals don't need resolution
            }
        }
    }

    fn resolve_pattern(&mut self, pattern: &Pattern, span: Span) {
        match pattern {
            Pattern::Identifier(name) => {
                // Pattern identifiers bind new variables
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Variable { ty: None, mutable: false },
                    span,
                };
                self.declare_symbol(symbol);
            }
            Pattern::Variant(variant_name, patterns) => {
                // Look for the variant constructor symbol
                if let Some(symbol) = self.lookup_symbol(variant_name) {
                    match &symbol.kind {
                        SymbolKind::EnumVariant { fields, .. } => {
                            let expected_arity = fields.len();
                            
                            if patterns.len() != expected_arity {
                                self.error_at(span, format!(
                                    "Pattern for variant '{}' expects {} arguments, got {}",
                                    variant_name, expected_arity, patterns.len()
                                ));
                            }
                        }
                        _ => {
                            self.error_at(span, format!(
                                "'{}' is not a variant constructor",
                                variant_name
                            ));
                        }
                    }
                } else {
                    self.error_at(span, format!(
                        "Unknown variant '{}'",
                        variant_name
                    ));
                }
                
                // Resolve nested patterns
                for pattern in patterns {
                    self.resolve_pattern(pattern, span);
                }
            }
            Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.resolve_pattern(pattern, span);
                }
            }
            Pattern::Literal(_) | Pattern::Wildcard => {
                // These don't bind variables
            }
        }
    }

    fn resolve_identifier(&self, name: &str) -> bool {
        self.lookup_symbol(name).is_some()
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn declare_symbol(&mut self, symbol: Symbol) {
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.symbols.contains_key(&symbol.name) {
                self.error(format!("Identifier '{}' is already declared", symbol.name));
            } else {
                current_scope.symbols.insert(symbol.name.clone(), symbol);
            }
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn error(&mut self, message: String) {
        // Create a dummy diagnostic since we don't have proper spans yet
        self.diagnostics.push(Diagnostic::error(
            ErrorCode::NameResolutionError,
            Span::new(0, 0, 0),
            &message,
        ));
    }

    fn error_at(&mut self, span: Span, message: String) {
        self.diagnostics.push(Diagnostic::error(
            ErrorCode::NameResolutionError,
            span,
            &message,
        ));
    }
}

impl Scope {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}