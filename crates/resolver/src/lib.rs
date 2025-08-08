use std::collections::HashMap;
use syntax::ast::*;
use syntax::ast::{ImportClause, Export};
use diagnostics::{Diagnostic, ErrorCode, Span};

/// Represents a constructor pattern for exhaustiveness checking
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstructorPattern {
    /// Unit variant like Inactive  
    Variant { name: String, arity: usize },
    /// Literal pattern like 5, "hello"
    Literal(String),
    /// Wildcard pattern _
    Wildcard,
}

/// Analysis result for match exhaustiveness and reachability
#[derive(Debug)]
struct MatchAnalysis {
    /// Missing patterns that would make the match exhaustive
    missing_patterns: Vec<ConstructorPattern>,
    /// Patterns that can never be reached
    unreachable_patterns: Vec<usize>, // indices into the arms
}

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
                    
                    // Also declare bare variant name for pattern matching
                    // Check for collision first
                    if self.lookup_symbol(&variant.name).is_some() {
                        self.error_at(span, format!(
                            "Variant name '{}' conflicts with existing symbol. Consider using qualified patterns.",
                            variant.name
                        ));
                    } else {
                        let bare_variant_symbol = Symbol {
                            name: variant.name.clone(),
                            kind: SymbolKind::EnumVariant {
                                enum_name: enm.name.clone(),
                                fields: variant.fields.clone(),
                            },
                            span,
                        };
                        self.declare_symbol(bare_variant_symbol);
                    }
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
                
                // Analyze match for exhaustiveness and reachability
                self.analyze_match(match_expr, span);
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
    
    /// Check if a name refers to a unit variant (EnumVariant with 0 fields)
    pub fn is_unit_variant(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                match &symbol.kind {
                    SymbolKind::EnumVariant { fields, .. } => {
                        return fields.is_empty();
                    }
                    _ => return false,
                }
            }
        }
        false
    }
    
    /// Normalize patterns in a module - convert identifier patterns that resolve to unit variants
    /// into variant patterns
    pub fn normalize_patterns(&self, module: &mut Module) {
        for item in &mut module.items {
            self.normalize_patterns_in_item(&mut item.value);
        }
    }
    
    fn normalize_patterns_in_item(&self, item: &mut Item) {
        match item {
            Item::Function(func) => {
                if let Some(body) = &mut func.body {
                    self.normalize_patterns_in_block(body);
                }
            }
            _ => {}
        }
    }
    
    fn normalize_patterns_in_block(&self, block: &mut Block) {
        for stmt in &mut block.statements {
            self.normalize_patterns_in_statement(&mut stmt.value);
        }
    }
    
    fn normalize_patterns_in_statement(&self, stmt: &mut Statement) {
        match stmt {
            Statement::Expression(expr) => {
                self.normalize_patterns_in_expression(expr);
            }
            Statement::Let(let_stmt) => {
                self.normalize_patterns_in_expression(&mut let_stmt.value);
            }
        }
    }
    
    fn normalize_patterns_in_expression(&self, expr: &mut Expression) {
        match expr {
            Expression::Match(match_expr) => {
                self.normalize_patterns_in_expression(&mut match_expr.scrutinee);
                for arm in &mut match_expr.arms {
                    self.normalize_pattern(&mut arm.pattern);
                    if let Some(guard) = &mut arm.guard {
                        self.normalize_patterns_in_expression(guard);
                    }
                    self.normalize_patterns_in_expression(&mut arm.body);
                }
            }
            Expression::Call(call) => {
                self.normalize_patterns_in_expression(&mut call.callee);
                for arg in &mut call.args {
                    self.normalize_patterns_in_expression(arg);
                }
            }
            Expression::Member(member) => {
                self.normalize_patterns_in_expression(&mut member.object);
            }
            Expression::Binary(binary) => {
                self.normalize_patterns_in_expression(&mut binary.left);
                self.normalize_patterns_in_expression(&mut binary.right);
            }
            Expression::Block(block) => {
                self.normalize_patterns_in_block(block);
            }
            Expression::VariantCtor { args, .. } => {
                for arg in args {
                    self.normalize_patterns_in_expression(arg);
                }
            }
            Expression::ObjectLiteral(fields) => {
                for field in fields {
                    self.normalize_patterns_in_expression(&mut field.value);
                }
            }
            _ => {}
        }
    }
    
    fn normalize_pattern(&self, pattern: &mut Pattern) {
        match pattern {
            Pattern::Identifier(name) => {
                if self.is_unit_variant(name) {
                    // Convert to unit variant pattern
                    *pattern = Pattern::Variant(name.clone(), vec![]);
                }
            }
            Pattern::Variant(_, patterns) => {
                for pattern in patterns {
                    self.normalize_pattern(pattern);
                }
            }
            Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.normalize_pattern(pattern);
                }
            }
            _ => {}
        }
    }

    /// Analyze match expression for exhaustiveness and reachability
    fn analyze_match(&mut self, match_expr: &MatchExpression, span: Span) {
        let analysis = self.check_match_exhaustiveness(&match_expr.arms, span);
        
        // Report missing patterns (exhaustiveness)
        if !analysis.missing_patterns.is_empty() {
            let missing_str = analysis.missing_patterns
                .iter()
                .map(|p| self.constructor_pattern_to_string(p))
                .collect::<Vec<_>>()
                .join(", ");
            
            self.error_at(span, format!(
                "Non-exhaustive match: missing patterns [{}]", 
                missing_str
            ));
        }
        
        // Report unreachable patterns
        for &arm_index in &analysis.unreachable_patterns {
            self.error_at(span, format!(
                "Unreachable pattern at match arm {}", 
                arm_index + 1
            ));
        }
    }
    
    /// Check exhaustiveness and reachability of match arms
    fn check_match_exhaustiveness(&self, arms: &[MatchArm], _span: Span) -> MatchAnalysis {
        let mut covered_for_reachability = Vec::new(); // For reachability analysis (no guarded patterns)
        let mut covered_for_exhaustiveness = Vec::new(); // For exhaustiveness analysis (all patterns)
        let mut unreachable_patterns = Vec::new();
        
        // Collect all constructor patterns from arms
        for (i, arm) in arms.iter().enumerate() {
            let constructor = self.pattern_to_constructor(&arm.pattern);
            
            // Check if this pattern is already covered (reachability)
            // Only count unguarded patterns as covering for reachability
            if self.is_pattern_covered(&constructor, &covered_for_reachability) {
                unreachable_patterns.push(i);
            } else {
                // Add to reachability coverage only if there's no guard
                if arm.guard.is_none() {
                    covered_for_reachability.push(constructor.clone());
                }
                
                // Always add to exhaustiveness coverage (guards can still contribute to exhaustiveness)
                covered_for_exhaustiveness.push(constructor);
            }
        }
        
        // Check for missing patterns (exhaustiveness) using all patterns
        let missing_patterns = self.find_missing_patterns(&covered_for_exhaustiveness);
        
        MatchAnalysis {
            missing_patterns,
            unreachable_patterns,
        }
    }
    
    /// Convert a pattern to a constructor pattern for analysis
    fn pattern_to_constructor(&self, pattern: &Pattern) -> ConstructorPattern {
        match pattern {
            Pattern::Wildcard => ConstructorPattern::Wildcard,
            Pattern::Identifier(name) => {
                // Check if this is a unit variant
                if self.is_unit_variant(name) {
                    ConstructorPattern::Variant { name: name.clone(), arity: 0 }
                } else {
                    // Regular binding pattern - acts like wildcard for exhaustiveness
                    ConstructorPattern::Wildcard
                }
            },
            Pattern::Variant(name, patterns) => {
                ConstructorPattern::Variant { 
                    name: name.clone(), 
                    arity: patterns.len() 
                }
            },
            Pattern::Literal(lit) => {
                let lit_str = match lit {
                    Literal::String(s) => format!("\"{}\"", s),
                    Literal::Number(n) => n.clone(),
                    Literal::Bool(b) => b.to_string(),
                };
                ConstructorPattern::Literal(lit_str)
            },
            Pattern::Tuple(patterns) => {
                // For simplicity, treat tuples as variants with arity
                ConstructorPattern::Variant { 
                    name: "tuple".to_string(), 
                    arity: patterns.len() 
                }
            },
        }
    }
    
    /// Check if a pattern is already covered by existing patterns
    fn is_pattern_covered(&self, pattern: &ConstructorPattern, covered: &[ConstructorPattern]) -> bool {
        for covered_pattern in covered {
            match (pattern, covered_pattern) {
                // Wildcard covers everything
                (_, ConstructorPattern::Wildcard) => return true,
                // Same constructor patterns
                (ConstructorPattern::Variant { name: n1, arity: a1 }, 
                 ConstructorPattern::Variant { name: n2, arity: a2 }) if n1 == n2 && a1 == a2 => return true,
                (ConstructorPattern::Literal(l1), ConstructorPattern::Literal(l2)) if l1 == l2 => return true,
                _ => continue,
            }
        }
        false
    }
    
    /// Find missing patterns to make the match exhaustive
    fn find_missing_patterns(&self, covered: &[ConstructorPattern]) -> Vec<ConstructorPattern> {
        // If we have a wildcard, the match is exhaustive
        if covered.iter().any(|p| matches!(p, ConstructorPattern::Wildcard)) {
            return Vec::new();
        }
        
        let mut missing = Vec::new();
        
        // For now, we'll do a simple analysis:
        // If we see variant patterns, check if all variants of that enum are covered
        let mut enum_variants = HashMap::new();
        
        for pattern in covered {
            if let ConstructorPattern::Variant { name, arity: _ } = pattern {
                // Try to find the enum this variant belongs to
                if let Some(enum_name) = self.find_enum_for_variant(name) {
                    enum_variants.entry(enum_name).or_insert_with(Vec::new).push(name.clone());
                }
            }
        }
        
        // Check if any enums have missing variants
        for (enum_name, covered_variants) in enum_variants {
            if let Some(all_variants) = self.get_enum_variants(&enum_name) {
                for variant in all_variants {
                    if !covered_variants.contains(&variant.name) {
                        missing.push(ConstructorPattern::Variant {
                            name: variant.name.clone(),
                            arity: variant.fields.len(),
                        });
                    }
                }
            }
        }
        
        missing
    }
    
    /// Find the enum that a variant belongs to
    fn find_enum_for_variant(&self, variant_name: &str) -> Option<String> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(variant_name) {
                if let SymbolKind::EnumVariant { enum_name, .. } = &symbol.kind {
                    return Some(enum_name.clone());
                }
            }
        }
        None
    }
    
    /// Get all variants of an enum
    fn get_enum_variants(&self, enum_name: &str) -> Option<&Vec<Variant>> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(enum_name) {
                if let SymbolKind::Type { kind: TypeKind::Enum(variants) } = &symbol.kind {
                    return Some(variants);
                }
            }
        }
        None
    }
    
    /// Convert constructor pattern to string for error messages
    fn constructor_pattern_to_string(&self, pattern: &ConstructorPattern) -> String {
        match pattern {
            ConstructorPattern::Wildcard => "_".to_string(),
            ConstructorPattern::Variant { name, arity } => {
                if *arity == 0 {
                    name.clone()
                } else {
                    format!("{}({})", name, (0..*arity).map(|_| "_").collect::<Vec<_>>().join(", "))
                }
            },
            ConstructorPattern::Literal(lit) => lit.clone(),
        }
    }
}

impl Scope {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}