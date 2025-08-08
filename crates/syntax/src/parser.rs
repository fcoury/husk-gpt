use crate::{Token, Spanned, ast::*};
use crate::ast::{ImportClause, ImportItem, Export, ExportItem};
use diagnostics::{Span, Diagnostic, ErrorCode};

pub struct Parser {
    tokens: Vec<Spanned<Token>>,
    current: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self {
            tokens,
            current: 0,
            diagnostics: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> (Module, Vec<Diagnostic>) {
        let mut items = Vec::new();
        
        while !self.is_at_end() {
            self.skip_newlines();
            if self.is_at_end() {
                break;
            }
            
            if let Some(item) = self.parse_item() {
                items.push(item);
            } else {
                self.advance();
            }
        }
        
        let module = Module { items };
        let diagnostics = std::mem::take(&mut self.diagnostics);
        (module, diagnostics)
    }

    fn parse_item(&mut self) -> Option<Spanned<Item>> {
        let start_span = self.peek()?.span;
        
        let vis = if self.match_token(&Token::Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        match &self.peek()?.value {
            Token::Fn => self.parse_function(vis, start_span),
            Token::Enum => self.parse_enum(vis, start_span),
            Token::Struct => self.parse_struct(vis, start_span),
            Token::Type => self.parse_type_alias(vis, start_span),
            Token::Interface => self.parse_interface(vis, start_span),
            Token::Extern => self.parse_extern_mod(start_span),
            Token::Import => self.parse_import(start_span),
            Token::Export => self.parse_export(start_span),
            _ => {
                self.error("Expected item");
                None
            }
        }
    }

    fn parse_function(&mut self, vis: Visibility, start_span: Span) -> Option<Spanned<Item>> {
        self.consume(&Token::Fn, "Expected 'fn'")?;
        
        let name = if let Token::Ident(name) = &self.peek()?.value {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.error("Expected function name");
            return None;
        };

        self.consume(&Token::LeftParen, "Expected '('")?;
        
        let mut params = Vec::new();
        if !self.check(&Token::RightParen) {
            loop {
                if let Some(param) = self.parse_parameter() {
                    params.push(param);
                } else {
                    break;
                }
                
                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }
        
        self.consume(&Token::RightParen, "Expected ')'")?;

        let return_type = if self.match_token(&Token::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = if self.check(&Token::LeftBrace) {
            Some(self.parse_block()?)
        } else {
            None
        };

        let end_span = self.previous()?.span;
        let span = Span::new(start_span.file_id, start_span.start, end_span.end);

        Some(Spanned::new(
            Item::Function(Function {
                vis,
                name,
                params,
                return_type,
                body,
            }),
            span
        ))
    }

    fn parse_enum(&mut self, vis: Visibility, start_span: Span) -> Option<Spanned<Item>> {
        self.consume(&Token::Enum, "Expected 'enum'")?;
        
        let name = if let Token::Ident(name) = &self.peek()?.value {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.error("Expected enum name");
            return None;
        };

        self.consume(&Token::LeftBrace, "Expected '{'")?;
        
        let mut variants = Vec::new();
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::RightBrace) || self.is_at_end() {
                break;
            }
            
            if let Some(variant) = self.parse_variant() {
                variants.push(variant);
                // Handle trailing comma or newline
                self.match_token(&Token::Comma);
                self.skip_newlines();
            } else {
                break;
            }
        }
        
        self.consume(&Token::RightBrace, "Expected '}'")?;

        let end_span = self.previous()?.span;
        let span = Span::new(start_span.file_id, start_span.start, end_span.end);

        Some(Spanned::new(
            Item::Enum(Enum {
                vis,
                name,
                variants,
            }),
            span
        ))
    }

    fn parse_variant(&mut self) -> Option<Variant> {
        let name = if let Token::Ident(name) = &self.peek()?.value {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.error("Expected variant name");
            return None;
        };

        let mut fields = Vec::new();
        if self.match_token(&Token::LeftParen) {
            if !self.check(&Token::RightParen) {
                loop {
                    if let Some(ty) = self.parse_type() {
                        fields.push(ty);
                    } else {
                        break;
                    }
                    
                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }
            self.consume(&Token::RightParen, "Expected ')'")?;
        }

        Some(Variant { name, fields })
    }

    fn parse_parameter(&mut self) -> Option<Parameter> {
        let name = if let Token::Ident(name) = &self.peek()?.value {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.error("Expected parameter name");
            return None;
        };

        self.consume(&Token::Colon, "Expected ':'")?;
        let ty = self.parse_type()?;

        Some(Parameter { name, ty })
    }

    fn parse_type(&mut self) -> Option<Type> {
        match &self.peek()?.value {
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                Some(Type::Path(name))
            },
            _ => {
                self.error("Expected type");
                None
            }
        }
    }

    fn parse_block(&mut self) -> Option<Block> {
        self.consume(&Token::LeftBrace, "Expected '{'")?;
        
        let mut statements = Vec::new();
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::RightBrace) || self.is_at_end() {
                break;
            }
            
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                self.advance();
            }
        }
        
        self.consume(&Token::RightBrace, "Expected '}'")?;
        
        Some(Block { statements })
    }

    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        let start_span = self.peek()?.span;
        
        match &self.peek()?.value {
            Token::Let => {
                self.advance();
                let mutable = self.match_token(&Token::Mut);
                
                let name = if let Token::Ident(name) = &self.peek()?.value {
                    let name = name.clone();
                    self.advance();
                    name
                } else {
                    self.error("Expected variable name");
                    return None;
                };

                let ty = if self.match_token(&Token::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                self.consume(&Token::Equal, "Expected '='")?;
                let value = self.parse_expression()?;
                
                // Optional semicolon
                self.match_token(&Token::Semicolon);

                let end_span = self.previous()?.span;
                let span = Span::new(start_span.file_id, start_span.start, end_span.end);

                Some(Spanned::new(
                    Statement::Let(LetStatement {
                        mutable,
                        name,
                        ty,
                        value,
                    }),
                    span
                ))
            },
            _ => {
                let expr = self.parse_expression()?;
                
                // Optional semicolon
                self.match_token(&Token::Semicolon);
                
                let end_span = self.previous()?.span;
                let span = Span::new(start_span.file_id, start_span.start, end_span.end);
                
                Some(Spanned::new(Statement::Expression(expr), span))
            }
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_or_expression()
    }
    
    fn parse_or_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_and_expression()?;
        
        while self.match_token(&Token::Or) {
            let right = self.parse_and_expression()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: BinaryOperator::Or,
                right: Box::new(right),
            });
        }
        
        Some(expr)
    }
    
    fn parse_and_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_equality_expression()?;
        
        while self.match_token(&Token::And) {
            let right = self.parse_equality_expression()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: BinaryOperator::And,
                right: Box::new(right),
            });
        }
        
        Some(expr)
    }
    
    fn parse_equality_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_comparison_expression()?;
        
        while let Some(op) = self.match_equality_operator() {
            let right = self.parse_comparison_expression()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            });
        }
        
        Some(expr)
    }
    
    fn match_equality_operator(&mut self) -> Option<BinaryOperator> {
        match self.peek()?.value {
            Token::EqualEqual => {
                self.advance();
                Some(BinaryOperator::Equal)
            }
            Token::NotEqual => {
                self.advance();
                Some(BinaryOperator::NotEqual)
            }
            _ => None
        }
    }
    
    fn parse_comparison_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_additive_expression()?;
        
        while let Some(op) = self.match_comparison_operator() {
            let right = self.parse_additive_expression()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            });
        }
        
        Some(expr)
    }
    
    fn match_comparison_operator(&mut self) -> Option<BinaryOperator> {
        match self.peek()?.value {
            Token::Less => {
                self.advance();
                Some(BinaryOperator::Less)
            }
            Token::LessEqual => {
                self.advance();
                Some(BinaryOperator::LessEqual)
            }
            Token::Greater => {
                self.advance();
                Some(BinaryOperator::Greater)
            }
            Token::GreaterEqual => {
                self.advance();
                Some(BinaryOperator::GreaterEqual)
            }
            _ => None
        }
    }
    
    fn parse_additive_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_multiplicative_expression()?;
        
        while let Some(op) = self.match_additive_operator() {
            let right = self.parse_multiplicative_expression()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            });
        }
        
        Some(expr)
    }
    
    fn match_additive_operator(&mut self) -> Option<BinaryOperator> {
        match self.peek()?.value {
            Token::Plus => {
                self.advance();
                Some(BinaryOperator::Add)
            }
            Token::Minus => {
                self.advance();
                Some(BinaryOperator::Subtract)
            }
            _ => None
        }
    }
    
    fn parse_multiplicative_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_postfix_expression()?;
        
        while let Some(op) = self.match_multiplicative_operator() {
            let right = self.parse_postfix_expression()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            });
        }
        
        Some(expr)
    }
    
    fn match_multiplicative_operator(&mut self) -> Option<BinaryOperator> {
        match self.peek()?.value {
            Token::Star => {
                self.advance();
                Some(BinaryOperator::Multiply)
            }
            Token::Slash => {
                self.advance();
                Some(BinaryOperator::Divide)
            }
            Token::Percent => {
                self.advance();
                Some(BinaryOperator::Modulo)
            }
            _ => None
        }
    }
    
    fn parse_postfix_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_primary_expression()?;
        
        loop {
            match &self.peek()?.value {
                Token::LeftParen => {
                    // Function call
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    
                    if !self.check(&Token::RightParen) {
                        loop {
                            if let Some(arg) = self.parse_expression() {
                                args.push(arg);
                            } else {
                                break;
                            }
                            
                            if !self.match_token(&Token::Comma) {
                                break;
                            }
                        }
                    }
                    
                    self.consume(&Token::RightParen, "Expected ')' after arguments")?;
                    
                    expr = Expression::Call(CallExpression {
                        callee: Box::new(expr),
                        args,
                    });
                }
                Token::Dot => {
                    // Member access
                    self.advance(); // consume '.'
                    
                    if let Token::Ident(property) = &self.peek()?.value {
                        let property = property.clone();
                        self.advance();
                        
                        expr = Expression::Member(MemberExpression {
                            object: Box::new(expr),
                            property,
                        });
                    } else {
                        self.error("Expected property name after '.'");
                        return None;
                    }
                }
                _ => break,
            }
        }
        
        Some(expr)
    }
    
    fn parse_primary_expression(&mut self) -> Option<Expression> {
        match &self.peek()?.value {
            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Some(Expression::Literal(Literal::String(s)))
            }
            Token::Number(n) => {
                let n = n.clone();
                self.advance();
                Some(Expression::Literal(Literal::Number(n)))
            }
            Token::Bool(b) => {
                let b = *b;
                self.advance();
                Some(Expression::Literal(Literal::Bool(b)))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                
                // Check for variant constructor: Enum::Variant
                if self.match_token(&Token::DoubleColon) {
                    if let Token::Ident(variant) = &self.peek()?.value {
                        let variant = variant.clone();
                        self.advance();
                        
                        // Check if followed by arguments
                        if self.check(&Token::LeftParen) {
                            self.advance(); // consume '('
                            let mut args = Vec::new();
                            
                            if !self.check(&Token::RightParen) {
                                loop {
                                    if let Some(arg) = self.parse_expression() {
                                        args.push(arg);
                                    } else {
                                        break;
                                    }
                                    
                                    if !self.match_token(&Token::Comma) {
                                        break;
                                    }
                                }
                            }
                            
                            self.consume(&Token::RightParen, "Expected ')' after variant arguments")?;
                            
                            Some(Expression::VariantCtor {
                                enum_name: name,
                                variant,
                                args,
                            })
                        } else {
                            // Unit variant
                            Some(Expression::VariantCtor {
                                enum_name: name,
                                variant,
                                args: Vec::new(),
                            })
                        }
                    } else {
                        self.error("Expected variant name after '::'");
                        None
                    }
                } else {
                    Some(Expression::Identifier(name))
                }
            }
            Token::LeftParen => {
                // Grouping
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                self.consume(&Token::RightParen, "Expected ')' after expression")?;
                Some(expr)
            }
            Token::LeftBrace => {
                // Object literal
                self.advance(); // consume '{'
                let mut fields = Vec::new();
                
                if !self.check(&Token::RightBrace) {
                    loop {
                        if let Token::Ident(name) = &self.peek()?.value {
                            let name = name.clone();
                            self.advance();
                            
                            self.consume(&Token::Colon, "Expected ':' after field name")?;
                            let value = self.parse_expression()?;
                            
                            fields.push(ObjectField { name, value });
                            
                            if !self.match_token(&Token::Comma) {
                                break;
                            }
                        } else {
                            self.error("Expected field name in object literal");
                            break;
                        }
                    }
                }
                
                self.consume(&Token::RightBrace, "Expected '}' after object fields")?;
                Some(Expression::ObjectLiteral(fields))
            }
            Token::Match => {
                // Match expression
                self.advance(); // consume 'match'
                let scrutinee = Box::new(self.parse_expression()?);
                
                self.consume(&Token::LeftBrace, "Expected '{' after match expression")?;
                
                let mut arms = Vec::new();
                while !self.check(&Token::RightBrace) && !self.is_at_end() {
                    self.skip_newlines();
                    if self.check(&Token::RightBrace) || self.is_at_end() {
                        break;
                    }
                    
                    if let Some(arm) = self.parse_match_arm() {
                        arms.push(arm);
                        // Optional trailing comma
                        self.match_token(&Token::Comma);
                    } else {
                        break;
                    }
                }
                
                self.consume(&Token::RightBrace, "Expected '}' after match arms")?;
                
                Some(Expression::Match(MatchExpression {
                    scrutinee,
                    arms,
                }))
            }
            _ => {
                self.error("Expected expression");
                None
            }
        }
    }
    
    fn parse_match_arm(&mut self) -> Option<MatchArm> {
        let pattern = self.parse_pattern()?;
        
        // Optional guard
        let guard = if self.match_token(&Token::If) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        self.consume(&Token::FatArrow, "Expected '=>' after match pattern")?;
        let body = self.parse_expression()?;
        
        Some(MatchArm {
            pattern,
            guard,
            body,
        })
    }
    
    fn parse_pattern(&mut self) -> Option<Pattern> {
        match &self.peek()?.value {
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                
                // Check if it's a variant pattern
                if self.check(&Token::LeftParen) {
                    self.advance(); // consume '('
                    let mut patterns = Vec::new();
                    
                    if !self.check(&Token::RightParen) {
                        loop {
                            if let Some(pattern) = self.parse_pattern() {
                                patterns.push(pattern);
                            } else {
                                break;
                            }
                            
                            if !self.match_token(&Token::Comma) {
                                break;
                            }
                        }
                    }
                    
                    self.consume(&Token::RightParen, "Expected ')' after variant patterns")?;
                    Some(Pattern::Variant(name, patterns))
                } else {
                    Some(Pattern::Identifier(name))
                }
            }
            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Some(Pattern::Literal(Literal::String(s)))
            }
            Token::Number(n) => {
                let n = n.clone();
                self.advance();
                Some(Pattern::Literal(Literal::Number(n)))
            }
            Token::Bool(b) => {
                let b = *b;
                self.advance();
                Some(Pattern::Literal(Literal::Bool(b)))
            }
            Token::Underscore => {
                self.advance();
                Some(Pattern::Wildcard)
            }
            Token::LeftParen => {
                // Tuple pattern
                self.advance(); // consume '('
                let mut patterns = Vec::new();
                
                if !self.check(&Token::RightParen) {
                    loop {
                        if let Some(pattern) = self.parse_pattern() {
                            patterns.push(pattern);
                        } else {
                            break;
                        }
                        
                        if !self.match_token(&Token::Comma) {
                            break;
                        }
                    }
                }
                
                self.consume(&Token::RightParen, "Expected ')' after tuple patterns")?;
                Some(Pattern::Tuple(patterns))
            }
            _ => {
                self.error("Expected pattern");
                None
            }
        }
    }

    // Stub implementations for other item types
    fn parse_struct(&mut self, vis: Visibility, start_span: Span) -> Option<Spanned<Item>> {
        self.consume(&Token::Struct, "Expected 'struct'")?;
        
        let name = if let Token::Ident(name) = &self.peek()?.value {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.error("Expected struct name");
            return None;
        };

        self.consume(&Token::LeftBrace, "Expected '{'")?;
        
        let mut fields = Vec::new();
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::RightBrace) || self.is_at_end() {
                break;
            }
            
            // Parse field visibility
            let field_vis = if self.match_token(&Token::Pub) {
                Visibility::Public
            } else {
                Visibility::Private
            };
            
            // Parse field name
            let field_name = if let Token::Ident(name) = &self.peek()?.value {
                let name = name.clone();
                self.advance();
                name
            } else {
                self.error("Expected field name");
                break;
            };
            
            self.consume(&Token::Colon, "Expected ':' after field name")?;
            let field_type = self.parse_type()?;
            
            fields.push(Field {
                vis: field_vis,
                name: field_name,
                ty: field_type,
            });
            
            // Handle trailing comma or newline
            self.match_token(&Token::Comma);
            self.skip_newlines();
        }
        
        self.consume(&Token::RightBrace, "Expected '}'")?;

        let end_span = self.previous()?.span;
        let span = Span::new(start_span.file_id, start_span.start, end_span.end);

        Some(Spanned::new(
            Item::Struct(Struct {
                vis,
                name,
                fields,
            }),
            span
        ))
    }

    fn parse_type_alias(&mut self, vis: Visibility, start_span: Span) -> Option<Spanned<Item>> {
        self.consume(&Token::Type, "Expected 'type'")?;
        
        let name = if let Token::Ident(name) = &self.peek()?.value {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.error("Expected type alias name");
            return None;
        };

        self.consume(&Token::Equal, "Expected '=' after type name")?;
        let ty = self.parse_type()?;

        // Optional semicolon
        self.match_token(&Token::Semicolon);
        
        let end_span = self.previous()?.span;
        let span = Span::new(start_span.file_id, start_span.start, end_span.end);

        Some(Spanned::new(
            Item::TypeAlias(TypeAlias {
                vis,
                name,
                ty,
            }),
            span
        ))
    }

    fn parse_interface(&mut self, vis: Visibility, start_span: Span) -> Option<Spanned<Item>> {
        self.consume(&Token::Interface, "Expected 'interface'")?;
        
        let name = if let Token::Ident(name) = &self.peek()?.value {
            let name = name.clone();
            self.advance();
            name
        } else {
            self.error("Expected interface name");
            return None;
        };

        self.consume(&Token::LeftBrace, "Expected '{'")?;
        
        let mut methods = Vec::new();
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::RightBrace) || self.is_at_end() {
                break;
            }
            
            // Parse method signature
            let method_name = if let Token::Ident(name) = &self.peek()?.value {
                let name = name.clone();
                self.advance();
                name
            } else {
                self.error("Expected method name");
                break;
            };
            
            self.consume(&Token::LeftParen, "Expected '(' after method name")?;
            
            let mut params = Vec::new();
            if !self.check(&Token::RightParen) {
                loop {
                    if let Some(param) = self.parse_parameter() {
                        params.push(param);
                    } else {
                        break;
                    }
                    
                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }
            
            self.consume(&Token::RightParen, "Expected ')'")?;

            let return_type = if self.match_token(&Token::Arrow) {
                Some(self.parse_type()?)
            } else {
                None
            };
            
            methods.push(Function {
                vis: Visibility::Public, // Interface methods are always public
                name: method_name,
                params,
                return_type,
                body: None, // Interface methods don't have bodies
            });
            
            // Optional comma or semicolon
            self.match_token(&Token::Comma) || self.match_token(&Token::Semicolon);
        }
        
        self.consume(&Token::RightBrace, "Expected '}'")?;

        let end_span = self.previous()?.span;
        let span = Span::new(start_span.file_id, start_span.start, end_span.end);

        Some(Spanned::new(
            Item::Interface(Interface {
                vis,
                name,
                methods,
            }),
            span
        ))
    }

    fn parse_extern_mod(&mut self, _start_span: Span) -> Option<Spanned<Item>> {
        self.error("Extern mod parsing not implemented yet");
        None
    }
    
    fn parse_import(&mut self, start_span: Span) -> Option<Spanned<Item>> {
        self.consume(&Token::Import, "Expected 'import'")?;
        
        let items = if self.match_token(&Token::Star) {
            // import * as name from "path"
            self.consume(&Token::As, "Expected 'as' after '*'")?;
            if let Token::Ident(name) = &self.peek()?.value {
                let name = name.clone();
                self.advance();
                ImportClause::Namespace(name)
            } else {
                self.error("Expected identifier after 'as'");
                return None;
            }
        } else if self.check(&Token::LeftBrace) {
            // import { ... } from "path"
            self.advance(); // consume '{'
            let mut items = Vec::new();
            
            if !self.check(&Token::RightBrace) {
                loop {
                    if let Token::Ident(name) = &self.peek()?.value {
                        let name = name.clone();
                        self.advance();
                        
                        let alias = if self.match_token(&Token::As) {
                            if let Token::Ident(alias) = &self.peek()?.value {
                                let alias = alias.clone();
                                self.advance();
                                Some(alias)
                            } else {
                                self.error("Expected identifier after 'as'");
                                return None;
                            }
                        } else {
                            None
                        };
                        
                        items.push(ImportItem { name, alias });
                        
                        if !self.match_token(&Token::Comma) {
                            break;
                        }
                    } else {
                        self.error("Expected identifier in import list");
                        break;
                    }
                }
            }
            
            self.consume(&Token::RightBrace, "Expected '}' after import list")?;
            ImportClause::Named(items)
        } else if let Token::Ident(name) = &self.peek()?.value {
            // import defaultName from "path" or import defaultName, { ... } from "path"
            let default_name = name.clone();
            self.advance();
            
            if self.match_token(&Token::Comma) {
                // Mixed import: import defaultName, { ... } from "path"
                self.consume(&Token::LeftBrace, "Expected '{' after comma")?;
                let mut named_items = Vec::new();
                
                if !self.check(&Token::RightBrace) {
                    loop {
                        if let Token::Ident(name) = &self.peek()?.value {
                            let name = name.clone();
                            self.advance();
                            
                            let alias = if self.match_token(&Token::As) {
                                if let Token::Ident(alias) = &self.peek()?.value {
                                    let alias = alias.clone();
                                    self.advance();
                                    Some(alias)
                                } else {
                                    self.error("Expected identifier after 'as'");
                                    return None;
                                }
                            } else {
                                None
                            };
                            
                            named_items.push(ImportItem { name, alias });
                            
                            if !self.match_token(&Token::Comma) {
                                break;
                            }
                        } else {
                            self.error("Expected identifier in import list");
                            break;
                        }
                    }
                }
                
                self.consume(&Token::RightBrace, "Expected '}' after import list")?;
                ImportClause::Mixed { default: default_name, named: named_items }
            } else {
                // Simple default import
                ImportClause::Default(default_name)
            }
        } else {
            self.error("Expected import clause");
            return None;
        };
        
        self.consume(&Token::From, "Expected 'from' after import clause")?;
        
        let path = if let Token::String(path) = &self.peek()?.value {
            let path = path.clone();
            self.advance();
            path
        } else {
            self.error("Expected string literal for import path");
            return None;
        };
        
        // Consume optional semicolon
        self.match_token(&Token::Semicolon);
        
        let end_span = self.previous()?.span;
        let span = Span::new(start_span.file_id, start_span.start, end_span.end);
        
        Some(Spanned::new(
            Item::Import(Import { path, items }),
            span
        ))
    }
    
    fn parse_export(&mut self, start_span: Span) -> Option<Spanned<Item>> {
        self.consume(&Token::Export, "Expected 'export'")?;
        
        let export = if self.match_token(&Token::Star) {
            // export * from "path"
            self.consume(&Token::From, "Expected 'from' after '*'")?;
            if let Token::String(path) = &self.peek()?.value {
                let path = path.clone();
                self.advance();
                self.match_token(&Token::Semicolon); // Consume optional semicolon
                Export::All(path)
            } else {
                self.error("Expected string literal after 'from'");
                return None;
            }
        } else if self.check(&Token::LeftBrace) {
            // export { ... } or export { ... } from "path"
            self.advance(); // consume '{'
            let mut items = Vec::new();
            
            if !self.check(&Token::RightBrace) {
                loop {
                    if let Token::Ident(name) = &self.peek()?.value {
                        let name = name.clone();
                        self.advance();
                        
                        let alias = if self.match_token(&Token::As) {
                            if let Token::Ident(alias) = &self.peek()?.value {
                                let alias = alias.clone();
                                self.advance();
                                Some(alias)
                            } else {
                                self.error("Expected identifier after 'as'");
                                return None;
                            }
                        } else {
                            None
                        };
                        
                        items.push(ExportItem { name, alias });
                        
                        if !self.match_token(&Token::Comma) {
                            break;
                        }
                    } else {
                        self.error("Expected identifier in export list");
                        break;
                    }
                }
            }
            
            self.consume(&Token::RightBrace, "Expected '}' after export list")?;
            
            if self.match_token(&Token::From) {
                // export { ... } from "path"
                if let Token::String(path) = &self.peek()?.value {
                    let path = path.clone();
                    self.advance();
                    self.match_token(&Token::Semicolon); // Consume optional semicolon
                    Export::NamedFrom { items, path }
                } else {
                    self.error("Expected string literal after 'from'");
                    return None;
                }
            } else {
                // export { ... }
                self.match_token(&Token::Semicolon); // Consume optional semicolon
                Export::Named(items)
            }
        } else {
            // export item or export default item
            let is_default = if let Token::Ident(name) = &self.peek()?.value {
                if name == "default" {
                    self.advance();
                    true
                } else {
                    false
                }
            } else {
                false
            };
            
            if let Some(item) = self.parse_item() {
                if is_default {
                    Export::Default(Box::new(item))
                } else {
                    Export::Item(Box::new(item))
                }
            } else {
                self.error("Expected item after 'export'");
                return None;
            }
        };
        
        let end_span = self.previous()?.span;
        let span = Span::new(start_span.file_id, start_span.start, end_span.end);
        
        Some(Spanned::new(
            Item::Export(export),
            span
        ))
    }

    // Helper methods
    fn peek(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> Option<&Spanned<Token>> {
        if self.current > 0 {
            self.tokens.get(self.current - 1)
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<&Spanned<Token>> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().map(|t| &t.value) == Some(&Token::Eof)
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, token: &Token) -> bool {
        if let Some(current) = self.peek() {
            std::mem::discriminant(&current.value) == std::mem::discriminant(token)
        } else {
            false
        }
    }

    fn consume(&mut self, token: &Token, message: &str) -> Option<&Spanned<Token>> {
        if self.check(token) {
            self.advance()
        } else {
            self.error(message);
            None
        }
    }

    fn error(&mut self, message: &str) {
        if let Some(current) = self.peek() {
            self.diagnostics.push(Diagnostic::error(
                ErrorCode::ParseError,
                current.span,
                message
            ));
        }
    }

    fn skip_newlines(&mut self) {
        while let Some(token) = self.peek() {
            if matches!(token.value, Token::Newline) {
                self.advance();
            } else {
                break;
            }
        }
    }
}