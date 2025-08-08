use crate::{Token, Spanned, ast::*};
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
            if let Some(variant) = self.parse_variant() {
                variants.push(variant);
                if !self.match_token(&Token::Comma) {
                    break;
                }
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
                let end_span = self.previous()?.span;
                let span = Span::new(start_span.file_id, start_span.start, end_span.end);
                
                Some(Spanned::new(Statement::Expression(expr), span))
            }
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        match &self.peek()?.value {
            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Some(Expression::Literal(Literal::String(s)))
            },
            Token::Number(n) => {
                let n = n.clone();
                self.advance();
                Some(Expression::Literal(Literal::Number(n)))
            },
            Token::Bool(b) => {
                let b = *b;
                self.advance();
                Some(Expression::Literal(Literal::Bool(b)))
            },
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                Some(Expression::Identifier(name))
            },
            _ => {
                self.error("Expected expression");
                None
            }
        }
    }

    // Stub implementations for other item types
    fn parse_struct(&mut self, _vis: Visibility, _start_span: Span) -> Option<Spanned<Item>> {
        self.error("Struct parsing not implemented yet");
        None
    }

    fn parse_type_alias(&mut self, _vis: Visibility, _start_span: Span) -> Option<Spanned<Item>> {
        self.error("Type alias parsing not implemented yet");
        None
    }

    fn parse_interface(&mut self, _vis: Visibility, _start_span: Span) -> Option<Spanned<Item>> {
        self.error("Interface parsing not implemented yet");
        None
    }

    fn parse_extern_mod(&mut self, _start_span: Span) -> Option<Spanned<Item>> {
        self.error("Extern mod parsing not implemented yet");
        None
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