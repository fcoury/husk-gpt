pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

pub use ast::*;
pub use diagnostics::Span;
pub use lexer::*;
pub use parser::*;
pub use token::*;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser as HuskParser;
    use crate::token::Token;

    #[test]
    fn test_lexer_basic_tokens() {
        let mut lexer = Lexer::new("fn main() { let x = 42; }".to_string(), 0);
        let tokens = lexer.tokenize();

        // Check we get the right sequence of tokens
        assert_eq!(tokens[0].value, Token::Fn);
        assert!(matches!(tokens[1].value, Token::Ident(_)));
        assert_eq!(tokens[2].value, Token::LeftParen);
        assert_eq!(tokens[3].value, Token::RightParen);
        assert_eq!(tokens[4].value, Token::LeftBrace);
        assert_eq!(tokens[5].value, Token::Let);
        // Should have EOF at the end
        assert_eq!(tokens.last().unwrap().value, Token::Eof);
    }

    #[test]
    #[ignore = "Unicode support not yet implemented - P2 feature"]
    fn test_lexer_unicode_support() {
        let mut lexer = Lexer::new("let café = \"héllo 🚀\"".to_string(), 0);
        let tokens = lexer.tokenize();

        // Check that Unicode characters are handled correctly
        assert_eq!(tokens[0].value, Token::Let);
        if let Token::Ident(name) = &tokens[1].value {
            assert_eq!(name, "café");
        } else {
            panic!("Expected identifier with Unicode");
        }

        if let Token::String(s) = &tokens[3].value {
            assert_eq!(s, "héllo 🚀");
        } else {
            panic!("Expected string with Unicode and emoji");
        }
    }

    #[test]
    fn test_parser_enum_with_variants() {
        let input = r#"
            pub enum Option {
                None,
                Some(string)
            }
        "#;

        let mut lexer = Lexer::new(input.to_string(), 0);
        let tokens = lexer.tokenize();
        let mut parser = HuskParser::new(tokens);
        let (module, diagnostics) = parser.parse();

        assert!(diagnostics.is_empty());
        assert_eq!(module.items.len(), 1);

        if let Item::Enum(enm) = &module.items[0].value {
            assert_eq!(enm.name, "Option");
            assert_eq!(enm.variants.len(), 2);
            assert_eq!(enm.variants[0].name, "None");
            assert_eq!(enm.variants[1].name, "Some");
            assert!(enm.variants[0].fields.is_empty());
            assert_eq!(enm.variants[1].fields.len(), 1);
        } else {
            panic!("Expected enum item");
        }
    }

    #[test]
    #[ignore = "Enhanced types (arrays, generics) not yet implemented - P2 feature"]
    fn test_parser_enhanced_types() {
        let input = r#"
            fn process(items: [string], lookup: HashMap<string, number>) -> (string, number) {
                items[0]
            }
        "#;

        let mut lexer = Lexer::new(input.to_string(), 0);
        let tokens = lexer.tokenize();
        let mut parser = HuskParser::new(tokens);
        let (module, diagnostics) = parser.parse();

        assert!(diagnostics.is_empty());
        assert_eq!(module.items.len(), 1);

        if let Item::Function(func) = &module.items[0].value {
            assert_eq!(func.params.len(), 2);

            // Check array type: [string]
            if let Type::Array(elem_type) = &func.params[0].ty {
                assert!(matches!(**elem_type, Type::Path(_)));
            } else {
                panic!("Expected array type");
            }

            // Check generic type: HashMap<string, number>
            if let Type::Generic(name, params) = &func.params[1].ty {
                assert_eq!(name, "HashMap");
                assert_eq!(params.len(), 2);
            } else {
                panic!("Expected generic type");
            }

            // Check tuple return type: (string, number)
            if let Some(Type::Tuple(types)) = &func.return_type {
                assert_eq!(types.len(), 2);
            } else {
                panic!("Expected tuple return type");
            }
        } else {
            panic!("Expected function item");
        }
    }
}
