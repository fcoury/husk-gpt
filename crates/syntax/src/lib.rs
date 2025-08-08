pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;

pub use token::*;
pub use lexer::*;
pub use ast::*;
pub use parser::*;
pub use diagnostics::Span;

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
