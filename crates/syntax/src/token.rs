#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Literals
    String(String),
    Number(String),
    Bool(bool),
    
    // Identifiers
    Ident(String),
    
    // Keywords
    Fn,
    Let,
    Mut,
    Struct,
    Enum,
    Impl,
    Interface,
    Type,
    Pub,
    Mod,
    Import,
    Export,
    Match,
    If,
    Else,
    Async,
    Await,
    Newtype,
    Nominal,
    Extern,
    
    // Punctuation
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Semicolon,    // ;
    Colon,        // :
    DoubleColon,  // ::
    Arrow,        // ->
    FatArrow,     // =>
    Dot,          // .
    Question,     // ?
    Pipe,         // |
    Ampersand,    // &
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Equal,        // =
    EqualEqual,   // ==
    NotEqual,     // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    And,          // &&
    Or,           // ||
    Not,          // !
    Hash,         // #
    At,           // @
    Underscore,   // _
    As,           // as
    From,         // from
    
    // Special
    Newline,
    Eof,
}

impl Token {
    pub fn keyword(s: &str) -> Option<Token> {
        match s {
            "fn" => Some(Token::Fn),
            "let" => Some(Token::Let),
            "mut" => Some(Token::Mut),
            "struct" => Some(Token::Struct),
            "enum" => Some(Token::Enum),
            "impl" => Some(Token::Impl),
            "interface" => Some(Token::Interface),
            "type" => Some(Token::Type),
            "pub" => Some(Token::Pub),
            "mod" => Some(Token::Mod),
            "import" => Some(Token::Import),
            "export" => Some(Token::Export),
            "match" => Some(Token::Match),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "async" => Some(Token::Async),
            "await" => Some(Token::Await),
            "newtype" => Some(Token::Newtype),
            "nominal" => Some(Token::Nominal),
            "extern" => Some(Token::Extern),
            "as" => Some(Token::As),
            "from" => Some(Token::From),
            "true" => Some(Token::Bool(true)),
            "false" => Some(Token::Bool(false)),
            _ => None,
        }
    }
}