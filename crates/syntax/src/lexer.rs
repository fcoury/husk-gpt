use crate::{Spanned, Token};
use diagnostics::Span;

pub struct Lexer {
    input: String,
    chars: Vec<char>,
    char_indices: Vec<usize>, // byte positions of each character
    position: usize,          // character position
    file_id: u32,
}

impl Lexer {
    pub fn new(input: String, file_id: u32) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let char_indices: Vec<usize> = input.char_indices().map(|(i, _)| i).collect();
        Self {
            input,
            chars,
            char_indices,
            position: 0,
            file_id,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Spanned<Token>> {
        let mut tokens = Vec::new();

        while self.position < self.chars.len() {
            self.skip_whitespace();

            if self.position >= self.chars.len() {
                break;
            }

            let start_char_pos = self.position;
            let token = self.next_token();
            let end_char_pos = self.position;

            if let Some(token) = token {
                let start_byte = self
                    .char_indices
                    .get(start_char_pos)
                    .copied()
                    .unwrap_or(self.input.len());
                let end_byte = self
                    .char_indices
                    .get(end_char_pos)
                    .copied()
                    .unwrap_or(self.input.len());
                tokens.push(Spanned::new(
                    token,
                    Span::new(self.file_id, start_byte as u32, end_byte as u32),
                ));
            }
        }

        tokens.push(Spanned::new(
            Token::Eof,
            Span::new(
                self.file_id,
                self.input.len() as u32,
                self.input.len() as u32,
            ),
        ));

        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        let ch = self.current_char()?;

        match ch {
            '(' => {
                self.advance();
                Some(Token::LeftParen)
            }
            ')' => {
                self.advance();
                Some(Token::RightParen)
            }
            '{' => {
                self.advance();
                Some(Token::LeftBrace)
            }
            '}' => {
                self.advance();
                Some(Token::RightBrace)
            }
            '[' => {
                self.advance();
                Some(Token::LeftBracket)
            }
            ']' => {
                self.advance();
                Some(Token::RightBracket)
            }
            ',' => {
                self.advance();
                Some(Token::Comma)
            }
            ';' => {
                self.advance();
                Some(Token::Semicolon)
            }
            '?' => {
                self.advance();
                Some(Token::Question)
            }
            '|' => {
                self.advance();
                if self.current_char() == Some('|') {
                    self.advance();
                    Some(Token::Or)
                } else {
                    Some(Token::Pipe)
                }
            }
            '&' => {
                self.advance();
                if self.current_char() == Some('&') {
                    self.advance();
                    Some(Token::And)
                } else {
                    Some(Token::Ampersand)
                }
            }
            '+' => {
                self.advance();
                Some(Token::Plus)
            }
            '*' => {
                self.advance();
                Some(Token::Star)
            }
            '/' => {
                self.advance();
                if self.current_char() == Some('/') {
                    self.skip_line_comment();
                    None
                } else {
                    Some(Token::Slash)
                }
            }
            '%' => {
                self.advance();
                Some(Token::Percent)
            }
            '.' => {
                self.advance();
                Some(Token::Dot)
            }
            '#' => {
                self.advance();
                Some(Token::Hash)
            }
            '@' => {
                self.advance();
                Some(Token::At)
            }
            '_' => {
                self.advance();
                Some(Token::Underscore)
            }
            ':' => {
                self.advance();
                if self.current_char() == Some(':') {
                    self.advance();
                    Some(Token::DoubleColon)
                } else {
                    Some(Token::Colon)
                }
            }
            '-' => {
                self.advance();
                if self.current_char() == Some('>') {
                    self.advance();
                    Some(Token::Arrow)
                } else {
                    Some(Token::Minus)
                }
            }
            '=' => {
                self.advance();
                match self.current_char() {
                    Some('=') => {
                        self.advance();
                        Some(Token::EqualEqual)
                    }
                    Some('>') => {
                        self.advance();
                        Some(Token::FatArrow)
                    }
                    _ => Some(Token::Equal),
                }
            }
            '!' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Some(Token::NotEqual)
                } else {
                    Some(Token::Not)
                }
            }
            '<' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Some(Token::LessEqual)
                } else {
                    Some(Token::Less)
                }
            }
            '>' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Some(Token::GreaterEqual)
                } else {
                    Some(Token::Greater)
                }
            }
            '"' => Some(self.read_string()),
            '\n' => {
                self.advance();
                Some(Token::Newline)
            }
            _ if ch.is_ascii_digit() => Some(self.read_number()),
            _ if ch.is_ascii_alphabetic() || ch == '_' => Some(self.read_identifier()),
            _ => {
                self.advance();
                None
            }
        }
    }

    fn read_string(&mut self) -> Token {
        self.advance(); // skip opening quote
        let mut value = String::new();

        while let Some(ch) = self.current_char() {
            if ch == '"' {
                self.advance(); // skip closing quote
                break;
            } else if ch == '\\' {
                self.advance();
                if let Some(escaped) = self.current_char() {
                    match escaped {
                        'n' => value.push('\n'),
                        't' => value.push('\t'),
                        'r' => value.push('\r'),
                        '\\' => value.push('\\'),
                        '"' => value.push('"'),
                        _ => {
                            value.push('\\');
                            value.push(escaped);
                        }
                    }
                    self.advance();
                }
            } else {
                value.push(ch);
                self.advance();
            }
        }

        Token::String(value)
    }

    fn read_number(&mut self) -> Token {
        let mut value = String::new();

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() || ch == '.' {
                value.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        Token::Number(value)
    }

    fn read_identifier(&mut self) -> Token {
        let mut value = String::new();

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                value.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        Token::keyword(&value).unwrap_or(Token::Ident(value))
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() && ch != '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn current_char(&self) -> Option<char> {
        self.chars.get(self.position).copied()
    }

    fn advance(&mut self) {
        if self.position < self.chars.len() {
            self.position += 1;
        }
    }
}
