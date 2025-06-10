use std::fmt::{Debug, Display, Formatter};
use std::io::Error;

use fs_err as fs;

use crate::lexer::{Lexer, Token, TokenType};

impl Lexer {
    pub fn print_tokens(&self) {
        // only prints tokens
        for item in self.tokens.iter().rev() {
            if let Some(value) = &item.lexeme {
                println!("{}", value);
            }
        }
    }

    pub fn print_all(&self) {
        // prints all: enum value and lexer
        for item in self.tokens.iter().rev() {
            if let Some(value) = &item.lexeme {
                println!("{}: {}", item.token_type, value);
            } else {
                println!("{}", item.token_type);
            }
        }
    }
}

impl Token {
    pub fn print(&self) {
        if let Some(val) = &self.lexeme {
            println!("{}", val);
        }
    }
}

impl Lexer {
    #[allow(dead_code)]
    fn to_txt(&self, path: String) -> Result<(), Error> {
        let mut output = String::from("");
        for item in self.tokens.iter().rev() {
            let mut buf = get_token_name(item.token_type);
            if let Some(value) = &item.lexeme {
                buf.push_str(": ");
                buf.push_str(value.as_str());
                buf.push('\n');
                output.push_str(buf.as_str());
            }
        }
        fs::write(path, output)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Type => write!(f, "Type"),
            Self::Control => write!(f, "Control"),
            Self::Specifier => write!(f, "Specifier"),
            Self::Operator => write!(f, "Operator"),
            Self::Identifier => write!(f, "Identifier"),
            Self::Separator => write!(f, "Separator"),
            Self::Return => write!(f, "Return"),
            Self::Boolean => write!(f, "Boolean"),
            Self::GroupBegin => write!(f, "GroupBegin"),
            Self::GroupEnd => write!(f, "GroupEnd"),
            Self::Integer => write!(f, "Integer"),
            Self::Decimal => write!(f, "Decimal"),
            Self::Function => write!(f, "Function"),
            Self::EOF => write!(f, "EOF"),
            _ => write!(f, "Unknown token type"),
        }
    }
}
#[allow(dead_code)]
fn get_token_name(_type: TokenType) -> String {
    // to avoid needing the debug specifier
    match _type {
        TokenType::Type => "Type".to_string(),
        TokenType::Control => "Control".to_string(),
        TokenType::Specifier => "Specifier".to_string(),
        TokenType::Operator => "Operator".to_string(),
        TokenType::Identifier => "Identifier".to_string(),
        TokenType::Separator => "Separator".to_string(),
        TokenType::Return => "Return".to_string(),
        TokenType::Boolean => "Boolean".to_string(),
        TokenType::GroupBegin => "GroupBegin".to_string(),
        TokenType::GroupEnd => "GroupEnd".to_string(),
        TokenType::Integer => "Integer".to_string(),
        TokenType::Function => "Function".to_string(),
        _ => "None".to_string(),
    }
}

pub struct LexerError {
    message: String,
}

impl LexerError {
    pub fn new(msg: &str) -> LexerError {
        LexerError {
            message: msg.to_string(),
        }
    }
}

impl Debug for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LexerError")
            .field("message", &self.message)
            .finish()
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for LexerError {}
