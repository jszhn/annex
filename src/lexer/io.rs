use crate::lexer::{Token, TokenStream, TokenType};
use std::fmt::{Display, Formatter};

impl Display for TokenStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = self
            .vector
            .iter()
            .rev()
            .map(|tok| tok.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        write!(f, "{}", str)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.typ)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            // literals
            Self::Identifier(val) => write!(f, "identifier: {}", val),
            Self::Boolean(val) => write!(f, "boolean: {}", val),
            Self::Integer(val) => write!(f, "integer: {}", val),
            Self::Decimal(val) => write!(f, "decimal: {}", val),
            // keywords
            Self::Type(val) => write!(f, "type: {}", val),
            Self::Operator(val) => write!(f, "operator: {}", val),
            Self::Function => write!(f, "fn"),
            Self::Return => write!(f, "return"),
            // specifiers
            Self::Variable => write!(f, "var"),
            Self::Constant => write!(f, "const"),
            Self::Volatile => write!(f, "vol"),
            // control
            Self::If => write!(f, "if"),
            Self::Elif => write!(f, "elif"),
            Self::Else => write!(f, "else"),
            // loops
            Self::For => write!(f, "for"),
            Self::While => write!(f, "while"),
            Self::Continue => write!(f, "continue"),
            Self::Break => write!(f, "break"),
            // delineators
            Self::Separator(val) => write!(f, "separator: {}", val),
            Self::GroupBegin(val) => write!(f, "group begin: {}", val),
            Self::GroupEnd(val) => write!(f, "group end: {}", val),
            // special
            Self::EOF => write!(f, "eof"),
        }
    }
}
