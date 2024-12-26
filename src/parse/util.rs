use std::fmt::{Debug, Display, Formatter};

pub struct ParserError {
    message: String,
}

impl ParserError {
    pub fn new(msg: &str) -> ParserError {
        ParserError {
            message: msg.to_string(),
        }
    }
}

impl Debug for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParserError")
            .field("message", &self.message)
            .finish()
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParserError {}
