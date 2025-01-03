use std::fmt::{Debug, Display, Formatter};

pub struct AstError {
    message: String,
}

impl AstError {
    pub fn new(msg: &str) -> AstError {
        AstError {
            message: msg.to_string(),
        }
    }
}

impl Debug for AstError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AstError")
            .field("message", &self.message)
            .finish()
    }
}

impl Display for AstError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for AstError {}
