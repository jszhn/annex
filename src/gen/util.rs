use std::fmt::{Debug, Display, Formatter};

pub struct AsmError {
    message: String,
}

impl AsmError {
    pub fn new(msg: &str) -> AsmError {
        AsmError {
            message: msg.to_string(),
        }
    }
}

impl Debug for AsmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AsmError")
            .field("message", &self.message)
            .finish()
    }
}

impl Display for AsmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for AsmError {}
