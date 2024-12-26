use std::fmt::{Debug, Display, Formatter};
use std::io::Error;

pub trait OutputHandler {
    fn to_txt(&self, path: String) -> Result<(), Error>;
    fn from_txt(path: String);
}

pub struct UnknownError {
    message: String,
}

impl UnknownError {
    pub fn new(msg: &str) -> UnknownError {
        UnknownError {
            message: msg.to_string(),
        }
    }
}

impl Debug for UnknownError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UnknownError")
            .field("message", &self.message)
            .finish()
    }
}

impl Display for UnknownError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for UnknownError {}
