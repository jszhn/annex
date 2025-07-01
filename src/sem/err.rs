use std::fmt::{Debug, Display, Formatter};

pub enum SemError {
    BadReturnType(String),
    BadAssignment(String),
    RedefFunction(String),
    RedefVar(String),
    MissingFunction(String),
    MissingVariable(String),
    InternalError(String),
}

impl Debug for SemError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SemError")
            .field(
                "message",
                match self {
                    SemError::BadReturnType(msg) => msg,
                    SemError::BadAssignment(msg) => msg,
                    SemError::RedefFunction(msg) => msg,
                    SemError::RedefVar(msg) => msg,
                    SemError::MissingFunction(msg) => msg,
                    SemError::MissingVariable(msg) => msg,
                    SemError::InternalError(msg) => msg,
                },
            )
            .finish()
    }
}

impl Display for SemError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SemError::BadReturnType(msg) => write!(f, "Bad return type: {msg}"),
            SemError::BadAssignment(msg) => write!(f, "Mismatched assignment type: {msg}"),
            SemError::RedefFunction(msg) => write!(
                f,
                "Function with same name already defined in same scope: {msg}"
            ),
            SemError::RedefVar(msg) => write!(
                f,
                "Scalar or array with same name already defined in same scope: {msg}"
            ),
            SemError::MissingFunction(msg) => {
                write!(f, "Missing a matching function declaration: {msg}")
            }
            SemError::MissingVariable(msg) => {
                write!(f, "Missing a matching variable declaration: {msg}")
            }
            SemError::InternalError(msg) => write!(f, "Internal compiler error: {msg}"),
        }
    }
}

impl std::error::Error for SemError {}
