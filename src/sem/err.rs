use std::fmt::{Debug, Display, Formatter};

use crate::ast::types::{Operator, Type};

pub struct SemError {
    kind: SemErrorKind,
    message: String,
    identifier: Option<String>,
}

impl SemError {
    fn new(kind: SemErrorKind, message: String, identifier: Option<String>) -> Self {
        Self {
            kind,
            message,
            identifier,
        }
    }

    pub fn id_redefined(identifier: String) -> Self {
        Self::new(
            SemErrorKind::IdentifierRedefined,
            "redefinition of".to_string(),
            Some(identifier),
        )
    }

    pub fn id_undefined(identifier: String) -> Self {
        Self::new(
            SemErrorKind::IdentifierUndefined,
            "use of undefined identifier".to_string(),
            Some(identifier),
        )
    }

    pub fn signature_mismatch(identifer: String) -> Self {
        Self::new(
            SemErrorKind::FunctionSignatureMismatch,
            "call to function does not meet definition".to_string(),
            Some(identifer),
        )
    }

    pub fn type_mismatch(expected: String, found: String) -> Self {
        Self::new(
            SemErrorKind::TypeMismatch,
            format!("expected {expected} but found {found}"),
            None,
        )
    }

    pub fn invalid_operands<T: Operator>(op: &T, left: Type, right: Option<Type>) -> Self {
        let right_str = if let Some(right_typ) = right {
            format!(" and {}", Type::from_enum(&right_typ))
        } else {
            String::new()
        };

        Self::new(
            SemErrorKind::InvalidOperands,
            format!(
                "{} cannot operate on types {}{}",
                Operator::from_enum(op),
                Type::from_enum(&left),
                right_str
            ),
            None,
        )
    }

    pub fn invalid_assignment(left_typ: Type, right_typ: Type) -> Self {
        Self::new(
            SemErrorKind::InvalidAssignment,
            format!(
                "cannot assign type {} to type {}",
                Type::from_enum(&right_typ),
                Type::from_enum(&left_typ)
            ),
            None,
        )
    }

    pub fn invalid_condition(typ: Type) -> Self {
        Self::new(
            SemErrorKind::InvalidCondition,
            format!("expected boolean type but found {}", Type::from_enum(&typ)),
            None,
        )
    }

    pub fn invalid_return_type(expected: String, found: String) -> Self {
        Self::new(
            SemErrorKind::InvalidReturn,
            format!("expected {expected} but found {found}"),
            None,
        )
    }

    pub fn invalid_function_call(identifier: String, reason: &str) -> Self {
        Self::new(
            SemErrorKind::InvalidCall,
            reason.to_string(),
            Some(identifier),
        )
    }

    pub fn invalid_array_size(identifier: String, reason: &str) -> Self {
        Self::new(
            SemErrorKind::InvalidArrayDefinition,
            reason.to_string(),
            Some(identifier),
        )
    }

    pub fn invalid_array_access(identifier: String, reason: &str) -> Self {
        Self::new(
            SemErrorKind::InvalidArrayAccess,
            reason.to_string(),
            Some(identifier),
        )
    }

    pub fn internal_error(message: &str) -> Self {
        Self::new(SemErrorKind::InternalError, message.to_string(), None)
    }
}

impl Debug for SemError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SemError")
            .field("kind", &self.kind)
            .field("message", &self.message)
            .field("identifier", &self.identifier)
            .finish()
    }
}

impl Display for SemError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = if let Some(id) = &self.identifier {
            format!("{}: {} ({})", self.kind, self.message, id)
        } else {
            format!("{}: {}", self.kind, self.message)
        };
        write!(f, "{str}")
    }
}

impl std::error::Error for SemError {}

#[derive(Debug)]
pub enum SemErrorKind {
    IdentifierRedefined,
    IdentifierUndefined,
    FunctionSignatureMismatch,
    TypeMismatch,
    InvalidOperands,
    InvalidAssignment,
    InvalidCondition,
    InvalidReturn,
    InvalidCall,
    InvalidArrayDefinition,
    InvalidArrayAccess,
    InternalError,
}

impl Display for SemErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SemErrorKind::IdentifierRedefined => write!(f, "IdentifierRedefined"),
            SemErrorKind::IdentifierUndefined => write!(f, "IdentifierUndefined"),
            SemErrorKind::FunctionSignatureMismatch => write!(f, "FunctionSignatureMismatch"),
            SemErrorKind::TypeMismatch => write!(f, "TypeMismatch"),
            SemErrorKind::InvalidOperands => write!(f, "InvalidOperands"),
            SemErrorKind::InvalidAssignment => write!(f, "InvalidAssignment"),
            SemErrorKind::InvalidCondition => write!(f, "InvalidCondition"),
            SemErrorKind::InvalidReturn => write!(f, "InvalidReturnType"),
            SemErrorKind::InvalidCall => write!(f, "InvalidFunctionCall"),
            SemErrorKind::InvalidArrayDefinition => write!(f, "InvalidArrayDefinition"),
            SemErrorKind::InvalidArrayAccess => write!(f, "InvalidArrayAccess"),
            SemErrorKind::InternalError => write!(f, "InternalError"),
        }
    }
}
