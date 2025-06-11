use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl Location {
    pub fn new(line: u32, column: u32) -> Self {
        Location { line, column }
    }
    
    pub fn unknown() -> Self {
        Location { line: 0, column: 0 }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub enum SemErrorKind {
    // Variable and function redefinition (check 1, 3)
    IdentifierRedefined,
    // Use before declaration (check 2)
    IdentifierUndefined,
    // Function signature mismatch (check 3, 10)
    FunctionSignatureMismatch,
    // Type mismatch errors (checks 4, 5, 6)
    TypeMismatch,
    // Invalid operand types (check 4)
    InvalidArithmeticOperands,
    // Logical operator type mismatch (check 5)
    InvalidLogicalOperands,
    // Assignment type mismatch (check 6)
    InvalidAssignment,
    // Invalid condition type (check 7)
    InvalidCondition,
    // Return type mismatch (check 9)
    InvalidReturnType,
    // Function call argument mismatch (check 10)
    InvalidFunctionCall,
    // Invalid array size (check 11)
    InvalidArraySize,
    // Array/scalar usage mismatch (check 12)
    InvalidArrayUsage,
    // Internal compiler errors
    InternalError,
}

#[derive(Debug, Clone)]
pub struct SemError {
    pub kind: SemErrorKind,
    pub location: Location,
    pub message: String,
    pub identifier: Option<String>,
}

impl SemError {
    pub fn new(
        kind: SemErrorKind,
        location: Location,
        message: String,
        identifier: Option<String>,
    ) -> Self {
        SemError {
            kind,
            location,
            message,
            identifier,
        }
    }
    
    pub fn identifier_redefined(location: Location, identifier: String) -> Self {
        SemError::new(
            SemErrorKind::IdentifierRedefined,
            location,
            format!("redefinition of {}", identifier),
            Some(identifier),
        )
    }
    
    pub fn identifier_undefined(location: Location, identifier: String) -> Self {
        SemError::new(
            SemErrorKind::IdentifierUndefined,
            location,
            format!("use of undefined identifier {}", identifier),
            Some(identifier),
        )
    }
    
    pub fn type_mismatch(location: Location, expected: String, found: String) -> Self {
        SemError::new(
            SemErrorKind::TypeMismatch,
            location,
            format!("type mismatch: expected {}, found {}", expected, found),
            None,
        )
    }
    
    pub fn invalid_arithmetic_operands(location: Location, op: String, typ: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidArithmeticOperands,
            location,
            format!("invalid operands to arithmetic operator '{}': cannot use with type {}", op, typ),
            None,
        )
    }
    
    pub fn invalid_logical_operands(location: Location, op: String, left_type: String, right_type: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidLogicalOperands,
            location,
            format!("invalid operands to logical operator '{}': {} and {}", op, left_type, right_type),
            None,
        )
    }
    
    pub fn invalid_assignment(location: Location, lhs_type: String, rhs_type: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidAssignment,
            location,
            format!("invalid assignment: cannot assign {} to {}", rhs_type, lhs_type),
            None,
        )
    }
    
    pub fn invalid_condition(location: Location, found_type: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidCondition,
            location,
            format!("invalid condition in if/while statement: expected bool, found {}", found_type),
            None,
        )
    }
    
    pub fn invalid_return_type(location: Location, expected: String, found: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidReturnType,
            location,
            format!("mismatched return statement: expected {}, found {}", expected, found),
            None,
        )
    }
    
    pub fn invalid_function_call(location: Location, function: String, reason: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidFunctionCall,
            location,
            format!("invalid call to function '{}': {}", function, reason),
            Some(function),
        )
    }
    
    pub fn invalid_array_size(location: Location, identifier: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidArraySize,
            location,
            format!("size cannot be negative for array {}", identifier),
            Some(identifier),
        )
    }
    
    pub fn invalid_array_usage(location: Location, identifier: String, reason: String) -> Self {
        SemError::new(
            SemErrorKind::InvalidArrayUsage,
            location,
            format!("invalid usage of {}: {}", identifier, reason),
            Some(identifier),
        )
    }
    
    pub fn internal_error(location: Location, message: String) -> Self {
        SemError::new(
            SemErrorKind::InternalError,
            location,
            format!("internal compiler error: {}", message),
            None,
        )
    }
}


impl Display for SemError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "sema: {} : {}", self.location, self.message)
    }
}

impl std::error::Error for SemError {}
