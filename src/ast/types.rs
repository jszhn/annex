use crate::util::UnknownError;

#[derive(Eq, Hash, PartialEq)]
pub enum DType {
    // type bit-val size
    Integer(u8),
    Float(u8),
    Bool,
    Void,
}

impl DType {
    pub fn from_str(val: Option<String>) -> Result<DType, UnknownError> {
        if val.is_none() {
            return Err(UnknownError::new("Unknown type"));
        }

        Ok(match val.unwrap().as_str() {
            "i8" => DType::Integer(8),
            "i16" => DType::Integer(16),
            "i32" => DType::Integer(32),
            "f32" => DType::Float(32),
            "f64" => DType::Float(64),
            "bool" => DType::Bool,
            "void" => DType::Void,
            _ => DType::Void, // this match arm should frankly never be called
        })
    }
}

pub struct CFunction {
    input_vars: Vec<CType>,
    return_var: CType,
    specifiers: Vec<CSpecifier>,
}

pub struct CVariable {
    _type: CType,
    label: String,
    ptr: bool,
    ptr_level: i32, // you wouldn't dare do a 32-nested pointer
    specifier: Vec<CSpecifier>,
}

pub struct CLiteral {
    _type: CType,
    value: String,
}

pub struct CType {
    _type: CTypeKeyword,
    _struct: bool,
}

pub enum CTypeKeyword {
    Int,
    Char,
    Float,
    Double,
    Bool,
    Short,
    Long,
    Custom,
}

pub enum COperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    Exp,
    Deref,
    Access,
    DerefAccess,
    BitshiftRight,
    BitshiftLeft,
    BitOr,
    BitAnd,
    BitXor,
}

pub enum CSpecifier {
    Auto,
    Const,
    Extern,
    Register,
    Unsigned,
    Signed,
    Static,
    Volatile,
}
