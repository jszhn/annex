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
