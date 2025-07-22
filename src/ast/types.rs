pub trait Operator {
    fn from_token(token: &str) -> Option<Self>
    where
        Self: Sized + std::fmt::Display;
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    // Logical
    And,
    Or,

    // Comparison
    Eq,
    NotEq,
    Greater,
    Less,
    GreaterEq,
    LessEq,

    // Miscellaneous
    Assign,
    Array,
}

impl Operator for BinaryOperator {
    fn from_token(token: &str) -> Option<Self> {
        match token {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "&" => Some(Self::BitAnd),
            "|" => Some(Self::BitOr),
            "^" => Some(Self::BitXor),
            "<<" => Some(Self::ShiftLeft),
            ">>" => Some(Self::ShiftRight),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "=" => Some(Self::Assign),
            "==" => Some(Self::Eq),
            "!=" => Some(Self::NotEq),
            ">" => Some(Self::Greater),
            "<" => Some(Self::Less),
            ">=" => Some(Self::GreaterEq),
            "<=" => Some(Self::LessEq),
            "[]" => Some(Self::Array),
            _ => None,
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::BitAnd => write!(f, "&"),
            BinaryOperator::BitOr => write!(f, "|"),
            BinaryOperator::BitXor => write!(f, "^"),
            BinaryOperator::ShiftLeft => write!(f, "<<"),
            BinaryOperator::ShiftRight => write!(f, ">>"),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Or => write!(f, "or"),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::NotEq => write!(f, "!="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::GreaterEq => write!(f, ">="),
            BinaryOperator::LessEq => write!(f, "<="),
            BinaryOperator::Assign => write!(f, "="),
            BinaryOperator::Array => write!(f, "[]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Neg,    // -
    BitNot, // ~
    Not,    // !
}

impl Operator for UnaryOperator {
    fn from_token(token: &str) -> Option<Self> {
        match token {
            "-" => Some(Self::Neg),
            "~" => Some(Self::BitNot),
            "!" => Some(Self::Not),
            _ => None,
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::BitNot => write!(f, "~"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Void,
    Array(Box<Type>, usize),
}

impl Type {
    pub fn from_token(token: &str) -> Option<Self> {
        match token {
            "i8" => Some(Self::I8),
            "i16" => Some(Self::I16),
            "i32" => Some(Self::I32),
            "i64" => Some(Self::I64),
            "u8" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            "f32" => Some(Self::F32),
            "f64" => Some(Self::F64),
            "bool" => Some(Self::Bool),
            "void" => Some(Self::Void),
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Array(typ_ptr, s) => {
                write!(f, "{typ_ptr}[{s}]")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StorageClass {
    Const,
    Var,
    Vol,
}

impl StorageClass {
    pub fn from_token(token: &str) -> Option<Self> {
        match token {
            "const" => Some(Self::Const),
            "var" => Some(Self::Var),
            "vol" => Some(Self::Vol),
            _ => None,
        }
    }
}

impl std::fmt::Display for StorageClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            StorageClass::Const => write!(f, "const"),
            StorageClass::Var => write!(f, "var"),
            StorageClass::Vol => write!(f, "vol"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(u64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, PartialEq)]
pub enum AstNode {
    Function(FunctionNode),
    FunctionCall(FunctionCallNode),
    Parameter(ParamNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    If(IfNode),
    While(WhileNode),
    For(ForNode),
    Return(Option<Box<AstNode>>),
    Break,
    Continue,
    Block(BlockNode),
    VarDecl(VarDeclNode),
    ArrDecl(ArrDeclNode),
    Identifier(String),
    Literal(Literal),
    Arr(ArrAccessNode),
}

#[derive(Clone, PartialEq)]
pub struct FunctionNode {
    pub name: String,
    pub params: Vec<ParamNode>,
    pub return_type: Type,
    pub body: Box<AstNode>,
}

#[derive(Clone, PartialEq)]
pub struct FunctionCallNode {
    pub name: String,
    pub args: Vec<AstNode>,
    pub return_type: Option<Type>,
}

#[derive(Clone, PartialEq)]
pub struct ParamNode {
    pub name: String,
    pub typ: Type,
    pub spec: StorageClass,
    pub size: Option<Box<AstNode>>,
}

#[derive(Clone, PartialEq)]
pub struct BinaryNode {
    pub op: BinaryOperator,
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
}

#[derive(Clone, PartialEq)]
pub struct UnaryNode {
    pub op: UnaryOperator,
    pub expr: Box<AstNode>,
}

#[derive(Clone, PartialEq)]
pub struct IfNode {
    pub condition: Box<AstNode>,
    pub then_branch: Box<AstNode>,
    pub elif_branches: Vec<(AstNode, AstNode)>, // condition, body
    pub else_branch: Option<Box<AstNode>>,
}

#[derive(Clone, PartialEq)]
pub struct WhileNode {
    pub condition: Box<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Clone, PartialEq)]
pub struct ForNode {
    pub init: Box<AstNode>,
    pub condition: Box<AstNode>,
    pub update: Box<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Clone, PartialEq)]
pub struct BlockNode {
    pub elems: Vec<AstNode>,
}

impl BlockNode {
    pub fn get_elems_ref(&self) -> &Vec<AstNode> {
        &self.elems
    }
}

#[derive(Clone, PartialEq)]
pub struct VarDeclNode {
    pub storage: StorageClass,
    pub name: String,
    pub typ: Type,
    pub initialiser: Option<Box<AstNode>>,
}

#[derive(Clone, PartialEq)]
pub struct ArrDeclNode {
    pub storage: StorageClass,
    pub name: String,
    pub typ: Type,
    pub size: Box<AstNode>,
    pub initialiser: Option<Box<AstNode>>,
}

#[derive(Clone, PartialEq)]
pub struct ArrAccessNode {
    pub name: String,
    pub access: Box<AstNode>,
}
