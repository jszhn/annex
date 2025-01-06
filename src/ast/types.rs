#[derive(Debug, Clone)]
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

impl BinaryOperator {
    pub fn from_token(token: &str) -> Option<Self> {
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

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,    // -
    BitNot, // ~
    Not,    // !
}

impl UnaryOperator {
    pub fn from_token(token: &str) -> Option<Self> {
        match token {
            "-" => Some(Self::Neg),
            "~" => Some(Self::BitNot),
            "?" => Some(Self::Not),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
}

pub enum AstNode {
    Program(Vec<AstNode>),
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

pub struct FunctionNode {
    pub name: String,
    pub params: Vec<ParamNode>,
    pub return_type: Type,
    pub body: Box<AstNode>,
}

pub struct FunctionCallNode {
    pub name: String,
    pub args: Vec<AstNode>,
}

pub struct ParamNode {
    pub name: String,
    pub typ: Type,
    pub spec: StorageClass,
    pub size: Option<Box<AstNode>>,
}

pub struct BinaryNode {
    pub op: BinaryOperator,
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
}

pub struct UnaryNode {
    pub op: UnaryOperator,
    pub expr: Box<AstNode>,
}

pub struct IfNode {
    pub condition: Box<AstNode>,
    pub then_branch: Box<AstNode>,
    pub elif_branches: Vec<(AstNode, AstNode)>,
    pub else_branch: Option<Box<AstNode>>,
}

pub struct WhileNode {
    pub condition: Box<AstNode>,
    pub body: Box<AstNode>,
}

pub struct ForNode {
    pub init: Box<AstNode>,
    pub condition: Box<AstNode>,
    pub update: Box<AstNode>,
    pub body: Box<AstNode>,
}

pub struct BlockNode {
    pub elems: Vec<AstNode>,
}

impl BlockNode {
    pub fn get_elems_ref(&self) -> &Vec<AstNode> {
        &self.elems
    }
}

pub struct VarDeclNode {
    pub storage: StorageClass,
    pub name: String,
    pub typ: Type,
    pub initialiser: Option<Box<AstNode>>,
}

pub struct ArrDeclNode {
    pub storage: StorageClass,
    pub name: String,
    pub typ: Type,
    pub size: Box<AstNode>,
    pub initialiser: Option<Box<AstNode>>,
}

pub struct ArrAccessNode {
    pub name: String,
    pub access: Box<AstNode>,
}
