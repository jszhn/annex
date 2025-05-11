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

    pub fn from_enum(op: &BinaryOperator) -> String {
        match op {
            BinaryOperator::Add => "+".to_string(),
            BinaryOperator::Sub => "-".to_string(),
            BinaryOperator::Mul => "*".to_string(),
            BinaryOperator::Div => "/".to_string(),
            BinaryOperator::BitAnd => "&".to_string(),
            BinaryOperator::BitOr => "|".to_string(),
            BinaryOperator::BitXor => "^".to_string(),
            BinaryOperator::ShiftLeft => "<<".to_string(),
            BinaryOperator::ShiftRight => ">>".to_string(),
            BinaryOperator::And => "and".to_string(),
            BinaryOperator::Or => "or".to_string(),
            BinaryOperator::Eq => "==".to_string(),
            BinaryOperator::NotEq => "!=".to_string(),
            BinaryOperator::Greater => ">".to_string(),
            BinaryOperator::Less => "<".to_string(),
            BinaryOperator::GreaterEq => ">=".to_string(),
            BinaryOperator::LessEq => "<=".to_string(),
            BinaryOperator::Assign => "=".to_string(),
            BinaryOperator::Array => "[]".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

    pub fn from_enum(op: &UnaryOperator) -> String {
        match op {
            UnaryOperator::Neg => "-".to_string(),
            UnaryOperator::BitNot => "~".to_string(),
            UnaryOperator::Not => "?".to_string(),
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

    pub fn from_enum(typ: &Type) -> String {
        match typ {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::Array(typ_ptr, s) => {
                format!("{}[{}]", Self::from_enum(typ_ptr), s)
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

    pub fn from_enum(sc: &StorageClass) -> String {
        match sc {
            StorageClass::Const => "const".to_string(),
            StorageClass::Var => "var".to_string(),
            StorageClass::Vol => "vol".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
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
