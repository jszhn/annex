use crate::ast::util::AstError;
use crate::parse::Parser;

mod util;

struct Ast {
    head: AstNode,
}

impl Ast {
    fn new(parse_tree: Parser) -> Result<Ast, AstError> {
        let parse_head = parse_tree.head;
        Ok(Ast {
            head: convert_parse_tree(parse_head)?,
        })
    }
}

fn convert_parse_tree(parse_node: ParseNode) -> Result<AstNode, AstError> {}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

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

    // Assignment
    Assign,
}

impl BinaryOperator {
    fn from_token(token: &str) -> Option<Self> {
        match token {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "%" => Some(Self::Mod),
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
    fn from_token(token: &str) -> Option<Self> {
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
    fn from_token(token: &str) -> Option<Self> {
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
    fn from_token(token: &str) -> Option<Self> {
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
    Parameter(ParamNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    If(IfNode),
    While(WhileNode),
    For(ForNode),
    Return(Option<Box<AstNode>>),
    Break,
    Continue,
    Block(Vec<AstNode>),
    VarDecl(VarDeclNode),
    ArrDecl(ArrDeclNode),
    Identifier(String),
    Literal(Literal),
    ArrayAccess(ArrAccessNode),
}

struct FunctionNode {
    name: String,
    params: Vec<ParamNode>,
    return_type: Type,
    body: Box<AstNode>,
}

struct ParamNode {
    name: String,
    typ: Type,
}

struct BinaryNode {
    op: BinaryOperator,
    left: Box<AstNode>,
    right: Box<AstNode>,
}

struct UnaryNode {
    op: UnaryOperator,
    expr: Box<AstNode>,
}

struct IfNode {
    condition: Box<AstNode>,
    then_branch: Box<AstNode>,
    elif_branches: Vec<(AstNode, AstNode)>,
    else_branch: Option<Box<AstNode>>,
}

struct WhileNode {
    condition: Box<AstNode>,
    body: Box<AstNode>,
}

struct ForNode {
    init: Box<AstNode>,
    condition: Box<AstNode>,
    update: Box<AstNode>,
    body: Box<AstNode>,
}

struct VarDeclNode {
    storage: StorageClass,
    name: String,
    typ: Type,
    initialiser: Option<Box<AstNode>>,
}

struct ArrDeclNode {
    storage: StorageClass,
    name: String,
    element_type: Type,
    size: Box<AstNode>,
    initialiser: Option<Box<AstNode>>,
}

struct ArrAccessNode {
    array: Box<AstNode>,
    index: Box<AstNode>,
}
