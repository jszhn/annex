use crate::ast::util::AstError;
use crate::parse::{ConstantNode, ParseNode, Parser, Value};

mod util;

pub struct Ast {
    head: AstNode,
}

impl Ast {
    pub fn new(parse_tree: Parser) -> Result<Ast, AstError> {
        let parse_head = parse_tree.head;
        Ok(Ast {
            head: convert_parse_tree(parse_head)?,
        })
    }

    pub fn get_head_ref(&self) -> &AstNode {
        &self.head
    }
}

fn convert_parse_tree(parse_node: ParseNode) -> Result<AstNode, AstError> {
    match parse_node {
        ParseNode::Function(node) => {
            let name = node.name.lexeme.unwrap_or_default();
            let return_type =
                Type::from_token(node.return_type.lexeme.unwrap_or_default().as_str())
                    .ok_or_else(|| AstError::new("Error: invalid return type"))?;
            let params = node
                .params
                .into_iter()
                .map(|p| convert_parse_tree(p))
                .collect::<Result<Vec<_>, _>>()?;
            let body = convert_parse_tree(*node.body)?;

            let node = FunctionNode {
                name,
                params: params
                    .into_iter()
                    .map(|p| match p {
                        AstNode::VarDecl(VarDeclNode {
                            storage,
                            name,
                            typ,
                            initialiser: _initialiser,
                            ..
                        }) => ParamNode {
                            name,
                            typ,
                            spec: storage,
                            size: None,
                        },
                        AstNode::ArrDecl(ArrDeclNode {
                            storage,
                            name,
                            typ,
                            size,
                            initialiser: _initialiser,
                        }) => ParamNode {
                            name,
                            typ,
                            spec: storage,
                            size: Some(size),
                        },
                        _ => panic!("Error: expected valid parameter node"),
                    })
                    .collect(),
                return_type,
                body: Box::new(body),
            };
            Ok(AstNode::Function(node))
        }
        ParseNode::Binary(node) => {
            let op = BinaryOperator::from_token(&node.op.lexeme.unwrap_or_default())
                .ok_or_else(|| AstError::new("Invalid binary operator"))?;
            let right = convert_parse_tree(*node.right)?;

            match op {
                BinaryOperator::Array => {
                    let name = match *node.left {
                        ParseNode::Value(Value { lexeme }) => lexeme,
                        _ => return Err(AstError::new("Expected non-null array identifier")),
                    };

                    let node = ArrAccessNode {
                        name,
                        access: Box::new(right),
                    };
                    Ok(AstNode::Arr(node))
                }
                _ => {
                    let left = convert_parse_tree(*node.left)?;
                    let node = BinaryNode {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                    Ok(AstNode::Binary(node))
                }
            }
        }
        ParseNode::Unary(node) => {
            let op = UnaryOperator::from_token(&node.op.lexeme.unwrap_or_default())
                .ok_or_else(|| AstError::new("Invalid unary operator"))?;
            let expr = convert_parse_tree(*node.operand)?;

            let node = UnaryNode {
                op,
                expr: Box::new(expr),
            };
            Ok(AstNode::Unary(node))
        }
        ParseNode::Value(node) => Ok(AstNode::Identifier(node.lexeme)),
        ParseNode::Constant(constant) => {
            let literal = match constant {
                ConstantNode::Bool(b) => Literal::Bool(b),
                ConstantNode::Float(f) => Literal::Float(f),
                ConstantNode::Int(i) => Literal::Int(i),
            };
            Ok(AstNode::Literal(literal))
        }
        ParseNode::ScalarDecl(node) => {
            let storage = StorageClass::from_token(&node.specifier.lexeme.unwrap_or_default())
                .ok_or_else(|| AstError::new("Invalid storage class"))?;
            let typ = Type::from_token(&node._type.lexeme.unwrap_or_default())
                .ok_or_else(|| AstError::new("Invalid type"))?;

            let node = VarDeclNode {
                storage,
                name: node.id,
                typ,
                initialiser: None,
            };
            Ok(AstNode::VarDecl(node))
        }
        ParseNode::ArrDecl(node) => {
            let storage = StorageClass::from_token(&node.specifier.lexeme.unwrap_or_default())
                .ok_or_else(|| AstError::new("Invalid storage class"))?;
            let typ = Type::from_token(&node._type.lexeme.unwrap_or_default())
                .ok_or_else(|| AstError::new("Invalid type"))?;
            let size = convert_parse_tree(*node.size)?;

            let node = ArrDeclNode {
                storage,
                name: node.id,
                typ,
                size: Box::new(size),
                initialiser: None,
            };
            Ok(AstNode::ArrDecl(node))
        }
        ParseNode::Return(node) => Ok(if node.expr.is_some() {
            AstNode::Return(node.expr.map(|e| Box::new(convert_parse_tree(*e).unwrap())))
        } else {
            AstNode::Return(None)
        }),
        ParseNode::Control(node) => {
            let cond = convert_parse_tree(*node._if.cond)?;
            let then = convert_parse_tree(*node._if.then)?;
            let elif = if let Some(elifs) = node.elif {
                elifs
                    .into_iter()
                    .map(|elif| {
                        Ok((
                            convert_parse_tree(*elif.cond)?,
                            convert_parse_tree(*elif.then)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, AstError>>()?
            } else {
                vec![]
            };
            let el = node.el.map(|e| Box::new(convert_parse_tree(*e).unwrap()));

            let node = IfNode {
                condition: Box::new(cond),
                then_branch: Box::new(then),
                elif_branches: elif,
                else_branch: el,
            };
            Ok(AstNode::If(node))
        }
        ParseNode::While(node) => {
            let cond = convert_parse_tree(*node.cond)?;
            let body = convert_parse_tree(*node.then)?;

            let node = WhileNode {
                condition: Box::new(cond),
                body: Box::new(body),
            };
            Ok(AstNode::While(node))
        }
        ParseNode::For(node) => {
            let init = convert_parse_tree(*node.pre)?;
            let cond = convert_parse_tree(*node.cond)?;
            let upd = convert_parse_tree(*node.post)?;
            let body = convert_parse_tree(*node.then)?;

            let node = ForNode {
                init: Box::new(init),
                condition: Box::new(cond),
                update: Box::new(upd),
                body: Box::new(body),
            };
            Ok(AstNode::For(node))
        }
        ParseNode::Scope(node) => {
            let stmts = node
                .contents
                .into_iter()
                .map(convert_parse_tree)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(AstNode::Block(BlockNode { elems: stmts }))
        }
        ParseNode::LoopControl(node) => match node._type.lexeme.as_deref() {
            Some("continue") => Ok(AstNode::Continue),
            Some("break") => Ok(AstNode::Break),
            _ => Err(AstError::new("Invalid loop control lexeme")),
        },
        ParseNode::FunctionCall(node) => {
            let name = match *node.function {
                ParseNode::Value(Value { lexeme }) => lexeme,
                _ => {
                    return Err(AstError::new(
                        "Expected non-null identifier for function call name",
                    ))
                }
            };
            let args = node
                .args
                .into_iter()
                .map(convert_parse_tree)
                .collect::<Result<Vec<_>, _>>()?;

            let node = FunctionCallNode { name, args };
            Ok(AstNode::FunctionCall(node))
        }
        ParseNode::None => Err(AstError::new(
            "Encountered unexpected None parse node type. Unable to convert",
        )),
    }
}

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

    // Miscellaneous
    Assign,
    Array,
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
    name: String,
    args: Vec<AstNode>,
}

pub struct ParamNode {
    name: String,
    typ: Type,
    spec: StorageClass,
    size: Option<Box<AstNode>>,
}

pub struct BinaryNode {
    op: BinaryOperator,
    left: Box<AstNode>,
    right: Box<AstNode>,
}

pub struct UnaryNode {
    op: UnaryOperator,
    expr: Box<AstNode>,
}

pub struct IfNode {
    condition: Box<AstNode>,
    then_branch: Box<AstNode>,
    elif_branches: Vec<(AstNode, AstNode)>,
    else_branch: Option<Box<AstNode>>,
}

pub struct WhileNode {
    condition: Box<AstNode>,
    body: Box<AstNode>,
}

pub struct ForNode {
    init: Box<AstNode>,
    condition: Box<AstNode>,
    update: Box<AstNode>,
    body: Box<AstNode>,
}

pub struct BlockNode {
    elems: Vec<AstNode>,
}

impl BlockNode {
    pub fn get_elems_ref(&self) -> &Vec<AstNode> {
        &self.elems
    }
}

pub struct VarDeclNode {
    storage: StorageClass,
    name: String,
    typ: Type,
    initialiser: Option<Box<AstNode>>,
}

pub struct ArrDeclNode {
    storage: StorageClass,
    name: String,
    typ: Type,
    size: Box<AstNode>,
    initialiser: Option<Box<AstNode>>,
}

pub struct ArrAccessNode {
    name: String,
    access: Box<AstNode>,
}
