use crate::lexer::Token;
use crate::parse::util::ParserError;

pub enum ParseNode {
    Constant(ConstantNode),
    Function(FunctionNode),
    FunctionCall(FunctionCallNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Value(Value),
    ScalarDecl(ScalarDeclNode),
    ArrDecl(ArrayDeclNode),
    Return(ReturnNode),
    Control(ControlNode),
    While(WhileNode),
    For(ForNode),
    LoopControl(LoopControlNode), // break, continue
    Scope(ScopeNode),
    None,
}

impl ParseNode {
    pub fn push_body(&mut self, node: ParseNode) -> Result<(), ParserError> {
        match self {
            ParseNode::Scope(n) => n.push_body(node),
            _ => {
                return Err(ParserError::new(
                    "Internal error: unexpected parser node type.",
                ))
            }
        }
        Ok(())
    }
}

/// Constant node. Stores value
pub enum ConstantNode {
    Bool(bool),
    Int(u64),
    Float(f64),
}
pub struct FunctionNode {
    pub name: String,
    pub return_type: String,
    pub params: Vec<ParseNode>,
    pub body: Box<ParseNode>,
}

impl FunctionNode {
    pub fn new(name: String, return_type: String, params: Vec<ParseNode>, body: ParseNode) -> Self {
        Self {
            name,
            return_type,
            params,
            body: Box::new(body),
        }
    }
}

pub struct FunctionCallNode {
    pub function: Box<ParseNode>,
    pub args: Vec<ParseNode>,
}

impl FunctionCallNode {
    pub fn new(func: ParseNode, args: Vec<ParseNode>) -> Self {
        Self {
            function: Box::new(func),
            args,
        }
    }
}

pub struct BinaryNode {
    pub left: Box<ParseNode>,
    pub op: String,
    pub right: Box<ParseNode>,
}

impl BinaryNode {
    pub fn new(op: String, left: ParseNode, right: ParseNode) -> Self {
        Self {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

pub struct UnaryNode {
    pub op: String,
    pub operand: Box<ParseNode>,
}

impl UnaryNode {
    pub fn new(op: String, operand: ParseNode) -> Self {
        Self {
            op,
            operand: Box::new(operand),
        }
    }
}

pub struct Value {
    pub lexeme: String,
}

impl Value {
    pub fn new(val: &str) -> Self {
        Self {
            lexeme: val.to_string(),
        }
    }
}

pub struct ScalarDeclNode {
    pub specifier: Token,
    pub typ: String,
    pub initialiser: Option<Box<ParseNode>>,
    pub id: String,
}

impl ScalarDeclNode {
    pub fn new(specifier: Token, typ: String, initialiser: Option<ParseNode>, id: String) -> Self {
        Self {
            specifier,
            typ,
            initialiser: initialiser.map(Box::new),
            id,
        }
    }
}

pub struct ArrayDeclNode {
    pub specifier: Token,
    pub typ: String,
    pub size: Box<ParseNode>,
    pub initialiser: Option<Box<ParseNode>>,
    pub id: String,
}

impl ArrayDeclNode {
    pub fn new(
        specifier: Token,
        typ: String,
        initialiser: Option<ParseNode>,
        size: ParseNode,
        id: String,
    ) -> Self {
        Self {
            specifier,
            typ,
            size: Box::new(size),
            initialiser: initialiser.map(Box::new),
            id,
        }
    }
}

pub struct ReturnNode {
    pub expr: Option<Box<ParseNode>>,
}

impl ReturnNode {
    pub fn new(expr: Option<ParseNode>) -> Self {
        Self {
            expr: expr.map(Box::new),
        }
    }
}

pub struct BasicControlNode {
    pub cond: Box<ParseNode>,
    pub then: Box<ParseNode>,
}

impl BasicControlNode {
    pub fn new(cond: ParseNode, then: ParseNode) -> Self {
        Self {
            cond: Box::new(cond),
            then: Box::new(then),
        }
    }
}

pub struct ControlNode {
    pub _if: BasicControlNode,
    pub elif: Option<Vec<BasicControlNode>>,
    pub el: Option<Box<ParseNode>>,
}

impl ControlNode {
    pub fn new(cond: ParseNode, then: ParseNode) -> Self {
        Self {
            _if: BasicControlNode::new(cond, then),
            elif: None,
            el: None,
        }
    }

    pub fn push_elif(&mut self, cond: ParseNode, then: ParseNode) {
        self.elif
            .get_or_insert(Vec::new())
            .push(BasicControlNode::new(cond, then))
    }

    pub fn set_else(&mut self, el: ParseNode) {
        self.el = Some(Box::new(el))
    }
}

pub struct WhileNode {
    pub cond: Box<ParseNode>,
    pub then: Box<ParseNode>,
}

impl WhileNode {
    pub fn new(cond: ParseNode, then: ParseNode) -> Self {
        Self {
            cond: Box::new(cond),
            then: Box::new(then),
        }
    }
}

pub struct ForNode {
    pub pre: Box<ParseNode>,
    pub cond: Box<ParseNode>,
    pub post: Box<ParseNode>,
    pub then: Box<ParseNode>,
}

impl ForNode {
    pub fn new(pre: ParseNode, cond: ParseNode, post: ParseNode, then: ParseNode) -> Self {
        Self {
            pre: Box::new(pre),
            cond: Box::new(cond),
            post: Box::new(post),
            then: Box::new(then),
        }
    }
}

pub struct LoopControlNode {
    pub typ: Token,
}

impl LoopControlNode {
    pub fn new(typ: Token) -> Self {
        Self { typ }
    }
}

pub struct ScopeNode {
    pub contents: Vec<ParseNode>,
}

impl ScopeNode {
    pub fn new() -> Self {
        Self {
            contents: Vec::new(),
        }
    }

    pub fn push_body(&mut self, node: ParseNode) {
        self.contents.push(node)
    }
}
