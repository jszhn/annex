use crate::lexer::{Token, TokenType};
use crate::parse::{ParseNode, WhileNode};

pub struct FunctionNode {
    pub name: Token,
    pub return_type: Token,
    pub params: Vec<ParseNode>,
    pub body: Box<ParseNode>,
}

impl FunctionNode {
    pub(super) fn new(name: &Token) -> FunctionNode {
        FunctionNode {
            name: name.clone(),
            return_type: Token::new(TokenType::Type, "void"),
            params: Vec::new(),
            body: Box::new(ParseNode::None),
        }
    }

    pub(super) fn set_body(&mut self, scope: ParseNode) {
        self.body = Box::new(scope);
    }

    pub(super) fn push_param(&mut self, param: ParseNode) {
        self.params.push(param);
    }

    pub(super) fn set_return(&mut self, typ: Token) {
        self.return_type = typ;
    }
}

pub struct FunctionCallNode {
    pub function: Box<ParseNode>,
    pub args: Vec<ParseNode>,
}

impl FunctionCallNode {
    pub(super) fn new(func: ParseNode, args: Vec<ParseNode>) -> FunctionCallNode {
        FunctionCallNode {
            function: Box::new(func),
            args,
        }
    }
}

pub struct BinaryNode {
    pub left: Box<ParseNode>,
    pub op: Token,
    pub right: Box<ParseNode>,
}

impl BinaryNode {
    pub(super) fn new(op: Token, left: ParseNode, right: ParseNode) -> BinaryNode {
        BinaryNode {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

pub struct UnaryNode {
    pub op: Token,
    pub operand: Box<ParseNode>,
}

impl UnaryNode {
    pub(super) fn new(op: Token, operand: ParseNode) -> UnaryNode {
        UnaryNode {
            op,
            operand: Box::new(operand),
        }
    }
}

pub struct Value {
    pub lexeme: String,
}

impl Value {
    pub(super) fn new(val: &String) -> Value {
        Value {
            lexeme: val.clone(),
        }
    }
}

pub struct ScalarDeclNode {
    pub specifier: Token,
    pub _type: Token,
    pub initialiser: Option<Box<ParseNode>>,
    pub id: String,
}

impl ScalarDeclNode {
    pub(super) fn new(
        specifier: Token,
        _type: Token,
        initialiser: Option<ParseNode>,
        id: String,
    ) -> ScalarDeclNode {
        ScalarDeclNode {
            specifier,
            _type,
            initialiser: initialiser.map(|p| Box::new(p)),
            id,
        }
    }
}

pub struct ArrayDeclNode {
    pub specifier: Token,
    pub _type: Token,
    pub size: Box<ParseNode>,
    pub initialiser: Option<Box<ParseNode>>,
    pub id: String,
}

impl ArrayDeclNode {
    pub(super) fn new(
        specifier: Token,
        _type: Token,
        initialiser: Option<ParseNode>,
        size: ParseNode,
        id: String,
    ) -> ArrayDeclNode {
        ArrayDeclNode {
            specifier,
            _type,
            size: Box::new(size),
            initialiser: initialiser.map(|p| Box::new(p)),
            id,
        }
    }
}

pub struct ReturnNode {
    pub expr: Option<Box<ParseNode>>,
}

impl ReturnNode {
    pub(super) fn new(expr: Option<ParseNode>) -> ReturnNode {
        return if expr.is_some() {
            ReturnNode {
                expr: Some(Box::new(expr.unwrap())),
            }
        } else {
            ReturnNode { expr: None }
        };
    }
}

pub struct BasicControlNode {
    pub cond: Box<ParseNode>,
    pub then: Box<ParseNode>,
}

impl BasicControlNode {
    pub(super) fn new(cond: ParseNode, then: ParseNode) -> BasicControlNode {
        BasicControlNode {
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
    pub(super) fn new(cond: ParseNode, then: ParseNode) -> ControlNode {
        ControlNode {
            _if: BasicControlNode::new(cond, then),
            elif: None,
            el: None,
        }
    }

    pub(super) fn push_elif(&mut self, cond: ParseNode, then: ParseNode) {
        self.elif
            .get_or_insert(Vec::new())
            .push(BasicControlNode::new(cond, then))
    }

    pub(super) fn set_else(&mut self, el: ParseNode) {
        self.el = Some(Box::new(el))
    }
}

impl WhileNode {
    pub(super) fn new(cond: ParseNode, then: ParseNode) -> WhileNode {
        WhileNode {
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
    pub(super) fn new(
        pre: ParseNode,
        cond: ParseNode,
        post: ParseNode,
        then: ParseNode,
    ) -> ForNode {
        ForNode {
            pre: Box::new(pre),
            cond: Box::new(cond),
            post: Box::new(post),
            then: Box::new(then),
        }
    }
}

pub struct LoopControlNode {
    pub _type: Token,
}

impl LoopControlNode {
    pub(super) fn new(_type: Token) -> LoopControlNode {
        LoopControlNode { _type }
    }
}

pub struct ScopeNode {
    pub contents: Vec<ParseNode>,
}

impl ScopeNode {
    pub(super) fn new() -> ScopeNode {
        ScopeNode {
            contents: Vec::new(),
        }
    }

    pub(super) fn push_body(&mut self, node: ParseNode) {
        self.contents.push(node)
    }
}
