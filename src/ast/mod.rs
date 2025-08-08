use crate::ast::oper::*;
use crate::ast::types::*;
use crate::ast::util::AstError;
use crate::parse::types::{ConstantNode, ParseNode, Value};
use crate::parse::ParseTree;

pub mod oper;
pub mod types;
mod util;

pub struct Ast {
    head: AstNode,
}

impl Ast {
    pub fn new(parse_tree: ParseTree) -> Result<Ast, AstError> {
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
            let name = node.name;
            let return_type = Type::from_token(node.return_type.as_str())
                .ok_or_else(|| AstError::new("Error: invalid return type"))?;
            let params = node
                .params
                .into_iter()
                .map(convert_parse_tree)
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
            let op = BinaryOperator::from_token(&node.op)
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
            let op = UnaryOperator::from_token(&node.op)
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
            let storage = StorageClass::from_token(&node.specifier.to_string())
                .ok_or_else(|| AstError::new("Invalid storage class"))?;
            let typ = Type::from_token(&node.typ).ok_or_else(|| AstError::new("Invalid type"))?;
            let init = match node.initialiser {
                Some(n) => Some(Box::new(convert_parse_tree(*n)?)),
                None => None,
            };

            let node = VarDeclNode {
                storage,
                name: node.id,
                typ,
                initialiser: init,
            };
            Ok(AstNode::VarDecl(node))
        }
        ParseNode::ArrDecl(node) => {
            let storage = StorageClass::from_token(&node.specifier.to_string())
                .ok_or_else(|| AstError::new("Invalid storage class"))?;
            let typ = Type::from_token(&node.typ).ok_or_else(|| AstError::new("Invalid type"))?;
            let size = convert_parse_tree(*node.size)?;
            let _ = match node.initialiser {
                Some(node) => Some(Box::new(convert_parse_tree(*node)?)),
                None => None,
            };

            let node = ArrDeclNode {
                storage,
                name: node.id,
                typ,
                size: Box::new(size),
                initialiser: None, // replace with init once I figure out how to pull this off
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
            let elif = if let Some(ctl) = node.elif {
                ctl.into_iter()
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
        ParseNode::LoopControl(node) => match node.typ.typ.to_string().as_str() {
            "continue" => Ok(AstNode::Continue),
            "break" => Ok(AstNode::Break),
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

            let node = FunctionCallNode {
                name,
                args,
                return_type: None,
            };
            Ok(AstNode::FunctionCall(node))
        }
        ParseNode::None => Err(AstError::new(
            "Encountered unexpected None parse node type. Unable to convert",
        )),
    }
}
