use std::fmt::{Debug, Display, Formatter};

use crate::parse::{ConstantNode, ParseNode};

pub struct ParserError {
    message: String,
}

impl ParserError {
    pub fn new(msg: &str) -> ParserError {
        ParserError {
            message: msg.to_string(),
        }
    }
}

impl Debug for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParserError")
            .field("message", &self.message)
            .finish()
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParserError {}

impl ParseNode {
    pub fn print(&self) {
        self.print_with_indent(0);
    }

    fn print_with_indent(&self, indent: usize) {
        let indent_str = "  ".repeat(indent);

        match self {
            ParseNode::Function(node) => {
                println!(
                    "{}Function: {}",
                    indent_str,
                    node.name.lexeme.as_ref().unwrap()
                );
                println!("{}Parameters:", indent_str);
                for param in &node.params {
                    param.print_with_indent(indent + 1);
                }
                println!("{}Body:", indent_str);
                node.body.print_with_indent(indent + 1);
            }
            ParseNode::FunctionCall(node) => {
                println!("{}Function call:", indent_str);
                node.function.print_with_indent(indent + 1);
                for arg in &node.args {
                    arg.print_with_indent(indent + 1);
                }
            }
            ParseNode::Binary(node) => {
                println!("{}Binary Operation:", indent_str);
                println!(
                    "{}Operator: {}",
                    indent_str,
                    node.op.lexeme.as_ref().unwrap()
                );
                println!("{}Left:", indent_str);
                node.left.print_with_indent(indent + 1);
                println!("{}Right:", indent_str);
                node.right.print_with_indent(indent + 1);
            }
            ParseNode::Unary(node) => {
                println!("{}Unary Operation:", indent_str);
                println!(
                    "{}Operator: {}",
                    indent_str,
                    node.op.lexeme.as_ref().unwrap()
                );
                println!("{}Operand:", indent_str);
                node.operand.print_with_indent(indent + 1);
            }
            ParseNode::Value(node) => {
                println!("{}Value: {}", indent_str, node.lexeme);
            }
            ParseNode::ScalarDecl(node) => {
                println!("{}Scalar Declaration:", indent_str);
                println!(
                    "{}Specifier: {}",
                    indent_str,
                    node.specifier.lexeme.as_ref().unwrap()
                );
                println!(
                    "{}Type: {}",
                    indent_str,
                    node._type.lexeme.as_ref().unwrap()
                );
                println!("{}Identifier: {}", indent_str, node.id);
            }
            ParseNode::ArrDecl(node) => {
                println!("{}Array Declaration:", indent_str);
                println!(
                    "{}Specifier: {}",
                    indent_str,
                    node.specifier.lexeme.as_ref().unwrap()
                );
                println!(
                    "{}Type: {}",
                    indent_str,
                    node._type.lexeme.as_ref().unwrap()
                );
                println!("{}Size:", indent_str);
                node.size.print_with_indent(indent + 1);
                println!("{}Identifier: {}", indent_str, node.id);
            }
            ParseNode::Expr(node) => {
                println!("{}Expression: {}", indent_str, node.lexeme);
                for child in &node.children {
                    child.print_with_indent(indent + 1);
                }
            }
            ParseNode::Return(node) => {
                println!("{}Return:", indent_str);
                if let Some(expr) = &node.expr {
                    expr.print_with_indent(indent + 1);
                } else {
                    println!("{}void", indent_str);
                }
            }
            ParseNode::Control(node) => {
                println!("{}If Statement:", indent_str);
                println!("{}Condition:", indent_str);
                node._if.cond.print_with_indent(indent + 1);
                println!("{}Then:", indent_str);
                node._if.then.print_with_indent(indent + 1);

                if let Some(elif_branches) = &node.elif {
                    println!("{}Elif Branches:", indent_str);
                    for elif in elif_branches {
                        println!("{}Condition:", indent_str);
                        elif.cond.print_with_indent(indent + 1);
                        println!("{}Then:", indent_str);
                        elif.then.print_with_indent(indent + 1);
                    }
                }

                if let Some(else_branch) = &node.el {
                    println!("{}Else:", indent_str);
                    else_branch.print_with_indent(indent + 1);
                }
            }
            ParseNode::While(node) => {
                println!("{}While Loop:", indent_str);
                println!("{}Condition:", indent_str);
                node.cond.print_with_indent(indent + 1);
                println!("{}Body:", indent_str);
                node.then.print_with_indent(indent + 1);
            }
            ParseNode::For(node) => {
                println!("{}For Loop:", indent_str);
                println!("{}Initialization:", indent_str);
                node.pre.print_with_indent(indent + 1);
                println!("{}Condition:", indent_str);
                node.cond.print_with_indent(indent + 1);
                println!("{}Update:", indent_str);
                node.post.print_with_indent(indent + 1);
                println!("{}Body:", indent_str);
                node.then.print_with_indent(indent + 1);
            }
            ParseNode::Scope(node) => {
                println!("{}Scope:", indent_str);
                for stmt in &node.contents {
                    stmt.print_with_indent(indent + 1);
                }
            }
            ParseNode::LoopControl(node) => {
                println!(
                    "{}Loop Control: {}",
                    indent_str,
                    &node._type.lexeme.as_ref().unwrap()
                );
            }
            ParseNode::Constant(constant) => match constant {
                ConstantNode::Bool(b) => println!("{}Constant: bool({})", indent_str, b),
                ConstantNode::Int(i) => println!("{}Constant: int({})", indent_str, i),
                ConstantNode::Float(f) => println!("{}Constant: float({})", indent_str, f),
            },
            ParseNode::None => {
                println!("{}None", indent_str);
            }
        }
    }
}
