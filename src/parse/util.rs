use std::fmt::{Debug, Display, Formatter};

use crate::parse::{ConstantNode, ParseNode, Parser};

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

impl Parser {
    pub fn print(&self, stdout: bool) -> String {
        let result = self.head.print();
        if stdout {
            print!("{}", &result);
        }
        result
    }
}

impl ParseNode {
    fn print(&self) -> String {
        self.print_with_indent(0)
    }

    fn print_with_indent(&self, indent: usize) -> String {
        let mut output = String::new();
        let indent_str = "  ".repeat(indent);

        match self {
            ParseNode::Function(node) => {
                output.push_str(&format!(
                    "{}Function: {}\n",
                    indent_str,
                    node.name.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!(
                    "{}Return type: {}\n",
                    indent_str,
                    node.return_type.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!("{}Parameters:\n", indent_str));
                for param in &node.params {
                    output.push_str(&param.print_with_indent(indent + 1));
                }
                output.push_str(&format!("{}Body:\n", indent_str));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            ParseNode::FunctionCall(node) => {
                output.push_str(&format!("{}Function call:\n", indent_str));
                output.push_str(&node.function.print_with_indent(indent + 1));
                for arg in &node.args {
                    output.push_str(&arg.print_with_indent(indent + 1));
                }
            }
            ParseNode::Binary(node) => {
                output.push_str(&format!("{}Binary Operation:\n", indent_str));
                output.push_str(&format!(
                    "{}Operator: {}\n",
                    indent_str,
                    node.op.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!("{}Left:\n", indent_str));
                output.push_str(&node.left.print_with_indent(indent + 1));
                output.push_str(&format!("{}Right:\n", indent_str));
                output.push_str(&node.right.print_with_indent(indent + 1));
            }
            ParseNode::Unary(node) => {
                output.push_str(&format!("{}Unary Operation:\n", indent_str));
                output.push_str(&format!(
                    "{}Operator: {}\n",
                    indent_str,
                    node.op.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!("{}Operand:\n", indent_str));
                output.push_str(&node.operand.print_with_indent(indent + 1));
            }
            ParseNode::Value(node) => {
                output.push_str(&format!("{}Value: {}\n", indent_str, node.lexeme));
            }
            ParseNode::ScalarDecl(node) => {
                output.push_str(&format!("{}Scalar Declaration:\n", indent_str));
                let inner_indent = format!("{}  ", indent_str);
                output.push_str(&format!(
                    "{}Specifier: {}\n",
                    inner_indent,
                    node.specifier.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!(
                    "{}Type: {}\n",
                    inner_indent,
                    node._type.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!("{}Identifier: {}\n", inner_indent, node.id));
            }
            ParseNode::ArrDecl(node) => {
                output.push_str(&format!("{}Array Declaration:\n", indent_str));
                let inner_indent = format!("{}  ", indent_str);
                output.push_str(&format!(
                    "{}Specifier: {}\n",
                    inner_indent,
                    node.specifier.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!(
                    "{}Type: {}\n",
                    inner_indent,
                    node._type.lexeme.as_ref().unwrap()
                ));
                output.push_str(&format!("{}Size:\n", inner_indent));
                output.push_str(&node.size.print_with_indent(indent + 2));
                output.push_str(&format!("{}Identifier: {}\n", inner_indent, node.id));
            }
            ParseNode::Return(node) => {
                output.push_str(&format!("{}Return:\n", indent_str));
                if let Some(expr) = &node.expr {
                    output.push_str(&expr.print_with_indent(indent + 1));
                } else {
                    output.push_str(&format!("{}void\n", indent_str));
                }
            }
            ParseNode::Control(node) => {
                output.push_str(&format!("{}If Statement:\n", indent_str));
                output.push_str(&format!("{}Condition:\n", indent_str));
                output.push_str(&node._if.cond.print_with_indent(indent + 1));
                output.push_str(&format!("{}Then:\n", indent_str));
                output.push_str(&node._if.then.print_with_indent(indent + 1));

                if let Some(elif_branches) = &node.elif {
                    output.push_str(&format!("{}Elif Branches:\n", indent_str));
                    for elif in elif_branches {
                        output.push_str(&format!("{}Condition:\n", indent_str));
                        output.push_str(&elif.cond.print_with_indent(indent + 1));
                        output.push_str(&format!("{}Then:\n", indent_str));
                        output.push_str(&elif.then.print_with_indent(indent + 1));
                    }
                }

                if let Some(else_branch) = &node.el {
                    output.push_str(&format!("{}Else:\n", indent_str));
                    output.push_str(&else_branch.print_with_indent(indent + 1));
                }
            }
            ParseNode::While(node) => {
                output.push_str(&format!("{}While Loop:\n", indent_str));
                output.push_str(&format!("{}Condition:\n", indent_str));
                output.push_str(&node.cond.print_with_indent(indent + 1));
                output.push_str(&format!("{}Body:\n", indent_str));
                output.push_str(&node.then.print_with_indent(indent + 1));
            }
            ParseNode::For(node) => {
                output.push_str(&format!("{}For Loop:\n", indent_str));
                output.push_str(&format!("{}Initialization:\n", indent_str));
                output.push_str(&node.pre.print_with_indent(indent + 1));
                output.push_str(&format!("{}Condition:\n", indent_str));
                output.push_str(&node.cond.print_with_indent(indent + 1));
                output.push_str(&format!("{}Update:\n", indent_str));
                output.push_str(&node.post.print_with_indent(indent + 1));
                output.push_str(&format!("{}Body:\n", indent_str));
                output.push_str(&node.then.print_with_indent(indent + 1));
            }
            ParseNode::Scope(node) => {
                output.push_str(&format!("{}Scope:\n", indent_str));
                for stmt in &node.contents {
                    output.push_str(&stmt.print_with_indent(indent + 1));
                }
            }
            ParseNode::LoopControl(node) => {
                output.push_str(&format!(
                    "{}Loop Control: {}\n",
                    indent_str,
                    node._type.lexeme.as_ref().unwrap()
                ));
            }
            ParseNode::Constant(constant) => match constant {
                ConstantNode::Bool(b) => {
                    output.push_str(&format!("{}Constant: bool({})\n", indent_str, b))
                }
                ConstantNode::Int(i) => {
                    output.push_str(&format!("{}Constant: int({})\n", indent_str, i))
                }
                ConstantNode::Float(f) => {
                    output.push_str(&format!("{}Constant: float({})\n", indent_str, f))
                }
            },
            ParseNode::None => {
                output.push_str(&format!("{}None\n", indent_str));
            }
        }

        output
    }
}
