use std::fmt::{Debug, Display, Formatter};

use crate::parse::structs::ParseNode;
use crate::parse::{ConstantNode, ParseTree};

/// Error type for parser operations.
/// Contains detailed information about parsing failures.
pub struct ParserError {
    message: String,
    kind: ErrorKind,
}

impl ParserError {
    pub fn new(msg: &str) -> Self {
        Self {
            message: msg.to_string(),
            kind: ErrorKind::SyntaxError,
        }
    }

    pub fn with_kind(msg: &str, kind: ErrorKind) -> Self {
        Self {
            message: msg.to_string(),
            kind,
        }
    }

    pub fn unexpected_token(expected: &str, found: &str) -> Self {
        Self {
            message: format!("Expected {}, but found {}", expected, found),
            kind: ErrorKind::UnexpectedToken,
        }
    }

    pub fn unexpected_token_stmt(expected: &str, stmt: &str) -> Self {
        Self {
            message: format!("Expected {} for {} statement", expected, stmt),
            kind: ErrorKind::UnexpectedToken,
        }
    }

    pub fn missing_token(token: &str) -> Self {
        Self {
            message: format!("Expected {}, but not found", token),
            kind: ErrorKind::MissingToken,
        }
    }

    pub fn illegal_statement(msg: &str) -> Self {
        Self {
            message: msg.to_string(),
            kind: ErrorKind::IllegalStatement,
        }
    }

    pub fn internal_error(msg: &str) -> Self {
        Self {
            message: msg.to_string(),
            kind: ErrorKind::InternalError,
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

impl Debug for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParserError")
            .field("message", &self.message)
            .field("kind", &self.kind)
            .finish()
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)
    }
}

impl std::error::Error for ParserError {}

/// Categorization of parser errors for better error handling
#[derive(Debug)]
pub enum ErrorKind {
    SyntaxError,
    UnexpectedToken,
    MissingToken,
    InvalidExpression,
    IllegalStatement,
    InvalidFunction,
    InternalError,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::SyntaxError => write!(f, "SyntaxError"),
            ErrorKind::UnexpectedToken => write!(f, "UnexpectedToken"),
            ErrorKind::MissingToken => write!(f, "MissingToken"),
            ErrorKind::InvalidExpression => write!(f, "InvalidExpression"),
            ErrorKind::IllegalStatement => write!(f, "IllegalStatement"),
            ErrorKind::InvalidFunction => write!(f, "InvalidFunction"),
            ErrorKind::InternalError => write!(f, "InternalError"),
        }
    }
}

impl ParseTree {
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
                output.push_str(&format!("{}Function: {}\n", indent_str, node.name));
                output.push_str(&format!(
                    "{}Return type: {}\n",
                    indent_str, node.return_type
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
                output.push_str(&format!("{}Operator: {}\n", indent_str, node.op));
                output.push_str(&format!("{}Left:\n", indent_str));
                output.push_str(&node.left.print_with_indent(indent + 1));
                output.push_str(&format!("{}Right:\n", indent_str));
                output.push_str(&node.right.print_with_indent(indent + 1));
            }
            ParseNode::Unary(node) => {
                output.push_str(&format!("{}Unary Operation:\n", indent_str));
                output.push_str(&format!("{}Operator: {}\n", indent_str, node.op));
                output.push_str(&format!("{}Operand:\n", indent_str));
                output.push_str(&node.operand.print_with_indent(indent + 1));
            }
            ParseNode::Value(node) => {
                output.push_str(&format!("{}Value: {}\n", indent_str, node.lexeme));
            }
            ParseNode::ScalarDecl(node) => {
                output.push_str(&format!("{}Scalar Declaration:\n", indent_str));
                let inner_indent = format!("{}  ", indent_str);
                output.push_str(&format!("{}Specifier: {}\n", inner_indent, node.specifier));
                output.push_str(&format!("{}Type: {}\n", inner_indent, node.typ));
                output.push_str(&format!("{}Identifier: {}\n", inner_indent, node.id));
            }
            ParseNode::ArrDecl(node) => {
                output.push_str(&format!("{}Array Declaration:\n", indent_str));
                let inner_indent = format!("{}  ", indent_str);
                output.push_str(&format!("{}Specifier: {}\n", inner_indent, node.specifier));
                output.push_str(&format!("{}Type: {}\n", inner_indent, node.typ));
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
                output.push_str(&format!("{}{}\n", indent_str, node.typ.to_string()));
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
