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
            message: format!("Expected {expected}, but found {found}"),
            kind: ErrorKind::UnexpectedToken,
        }
    }

    pub fn unexpected_token_stmt(expected: &str, stmt: &str) -> Self {
        Self {
            message: format!("Expected {expected} for {stmt} statement"),
            kind: ErrorKind::UnexpectedToken,
        }
    }

    pub fn missing_token(token: &str) -> Self {
        Self {
            message: format!("Expected {token}, but not found"),
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
                output.push_str(&format!("{indent_str}Function: {}\n", node.name));
                output.push_str(&format!("{indent_str}Return type: {}\n", node.return_type));
                output.push_str(&format!("{indent_str}Parameters:\n"));
                for param in &node.params {
                    output.push_str(&param.print_with_indent(indent + 1));
                }
                output.push_str(&format!("{indent_str}Body:\n"));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            ParseNode::FunctionCall(node) => {
                output.push_str(&format!("{indent_str}Function call:\n"));
                output.push_str(&node.function.print_with_indent(indent + 1));
                for arg in &node.args {
                    output.push_str(&arg.print_with_indent(indent + 1));
                }
            }
            ParseNode::Binary(node) => {
                output.push_str(&format!("{indent_str}Binary Operation:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Operator: {}\n", node.op));
                output.push_str(&format!("{inner_indent}Left:\n"));
                output.push_str(&node.left.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Right:\n"));
                output.push_str(&node.right.print_with_indent(indent + 2));
            }
            ParseNode::Unary(node) => {
                output.push_str(&format!("{indent_str}Unary Operation:\n"));
                output.push_str(&format!("{indent_str}Operator: {}\n", node.op));
                output.push_str(&format!("{indent_str}Operand:\n"));
                output.push_str(&node.operand.print_with_indent(indent + 1));
            }
            ParseNode::Value(node) => {
                output.push_str(&format!("{indent_str}Value: {}\n", node.lexeme));
            }
            ParseNode::ScalarDecl(node) => {
                output.push_str(&format!("{indent_str}Scalar Declaration:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Specifier: {}\n", node.specifier));
                output.push_str(&format!("{inner_indent}Type: {}\n", node.typ));
                output.push_str(&format!("{inner_indent}Identifier: {}\n", node.id));
            }
            ParseNode::ArrDecl(node) => {
                output.push_str(&format!("{indent_str}Array Declaration:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Specifier: {}\n", node.specifier));
                output.push_str(&format!("{inner_indent}Type: {}\n", node.typ));
                output.push_str(&format!("{inner_indent}Size:\n"));
                output.push_str(&node.size.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Identifier: {}\n", node.id));
            }
            ParseNode::Return(node) => {
                output.push_str(&format!("{indent_str}Return:\n"));
                if let Some(expr) = &node.expr {
                    output.push_str(&expr.print_with_indent(indent + 1));
                } else {
                    output.push_str(&format!("{indent_str}  void\n"));
                }
            }
            ParseNode::Control(node) => {
                output.push_str(&format!("{indent_str}If Statement:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Condition:\n"));
                output.push_str(&node._if.cond.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Then:\n"));
                output.push_str(&node._if.then.print_with_indent(indent + 2));

                if let Some(elif_branches) = &node.elif {
                    output.push_str(&format!("{inner_indent}Elif Branches:\n"));
                    for elif in elif_branches {
                        output.push_str(&format!("{inner_indent}  Condition:\n"));
                        output.push_str(&elif.cond.print_with_indent(indent + 3));
                        output.push_str(&format!("{inner_indent}  Then:\n"));
                        output.push_str(&elif.then.print_with_indent(indent + 3));
                    }
                }

                if let Some(else_branch) = &node.el {
                    output.push_str(&format!("{inner_indent}Else:\n"));
                    output.push_str(&else_branch.print_with_indent(indent + 2));
                }
            }
            ParseNode::While(node) => {
                output.push_str(&format!("{indent_str}While Loop:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Condition:\n"));
                output.push_str(&node.cond.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Body:\n"));
                output.push_str(&node.then.print_with_indent(indent + 2));
            }
            ParseNode::For(node) => {
                output.push_str(&format!("{indent_str}For Loop:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Initialization:\n"));
                output.push_str(&node.pre.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Condition:\n"));
                output.push_str(&node.cond.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Update:\n"));
                output.push_str(&node.post.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Body:\n"));
                output.push_str(&node.then.print_with_indent(indent + 2));
            }
            ParseNode::Scope(node) => {
                output.push_str(&format!("{indent_str}Scope:\n"));
                for stmt in &node.contents {
                    output.push_str(&stmt.print_with_indent(indent + 1));
                }
            }
            ParseNode::LoopControl(node) => {
                output.push_str(&format!("{indent_str}{}\n", node.typ));
            }
            ParseNode::Constant(constant) => match constant {
                ConstantNode::Bool(b) => {
                    output.push_str(&format!("{indent_str}Constant: bool({b})\n"))
                }
                ConstantNode::Int(i) => {
                    output.push_str(&format!("{indent_str}Constant: int({i})\n"))
                }
                ConstantNode::Float(f) => {
                    output.push_str(&format!("{indent_str}Constant: float({f})\n"))
                }
            },
            ParseNode::None => {
                output.push_str(&format!("{indent_str}None\n"));
            }
        }

        output
    }
}
