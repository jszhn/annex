use std::fmt::{Debug, Display, Formatter};

use crate::ast::types::{AstNode, Literal};
use crate::ast::Ast;

pub struct AstError {
    message: String,
}

impl AstError {
    pub fn new(msg: &str) -> AstError {
        AstError {
            message: msg.to_string(),
        }
    }
}

impl Debug for AstError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AstError")
            .field("message", &self.message)
            .finish()
    }
}

impl Display for AstError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for AstError {}

impl Display for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.head.print())
    }
}

impl AstNode {
    fn print(&self) -> String {
        self.print_with_indent(0)
    }

    fn print_with_indent(&self, indent: usize) -> String {
        let mut output = String::new();
        let indent_str = "  ".repeat(indent);

        match self {
            AstNode::Parameter(_) => unreachable!(),
            AstNode::Function(node) => {
                output.push_str(&format!("{indent_str}Function: {}\n", node.name));
                output.push_str(&format!("{indent_str}Return type: {}\n", node.return_type));
                output.push_str(&format!("{indent_str}Parameters:\n"));
                for param in &node.params {
                    output.push_str(&format!("{indent_str}  Parameter:\n"));
                    output.push_str(&format!("{indent_str}    Name: {}\n", param.name));
                    output.push_str(&format!("{indent_str}    Type: {}\n", &param.typ));
                    output.push_str(&format!("{indent_str}    Storage: {}\n", param.spec));

                    if let Some(size) = &param.size {
                        output.push_str(&format!("{indent_str}    Size:\n"));
                        output.push_str(&size.print_with_indent(indent + 3));
                    }
                }
                output.push_str(&format!("{indent_str}Body:\n"));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            AstNode::FunctionCall(node) => {
                output.push_str(&format!("{indent_str}FunctionCall: {}\n", node.name));
                if !node.args.is_empty() {
                    output.push_str(&format!("{indent_str}Arguments:\n"));
                    for arg in &node.args {
                        output.push_str(&arg.print_with_indent(indent + 1));
                    }
                }
            }
            AstNode::Binary(node) => {
                output.push_str(&format!("{indent_str}Binary Operation:\n"));
                output.push_str(&format!("{indent_str}Operator: {}\n", node.op));
                output.push_str(&format!("{indent_str}Left:\n"));
                output.push_str(&node.left.print_with_indent(indent + 1));
                output.push_str(&format!("{indent_str}Right:\n"));
                output.push_str(&node.right.print_with_indent(indent + 1));
            }
            AstNode::Unary(node) => {
                output.push_str(&format!("{indent_str}Unary Operation:\n"));
                output.push_str(&format!("{indent_str}Operator: {}\n", node.op));
                output.push_str(&format!("{indent_str}Operand:\n"));
                output.push_str(&node.expr.print_with_indent(indent + 1));
            }
            AstNode::If(node) => {
                output.push_str(&format!("{indent_str}If Statement:\n"));
                output.push_str(&format!("{indent_str}Condition:\n"));
                output.push_str(&node.condition.print_with_indent(indent + 1));
                output.push_str(&format!("{indent_str}Then Branch:\n"));
                output.push_str(&node.then_branch.print_with_indent(indent + 1));

                if !node.elif_branches.is_empty() {
                    output.push_str(&format!("{indent_str}Elif Branches:\n"));
                    for (cond, branch) in &node.elif_branches {
                        output.push_str(&format!("{indent_str}Condition:\n"));
                        output.push_str(&cond.print_with_indent(indent + 1));
                        output.push_str(&format!("{indent_str}Then:\n"));
                        output.push_str(&branch.print_with_indent(indent + 1));
                    }
                }

                if let Some(else_branch) = &node.else_branch {
                    output.push_str(&format!("{indent_str}Else Branch:\n"));
                    output.push_str(&else_branch.print_with_indent(indent + 1));
                }
            }
            AstNode::While(node) => {
                output.push_str(&format!("{indent_str}While Loop:\n"));
                output.push_str(&format!("{indent_str}Condition:\n"));
                output.push_str(&node.condition.print_with_indent(indent + 1));
                output.push_str(&format!("{indent_str}Body:\n"));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            AstNode::For(node) => {
                output.push_str(&format!("{indent_str}For Loop:\n"));
                output.push_str(&format!("{indent_str}Initialization:\n"));
                output.push_str(&node.init.print_with_indent(indent + 1));
                output.push_str(&format!("{indent_str}Condition:\n"));
                output.push_str(&node.condition.print_with_indent(indent + 1));
                output.push_str(&format!("{indent_str}Update:\n"));
                output.push_str(&node.update.print_with_indent(indent + 1));
                output.push_str(&format!("{indent_str}Body:\n"));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            AstNode::Return(expr_opt) => {
                output.push_str(&format!("{indent_str}Return:\n"));
                if let Some(expr) = expr_opt {
                    output.push_str(&expr.print_with_indent(indent + 1));
                } else {
                    output.push_str(&format!("{indent_str}void\n"));
                }
            }
            AstNode::Break => {
                output.push_str(&format!("{indent_str}Break\n"));
            }
            AstNode::Continue => {
                output.push_str(&format!("{indent_str}Continue\n"));
            }
            AstNode::Block(node) => {
                output.push_str(&format!("{indent_str}Block:\n"));
                for elem in &node.elems {
                    output.push_str(&elem.print_with_indent(indent + 1));
                }
            }
            AstNode::VarDecl(node) => {
                output.push_str(&format!("{indent_str}Scalar Declaration:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Specifier: {}\n", node.storage));
                output.push_str(&format!("{inner_indent}Type: {}\n", node.typ));
                output.push_str(&format!("{inner_indent}Identifier: {}\n", node.name));
                if let Some(init) = &node.initialiser {
                    output.push_str(&format!("{inner_indent}Initializer:\n"));
                    output.push_str(&init.print_with_indent(indent + 2));
                }
            }
            AstNode::ArrDecl(node) => {
                output.push_str(&format!("{indent_str}Array Declaration:\n"));
                let inner_indent = format!("{indent_str}  ");
                output.push_str(&format!("{inner_indent}Specifier: {}\n", node.storage));
                output.push_str(&format!("{inner_indent}Type: {}\n", node.typ));
                output.push_str(&format!("{inner_indent}Size:\n"));
                output.push_str(&node.size.print_with_indent(indent + 2));
                output.push_str(&format!("{inner_indent}Identifier: {}\n", node.name));
                if let Some(init) = &node.initialiser {
                    output.push_str(&format!("{inner_indent}Initializer:\n"));
                    output.push_str(&init.print_with_indent(indent + 2));
                }
            }
            AstNode::Identifier(name) => {
                output.push_str(&format!("{indent_str}Identifier: {name}\n"));
            }
            AstNode::Literal(lit) => match lit {
                Literal::Int(i) => output.push_str(&format!("{indent_str}Literal: int({i})\n")),
                Literal::Float(f) => output.push_str(&format!("{indent_str}Literal: float({f})\n")),
                Literal::Bool(b) => output.push_str(&format!("{indent_str}Literal: bool({b})\n")),
            },
            AstNode::Arr(node) => {
                output.push_str(&format!("{indent_str}Array Access:\n"));
                output.push_str(&format!("{indent_str}Array: {}\n", node.name));
                output.push_str(&format!("{indent_str}Index:\n"));
                output.push_str(&node.access.print_with_indent(indent + 1));
            }
        }
        output
    }
}
