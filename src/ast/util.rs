use std::fmt::{Debug, Display, Formatter};

use crate::ast::types::{AstNode, BinaryOperator, Literal, StorageClass, Type, UnaryOperator};
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

/*
   print utilities
*/
impl Ast {
    pub fn print(&self, stdout: bool) -> String {
        let result = self.head.print();
        if stdout {
            print!("{}", &result);
        }
        result
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
                output.push_str(&format!("{}Function: {}\n", indent_str, node.name));
                output.push_str(&format!(
                    "{}Return type: {}\n",
                    indent_str,
                    Type::from_enum(&node.return_type)
                ));
                output.push_str(&format!("{}Parameters:\n", indent_str));
                for param in &node.params {
                    output.push_str(&format!("{}  Parameter:\n", indent_str));
                    output.push_str(&format!("{}    Name: {}\n", indent_str, param.name));
                    output.push_str(&format!(
                        "{}    Type: {}\n",
                        indent_str,
                        Type::from_enum(&param.typ)
                    ));
                    output.push_str(&format!(
                        "{}    Storage: {}\n",
                        indent_str,
                        StorageClass::from_enum(&param.spec)
                    ));

                    if let Some(size) = &param.size {
                        output.push_str(&format!("{}    Size:\n", indent_str));
                        output.push_str(&size.print_with_indent(indent + 3));
                    }
                }
                output.push_str(&format!("{}Body:\n", indent_str));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            AstNode::FunctionCall(node) => {
                output.push_str(&format!("{}FunctionCall: {}\n", indent_str, node.name));
                if !node.args.is_empty() {
                    output.push_str(&format!("{}Arguments:\n", indent_str));
                    for arg in &node.args {
                        output.push_str(&arg.print_with_indent(indent + 1));
                    }
                }
            }
            AstNode::Binary(node) => {
                output.push_str(&format!("{}Binary Operation:\n", indent_str));
                output.push_str(&format!(
                    "{}Operator: {}\n",
                    indent_str,
                    BinaryOperator::from_enum(&node.op)
                ));
                output.push_str(&format!("{}Left:\n", indent_str));
                output.push_str(&node.left.print_with_indent(indent + 1));
                output.push_str(&format!("{}Right:\n", indent_str));
                output.push_str(&node.right.print_with_indent(indent + 1));
            }
            AstNode::Unary(node) => {
                output.push_str(&format!("{}Unary Operation:\n", indent_str));
                output.push_str(&format!(
                    "{}Operator: {}\n",
                    indent_str,
                    UnaryOperator::from_enum(&node.op)
                ));
                output.push_str(&format!("{}Operand:\n", indent_str));
                output.push_str(&node.expr.print_with_indent(indent + 1));
            }
            AstNode::If(node) => {
                output.push_str(&format!("{}If Statement:\n", indent_str));
                output.push_str(&format!("{}Condition:\n", indent_str));
                output.push_str(&node.condition.print_with_indent(indent + 1));
                output.push_str(&format!("{}Then Branch:\n", indent_str));
                output.push_str(&node.then_branch.print_with_indent(indent + 1));

                if !node.elif_branches.is_empty() {
                    output.push_str(&format!("{}Elif Branches:\n", indent_str));
                    for (cond, branch) in &node.elif_branches {
                        output.push_str(&format!("{}Condition:\n", indent_str));
                        output.push_str(&cond.print_with_indent(indent + 1));
                        output.push_str(&format!("{}Then:\n", indent_str));
                        output.push_str(&branch.print_with_indent(indent + 1));
                    }
                }

                if let Some(else_branch) = &node.else_branch {
                    output.push_str(&format!("{}Else Branch:\n", indent_str));
                    output.push_str(&else_branch.print_with_indent(indent + 1));
                }
            }
            AstNode::While(node) => {
                output.push_str(&format!("{}While Loop:\n", indent_str));
                output.push_str(&format!("{}Condition:\n", indent_str));
                output.push_str(&node.condition.print_with_indent(indent + 1));
                output.push_str(&format!("{}Body:\n", indent_str));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            AstNode::For(node) => {
                output.push_str(&format!("{}For Loop:\n", indent_str));
                output.push_str(&format!("{}Initialization:\n", indent_str));
                output.push_str(&node.init.print_with_indent(indent + 1));
                output.push_str(&format!("{}Condition:\n", indent_str));
                output.push_str(&node.condition.print_with_indent(indent + 1));
                output.push_str(&format!("{}Update:\n", indent_str));
                output.push_str(&node.update.print_with_indent(indent + 1));
                output.push_str(&format!("{}Body:\n", indent_str));
                output.push_str(&node.body.print_with_indent(indent + 1));
            }
            AstNode::Return(expr_opt) => {
                output.push_str(&format!("{}Return:\n", indent_str));
                if let Some(expr) = expr_opt {
                    output.push_str(&expr.print_with_indent(indent + 1));
                } else {
                    output.push_str(&format!("{}void\n", indent_str));
                }
            }
            AstNode::Break => {
                output.push_str(&format!("{}Break\n", indent_str));
            }
            AstNode::Continue => {
                output.push_str(&format!("{}Continue\n", indent_str));
            }
            AstNode::Block(node) => {
                output.push_str(&format!("{}Block:\n", indent_str));
                for elem in &node.elems {
                    output.push_str(&elem.print_with_indent(indent + 1));
                }
            }
            AstNode::VarDecl(node) => {
                output.push_str(&format!("{}Scalar Declaration:\n", indent_str));
                let inner_indent = format!("{}  ", indent_str);
                output.push_str(&format!(
                    "{}Specifier: {}\n",
                    inner_indent,
                    StorageClass::from_enum(&node.storage)
                ));
                output.push_str(&format!(
                    "{}Type: {}\n",
                    inner_indent,
                    Type::from_enum(&node.typ)
                ));
                output.push_str(&format!("{}Identifier: {}\n", inner_indent, node.name));
                if let Some(init) = &node.initialiser {
                    output.push_str(&format!("{}Initializer:\n", inner_indent));
                    output.push_str(&init.print_with_indent(indent + 2));
                }
            }
            AstNode::ArrDecl(node) => {
                output.push_str(&format!("{}Array Declaration:\n", indent_str));
                let inner_indent = format!("{}  ", indent_str);
                output.push_str(&format!(
                    "{}Specifier: {}\n",
                    inner_indent,
                    StorageClass::from_enum(&node.storage)
                ));
                output.push_str(&format!(
                    "{}Type: {}\n",
                    inner_indent,
                    Type::from_enum(&node.typ)
                ));
                output.push_str(&format!("{}Size:\n", inner_indent));
                output.push_str(&node.size.print_with_indent(indent + 2));
                output.push_str(&format!("{}Identifier: {}\n", inner_indent, node.name));
                if let Some(init) = &node.initialiser {
                    output.push_str(&format!("{}Initializer:\n", inner_indent));
                    output.push_str(&init.print_with_indent(indent + 2));
                }
            }
            AstNode::Identifier(name) => {
                output.push_str(&format!("{}Identifier: {}\n", indent_str, name));
            }
            AstNode::Literal(lit) => match lit {
                Literal::Int(i) => output.push_str(&format!("{}Literal: int({})\n", indent_str, i)),
                Literal::Float(f) => {
                    output.push_str(&format!("{}Literal: float({})\n", indent_str, f))
                }
                Literal::Bool(b) => {
                    output.push_str(&format!("{}Literal: bool({})\n", indent_str, b))
                }
            },
            AstNode::Arr(node) => {
                output.push_str(&format!("{}Array Access:\n", indent_str));
                output.push_str(&format!("{}Array: {}\n", indent_str, node.name));
                output.push_str(&format!("{}Index:\n", indent_str));
                output.push_str(&node.access.print_with_indent(indent + 1));
            }
        }
        output
    }
}
