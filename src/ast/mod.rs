mod grammar;
mod io;
mod sym_table;
pub mod types;

use crate::ast::sym_table::{FuncEntry, SymTable, VarEntry};
use crate::lexer::{Lexer, Token, TokenType};
use std::collections::HashSet;
use std::error::Error;

trait AstCommon {
    fn visit(&self) {}
}

pub(crate) enum ExprType {
    Atom(String),
    Cons(String, Vec<ExprType>),
}

pub struct Ast {
    head: Box<AstNode>,
}

impl Ast {
    pub fn new(mut tokens: Lexer) -> Result<Ast, Box<dyn Error>> {
        return parse(&mut tokens);
    }

    pub fn new_head(head: Box<AstNode>) -> Ast {
        return Ast { head };
    }

    pub fn get_head_ref(&self) -> &Box<AstNode> {
        return &self.head;
    }
}

pub struct AstNode {
    expr_type: ExprType,
    var_table: SymTable<VarEntry>,
    func_table: SymTable<FuncEntry>,
}

impl AstNode {
    pub fn print(&self) {
        self.expr_type.print();
    }

    fn new(expr_type: ExprType) -> Box<AstNode> {
        return Box::new(AstNode { expr_type });
    }

    fn new_atom(value: String) -> Box<AstNode> {
        return Box::new(AstNode {
            expr_type: ExprType::Atom(value),
        });
    }

    fn new_cons(value: String, next: Vec<ExprType>) -> Box<AstNode> {
        return Box::new(AstNode {
            expr_type: ExprType::Cons(value, next),
        });
    }
}

enum ExprNode {
    Unary(UnaryExprNode),
    Binary(BinaryExprNode),
    Call(CallExprNode),
}

enum UnaryOperator {
    Not,
}
struct UnaryExprNode {
    operator: UnaryOperator,
    operand: Box<ExprNode>,
}

enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div,
    ShiftLeft,
    ShiftRight,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Eq,
}

struct BinaryExprNode {
    operator: BinaryOperator,
    left: Box<ExprNode>,
    right: Box<ExprNode>,
}

struct CallExprNode {
    name: String,
    args: Vec<ArgumentNode>,
}

struct ArgumentNode {
    expr: ExprNode,
}

enum StmtNode {
    Assign(AssignStmtNode),
    Expr(ExprStmtNode),
    If(IfStmtNode),
    While(WhileStmtNode),
    For(ForStmtNode),
    Return(ReturnStmtNode),
    Scope(ScopeNode),
}

struct AssignStmtNode {
    target: VariableNode,
    expr: ExprNode,
}

struct ExprStmtNode {
    expr: ExprNode,
}

struct IfStmtNode {
    condition: ExprNode,
    then: Box<StmtNode>,
    elif: Vec<Box<StmtNode>>,
    _else: Option<Box<StmtNode>>,
}

struct WhileStmtNode {
    condition: ExprNode,
    body: Box<StmtNode>,
}

struct ForStmtNode {
    pre: ExprNode,
    condition: ExprNode,
    post: ExprNode,
    body: Box<StmtNode>,
}

struct ReturnStmtNode {
    expr: Option<ExprNode>,
}

struct ScopeNode {
    var_table: SymTable<VarEntry>,
}

impl ExprType {
    fn print(&self) {
        match self {
            ExprType::Atom(s) => println!("Atom: {}", s),
            ExprType::Cons(s, vec) => {
                println!("Cons: {}, {} elems in vector (", s, vec.len());
                for e in vec {
                    e.print();
                }
                println!(")");
            }
        }
    }
}
