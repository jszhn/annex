pub mod types;

use crate::token;

pub struct Ast {
    head: AstNode,
}

pub struct AstNode {
    token: token::CToken,
    next: Vec<token::CToken>,
}

impl Ast {
    pub fn new(tokens: Vec<token::CToken>) {
        // return parse(tokens);
    }
}

// fn parse(tokens: Vec<token::CToken>) -> Ast {
//     let size = tokens.len();
// }
//
// fn construct_expr() -> AstNode {}
