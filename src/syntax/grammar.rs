use crate::syntax::AstNode;
use crate::token;
use crate::token::Lexer;

#[derive(Eq, Hash, PartialEq)]
pub struct Function {
    id: String,
    return_type: token::types::TokenType,
    inputs: Option<Vec<token::Token>>,
}

impl Function {
    pub fn new(
        id: String,
        return_type: token::types::TokenType,
        inputs: Option<Vec<token::Token>>,
    ) -> Function {
        return Function {
            id,
            return_type,
            inputs,
        };
    }
}

// pub fn function_handler(tokens: &mut Lexer) -> Function {
//
// }

pub struct Variable {}

pub struct Expression {
    head: AstNode,
}
