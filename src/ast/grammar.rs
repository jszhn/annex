use crate::ast::types::DType;
use crate::ast::AstNode;
use crate::lexer;

#[derive(Eq, Hash, PartialEq)]
pub struct Function {
    id: String,
    return_type: DType,
    inputs: Option<Vec<lexer::Token>>,
}

impl Function {
    pub fn new(id: String, return_type: DType, inputs: Option<Vec<lexer::Token>>) -> Function {
        return Function {
            id,
            return_type,
            inputs,
        };
    }
}
//
// pub fn function_handler(itr: Rev<Iter<Token>>) -> Function {
//     for lexer in itr {}
//     todo!()
// }

pub struct Variable {}

pub struct Expression {
    head: AstNode,
}
