use crate::syntax::AstNode;
use crate::token;

#[derive(Eq, Hash, PartialEq)]
pub struct Function {
    id: String,
    return_type: DType,
    inputs: Option<Vec<token::Token>>,
}

impl Function {
    pub fn new(id: String, return_type: DType, inputs: Option<Vec<token::Token>>) -> Function {
        return Function {
            id,
            return_type,
            inputs,
        };
    }
}
//
// pub fn function_handler(itr: Rev<Iter<Token>>) -> Function {
//     for token in itr {}
//     todo!()
// }

pub struct Variable {}

pub struct Expression {
    head: AstNode,
}

#[derive(Eq, Hash, PartialEq)]
pub enum DType {
    // type bit-val size
    Integer(u8),
    Float(u8),
    Void,
}

impl DType {
    pub fn from_str(val: Option<String>) -> DType {
        if let None = val {
            eprintln!("ERR: Unknown data type. No string value associated.");
            std::process::exit(-1);
        }

        match val.unwrap().as_str() {
            "i32" => DType::Integer(32),
            "i16" => DType::Integer(16),
            "i8" => DType::Integer(8),
            "i64" => DType::Integer(64),
            "f32" => DType::Float(32),
            "f64" => DType::Float(64),
            "void" => DType::Void,
            _ => DType::Void, // this match arm should frankly never be called
        }
    }
}
