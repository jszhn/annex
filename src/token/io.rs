use crate::token::{Token, TokenType};
use std::fs;

pub fn print_tokens(vec: &Vec<Token>) {
    // only prints tokens
    for item in vec {
        println!("{}", item.value);
    }
}

pub fn print_all(vec: &Vec<Token>) {
    // prints all: enum value and token
    for item in vec {
        println!("{}: {}", get_token_name(item.token_type), item.value);
    }
}

impl crate::util::OutputHandler for Vec<Token> {
    fn to_txt(&self, path: String) -> bool {
        let mut output: String = String::from("");
        for item in self {
            let mut buf = get_token_name(item.token_type);
            buf.push_str(": ");
            buf.push_str(item.value.as_str());
            buf.push_str("\n");
            output.push_str(buf.as_str());
        }
        if let Ok(()) = fs::write(path, output) {
            true
        } else {
            false
        }
    }

    fn from_txt(&self, path: String) {
        todo!()
    }
}

fn get_token_name(_type: TokenType) -> String {
    // to avoid needing the debug specifier
    match _type {
        TokenType::Type => "Type".to_string(),
        TokenType::Control => "Control".to_string(),
        TokenType::Specifier => "Specifier".to_string(),
        TokenType::Operator => "Operator".to_string(),
        TokenType::Identifier => "Identifier".to_string(),
        TokenType::Separator => "Separator".to_string(),
        TokenType::Return => "Return".to_string(),
        TokenType::Boolean => "Boolean".to_string(),
        TokenType::GroupBegin => "GroupBegin".to_string(),
        TokenType::GroupEnd => "GroupEnd".to_string(),
        _ => "None".to_string(),
    }
}
