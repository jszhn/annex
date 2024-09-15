use crate::token::types::TokenType;
use crate::token::{Lexer, Token};
use crate::{token, util};
use std::fs;

impl Lexer {
    pub fn print_tokens(&self) {
        // only prints tokens
        for item in &self.tokens {
            if let Some(value) = &item.value {
                println!("{}", value);
            }
        }
    }

    pub fn print_all(&self) {
        // prints all: enum value and token
        for item in &self.tokens {
            if let Some(value) = &item.value {
                println!("{}: {}", get_token_name(item.token_type), value);
            }
        }
    }
}

impl Token {
    pub fn print(&self) {
        if let Some(val) = &self.value {
            println!("{}", val);
        }
    }
}

impl util::OutputHandler for Vec<token::Token> {
    fn to_txt(&self, path: String) -> bool {
        let mut output: String = String::from("");
        for item in self {
            let mut buf = get_token_name(item.token_type);
            if let Some(value) = &item.value {
                buf.push_str(": ");
                buf.push_str(value.as_str());
                buf.push_str("\n");
                output.push_str(buf.as_str());
            }
        }
        if let Ok(()) = fs::write(path, output) {
            true
        } else {
            false
        }
    }

    fn from_txt(path: String) {
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
