use crate::token::{CToken, CTokenType};

pub fn print_tokens(vec: &Vec<CToken>) {
    // only prints tokens
    for item in vec {
        println!("{}", item.value);
    }
}

pub fn print_all(vec: &Vec<CToken>) {
    // prints all: enum value and token
    for item in vec {
        println!("{}: {}", get_token_name(item.token_type), item.value);
    }
}

impl crate::util::OutputHandler for Vec<CToken> {
    fn to_txt(&self, path: String) -> bool {
        todo!()
    }

    fn from_txt(&self, path: String) {
        todo!()
    }
}

fn get_token_name(_type: CTokenType) -> String {
    // to avoid needing the debug specifier
    match _type {
        CTokenType::Type => "Type".to_string(),
        CTokenType::Control => "Control".to_string(),
        CTokenType::Specifier => "Specifier".to_string(),
        CTokenType::Operator => "Operator".to_string(),
        CTokenType::Identifier => "Identifier".to_string(),
        CTokenType::Separator => "Separator".to_string(),
        CTokenType::Return => "Return".to_string(),
        CTokenType::Boolean => "Boolean".to_string(),
        CTokenType::GroupBegin => "GroupBegin".to_string(),
        CTokenType::GroupEnd => "GroupEnd".to_string(),
        _ => "None".to_string(),
    }
}
