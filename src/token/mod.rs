use crate::token::enums::CLiteral::Integer;
use crate::token::enums::CTokenType;

pub mod enums;
mod debug;

pub struct CToken {
    token_type: CTokenType,
    value: String
}

pub fn tokenise(file: String) -> Vec<CToken> {
    let mut tokens: Vec<CToken> = Vec::new();
    let chars = file.chars();

    let mut line_num = 1; // running count for later use
    let mut tally = String::new();
    for c in chars {
        let char = c;

        match char { // handle single-char tokens first
            ';' => {
                if tally.len() != 0 { // TODO: make this more robust, shouldn't need to do this for every char
                    tokens.push(multichar_token(&tally));
                    tally.clear();
                }
                tokens.push(CToken {
                    token_type: CTokenType::Separator, value: String::from(";")})
            },
            ' ' => {
                if tally.len() != 0 {
                    tokens.push(multichar_token(&tally));
                    tally.clear();
                }
            },
            '\n' => line_num += 1,
            _ => tally.push(char),
        }
    }
    return tokens;
}

fn multichar_token(token: &String) -> CToken {
    // logic handler determining what a multi-character token is
    match token.as_str() {
        "return" => CToken {
            token_type: CTokenType::Keyword, value: String::from("return")},
        _ => multichar_token_id(&token)
    }
}

fn multichar_token_id(token: &String) -> CToken {
    return CToken {
        token_type: CTokenType::Literal(Integer), value: token.clone()
    }
}