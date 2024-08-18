use crate::token::types::CTokenType;

pub mod types;
pub mod io;

pub struct CToken {
    token_type: CTokenType,
    value: String
}

pub fn tokenise(file: String) -> Vec<CToken> {
    let mut tokens: Vec<CToken> = Vec::new();
    let chars = file.chars();

    let mut line_num = 1; // running count for later use
    let mut tally = String::new();
    let mut token_type = CTokenType::Literal;
    for c in chars {
        let char = c;

        let mut single_token = true;
        let mut div = false;
        let mut comment = false;
        let mut push = true;

        match char { // handle single-char tokens first
            '(' | '{' | '[' => token_type = CTokenType::GroupBegin,
            ')' | '}' | ']' => token_type = CTokenType::GroupEnd,
            ';' | ',' | '.' => token_type = CTokenType::Separator,
            ' ' | '\t' | '\r' => {
                if tally.len() != 0 { // clear existing multichar tokens
                    tokens.push(multichar_token(&tally));
                    tally.clear();
                } else {
                    div = false;
                }
                continue;
            },
            '/' => {
                if !div {
                    div = true;
                    token_type = CTokenType::Operator;
                } else {
                    comment = true;
                }
            },
            '+' | '-' | '=' | '*' => token_type = CTokenType::Operator,
            '\n' => {
                line_num += 1;
                if comment { // single-line comment
                    tally.clear();
                }
                push = false;
            },
            _ => {
                single_token = false;
                tally.push(char)
            },
        }

        if single_token {
            if tally.len() != 0 { // clear existing multichar token
                tokens.push(multichar_token(&tally));
                tally.clear();
            }
            if push {
                tokens.push(CToken { token_type, value: char.to_string() });
            }
        }
    }
    return tokens;
}

fn multichar_token(token: &String) -> CToken {
    // logic handler determining what a multi-character token is
    match token.as_str() {
        "int" | "double" | "float" | "char" | "void" | "struct" | "enum" | "long" | "short" =>
            CToken {token_type: CTokenType::Type, value: token.clone()},
        "auto" | "const" | "register" | "unsigned" | "static" | "volatile" | "extern" =>
            CToken {token_type: CTokenType::Specifier, value: token.clone()},
        "return" | "if" | "break" | "continue" | "do" | "else" | "for" | "goto" | "switch" | "while" =>
            CToken {token_type: CTokenType::Control, value: token.clone()},
        _ => multichar_token_id(&token)
    }
}

fn multichar_token_id(token: &String) -> CToken {
    return CToken {
        token_type: CTokenType::Literal, value: token.clone()
    }
}