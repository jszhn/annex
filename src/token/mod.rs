use crate::token::types::CTokenType;

pub mod io;
pub mod types;

pub struct CToken {
    token_type: CTokenType,
    value: String,
}

pub fn tokenise(file: String) -> Vec<CToken> {
    let chars = file.chars();
    let mut tokens: Vec<CToken> = Vec::new();
    let mut buf = String::new();

    // tracking variables
    let mut token_type = CTokenType::Identifier;
    let mut comment = false; // used with div boolean
    let mut div = false; // checks division op versus single-line comment

    for c in chars {
        let mut single_token = true; // for checking multichar tokens

        match c {
            // handle single-char tokens first
            '(' | '{' | '[' => token_type = CTokenType::GroupBegin,
            ')' | '}' | ']' => token_type = CTokenType::GroupEnd,
            ';' | ',' | '.' | ':' => token_type = CTokenType::Separator,
            ' ' | '\t' | '\r' => {
                if buf.len() != 0 && !comment {
                    // clear existing multichar tokens
                    tokens.push(multichar_token(&buf));
                    buf.clear();
                } else {
                    div = false;
                }
                continue;
            }
            '/' => {
                if !div {
                    div = true;
                    token_type = CTokenType::Operator;
                } else {
                    _ = tokens.pop();
                    comment = true;
                    continue;
                }
            }
            '+' | '-' | '=' | '*' | '<' | '>' | '?' => token_type = CTokenType::Operator,
            '\n' => {
                if comment {
                    // single-line comment
                    buf.clear();
                    comment = false;
                    div = false;
                }
                continue;
            }
            _ => {
                if comment {
                    continue;
                }
                single_token = false;
                buf.push(c)
            }
        }

        if single_token {
            if buf.len() != 0 {
                // clear existing multichar token
                tokens.push(multichar_token(&buf));
                buf.clear();
            }
            tokens.push(CToken {
                token_type,
                value: c.to_string(),
            });
        }
    }
    return tokens;
}

fn multichar_token(token: &String) -> CToken {
    // logic handler determining what a multi-character token is
    match token.as_str() {
        "int" | "double" | "float" | "char" | "void" | "struct" | "enum" | "long" | "short" => {
            CToken {
                token_type: CTokenType::Type,
                value: token.clone(),
            }
        }
        "auto" | "const" | "register" | "unsigned" | "static" | "volatile" | "extern" => CToken {
            token_type: CTokenType::Specifier,
            value: token.clone(),
        },
        "if" | "break" | "continue" | "do" | "else" | "for" | "goto" | "switch" | "while" => {
            CToken {
                token_type: CTokenType::Control,
                value: token.clone(),
            }
        }
        "return" => CToken {
            token_type: CTokenType::Return,
            value: "".to_string(),
        },
        "true" | "false" => CToken {
            token_type: CTokenType::Boolean,
            value: token.clone(),
        },
        _ => multichar_token_id(&token),
    }
}

fn multichar_token_id(token: &String) -> CToken {
    return CToken {
        token_type: CTokenType::Identifier,
        value: token.clone(),
    };
}
