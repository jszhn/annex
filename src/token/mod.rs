use crate::token::types::TokenType;

pub mod io;
pub mod types;

pub struct Lexer {
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(file: String) -> Lexer {
        Lexer {
            tokens: tokenise(file),
        }
    }

    pub fn consume(&mut self) -> Token {
        self.tokens
            .pop()
            .unwrap_or(Token::new_blank(TokenType::EOF))
    }

    pub fn peek(&mut self) -> Token {
        self.tokens
            .last()
            .cloned()
            .unwrap_or(Token::new_blank(TokenType::EOF))
    }

    pub fn get_ref(&mut self) -> &Vec<Token> {
        return &self.tokens;
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) value: Option<String>,
}

impl Token {
    pub fn new(token_type: TokenType, value: String) -> Token {
        Token {
            token_type,
            value: Some(value),
        }
    }

    pub fn new_blank(token_type: TokenType) -> Token {
        Token {
            token_type,
            value: None,
        }
    }
}

fn tokenise(file: String) -> Vec<Token> {
    // primary tokenisation interface
    let chars = file.chars();
    let mut tokens: Vec<Token> = Vec::new();
    let mut buf = String::new();

    // tracking variables
    let mut token_type = TokenType::Identifier;
    let mut comment = false; // used with div boolean
    let mut div = false; // checks division op versus single-line comment

    for c in chars {
        let mut single_token = true; // for checking multichar tokens

        match c {
            // handle single-char tokens first
            '(' | '{' | '[' => token_type = TokenType::GroupBegin,
            ')' | '}' | ']' => token_type = TokenType::GroupEnd,
            ';' | ',' | '.' | ':' | '#' => token_type = TokenType::Separator,
            ' ' | '\t' | '\r' | '\x04' => {
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
                    token_type = TokenType::Operator;
                } else {
                    _ = tokens.pop();
                    comment = true;
                    continue;
                }
            }
            '+' | '-' | '=' | '*' | '?' | '|' | '&' | '^' | '!' => token_type = TokenType::Operator,
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
            tokens.push(Token::new(token_type, c.to_string()));
        }
    }
    tokens.reverse(); // for iterating during parse tree generation
    return tokens;
}

fn multichar_token(token: &String) -> Token {
    // logic handler determining what a multi-character token is
    match token.as_str() {
        ">>" | "<<" | ">" | "<" | "and" | "or" => Token::new(TokenType::Operator, token.clone()),
        "i32" | "f64" | "f32" | "i8" | "i64" | "i16" | "void" | "enum" => {
            Token::new(TokenType::Type, token.clone())
        }
        "fn" => Token::new_blank(TokenType::Function),
        "immut" | "unsigned" => Token::new(TokenType::Specifier, token.clone()),
        "if" | "break" | "continue" | "do" | "else" | "for" | "goto" | "switch" | "while" => {
            Token::new(TokenType::Control, token.clone())
        }
        "return" => Token::new_blank(TokenType::Return),
        "true" | "false" => Token::new(TokenType::Boolean, token.clone()),
        _ => Token::new(TokenType::Identifier, token.clone()),
    }
}

// fn combine_compound(tokens: Vec<Token>) -> Vec<Token> {
//     /*
//        Combines multichar operator tokens.
//     */
//     let mut new_tokens: Vec<Token> = vec![];
//
//     let mut iter = tokens.into_iter().peekable();
//     while let Some(curr) = iter.next() {
//         match curr.token_type {
//             TokenType::Operator => {
//                 // check for double char operators
//                 if let Some(next) = iter.next() {
//                     match next.token_type {
//                         // check next token value
//                         TokenType::Operator => {
//                             let mut new_val = curr.value.clone();
//                             new_val.push_str(next.value.clone().as_str());
//                             new_tokens.push(Token::new(TokenType::Operator, new_val));
//                         }
//                         _ => {
//                             new_tokens.push(curr);
//                             new_tokens.push(next)
//                         } // if next token isn't an operator, we push as normal
//                     }
//                 }
//             }
//             _ => new_tokens.push(curr),
//         }
//     }
//     new_tokens
// }
