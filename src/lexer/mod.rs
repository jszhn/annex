use std::error::Error;

use log::info;

pub mod io;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub enum TokenType {
    Type,
    Control,
    Function,
    Specifier,
    Operator,
    Identifier, // catch-all for non-keyword tokens
    Separator,
    Return,
    Boolean,
    Decimal,
    Integer,
    GroupBegin,
    GroupEnd,
    None,
    EOF,
}

pub struct Lexer {
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(file: String) -> Result<Lexer, Box<dyn Error>> {
        info!("[Lexer]: starting");
        let token_str = tokenise(file)?;
        info!("[Lexer]: finished");
        Ok(Lexer { tokens: token_str })
    }

    /// Pops the next token from the stack.
    pub fn consume(&mut self) -> Token {
        self.tokens
            .pop()
            .unwrap_or(Token::new_blank(TokenType::EOF))
    }

    /// Clones token at the top of the stack.
    pub fn peek(&mut self) -> Token {
        self.tokens
            .last()
            .cloned()
            .unwrap_or(Token::new_blank(TokenType::EOF))
    }

    /// Returns reference to the token stack.
    pub fn get_ref(&mut self) -> &Vec<Token> {
        &self.tokens
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
}

impl Token {
    pub fn new(token_type: TokenType, value: &str) -> Token {
        Token {
            token_type,
            lexeme: Some(value.to_string()),
        }
    }

    /// Token constructor with no lexeme field.
    pub fn new_blank(token_type: TokenType) -> Token {
        Token {
            token_type,
            lexeme: None,
        }
    }
}

/// Primary lexical analysis interface.
///     Input: file contents
///     Returns: vector of tokens
fn tokenise(file: String) -> Result<Vec<Token>, Box<dyn Error>> {
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
                if !buf.is_empty() && !comment {
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
            '+' | '-' | '=' | '*' | '?' | '|' | '&' | '^' | '>' | '<' | '~' => {
                token_type = TokenType::Operator
            }
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
            if !buf.is_empty() {
                // clear existing multichar lexeme
                tokens.push(multichar_token(&buf));
                buf.clear();
            }
            tokens.push(Token::new(token_type, &c.to_string())); // ensures we don't miss tokens
        }
    }
    tokens.push(Token::new_blank(TokenType::EOF));
    tokens.reverse(); // for iterating during parse tree generation
    Ok(tokens)
}

/// Function that converts a multi-character lexeme into a token.
/// By default, any unreserved lexemes will be converted into an identifier token.
fn multichar_token(token: &str) -> Token {
    // determines what a multi-character token is
    match token {
        ">>" | "<<" | "and" | "or" => Token::new(TokenType::Operator, token),
        "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f64" | "f32" | "void" => {
            Token::new(TokenType::Type, token)
        }
        "fn" => Token::new_blank(TokenType::Function),
        "if" | "while" | "for" | "break" | "continue" | "elif" | "else" => {
            Token::new(TokenType::Control, token)
        }
        "var" | "const" | "vol" => Token::new(TokenType::Specifier, token),
        "return" => Token::new_blank(TokenType::Return),
        "true" | "false" => Token::new(TokenType::Boolean, token),
        _ => {
            if token.parse::<i32>().is_ok() {
                Token::new(TokenType::Integer, token)
            } else if token.parse::<f32>().is_ok() {
                Token::new(TokenType::Decimal, token)
            } else {
                Token::new(TokenType::Identifier, token)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let files: Vec<&str> = vec!["tests/files/arithmetic.ax"];
        let tokens: Vec<Vec<Token>> = vec![vec![
            Token::new_blank(TokenType::EOF),
            Token::new(TokenType::Separator, ";"),
            Token::new(TokenType::Integer, "555"),
            Token::new(TokenType::Separator, "."),
            Token::new(TokenType::Identifier, "3"),
            Token::new(TokenType::Operator, "*"),
            Token::new(TokenType::Identifier, "10"),
            Token::new(TokenType::Operator, "+"),
            Token::new(TokenType::Identifier, "id"),
        ]];

        for (i, file) in files.iter().enumerate() {
            let contents = std::fs::read_to_string(file).unwrap();
            let mut lex = Lexer::new(contents).expect("err: lexical analysis failed!");
            lex.print_all();
            let test_tokens = lex.get_ref();
            for j in 0..tokens.len() {
                assert_eq!(tokens[i][j].token_type, test_tokens[j].token_type);
                assert_eq!(tokens[i][j].lexeme, test_tokens[j].lexeme);
            }
        }
    }
}
