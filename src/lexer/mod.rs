use std::error::Error;

pub mod io;

/// Primary lexer type that later compiler components will interface with.
pub struct Lexer {
    pub tokens: Vec<Token>,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub enum TokenType {
    // Literals
    Identifier,
    Boolean,
    Integer,
    Decimal,

    // Keywords
    Type,
    Control,
    Function,
    Specifier,
    Operator,
    Return,

    // Delineators
    Separator,
    GroupBegin,
    GroupEnd,

    // Special tokens
    None,
    EOF,
}

impl Lexer {
    pub fn new(file: String) -> Result<Self, Box<dyn Error>> {
        let tokens = tokenise(file)?;
        Ok(Lexer { tokens })
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

