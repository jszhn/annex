mod util;

use crate::lexer::{Lexer, Token, TokenType};
use crate::parse::util::ParserError;
use std::error::Error;

pub struct Parser {
    tree: Vec<ParseNode>,
}

impl Parser {
    pub fn new(mut tokens: Lexer) -> Result<Parser, Box<dyn Error>> {
        parse(&mut tokens)
    }
}

enum ParseNode {
    Function(FunctionNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Value(Value),
    Expr(ExprNode),
}

struct FunctionNode {
    name: Token,
    params: Vec<ParseNode>,
    body: Box<ParseNode>,
}

struct BinaryNode {
    left: Box<ParseNode>,
    op: Token,
    right: Box<ParseNode>,
}

impl BinaryNode {
    fn new(op: Token, left: ParseNode, right: ParseNode) -> BinaryNode {
        BinaryNode {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

struct UnaryNode {
    op: Token,
    operand: Box<ParseNode>,
}

impl UnaryNode {
    fn new(op: Token, operand: ParseNode) -> UnaryNode {
        UnaryNode {
            op,
            operand: Box::new(operand),
        }
    }
}

struct Value {
    lexeme: String,
}

impl Value {
    fn new(val: &String) -> Value {
        Value {
            lexeme: val.clone(),
        }
    }
}

struct ExprNode {
    lexeme: String,
    children: Vec<ParseNode>,
}

impl ExprNode {
    fn new(val: &String, vec: Vec<ParseNode>) -> ExprNode {
        ExprNode {
            lexeme: val.clone(),
            children: vec,
        }
    }
}

fn parse(tokens: &mut Lexer) -> Result<Parser, Box<dyn Error>> {
    let head = construct_expr(tokens, 0)?;
    Ok(Parser { tree: vec![head] })
}

fn construct_expr(tokens: &mut Lexer, min_power: usize) -> Result<ParseNode, ParserError> {
    let token = tokens.consume();
    let Some(value) = &token.lexeme else {
        return Err(ParserError::new(
            "Invalid lexer value. Possibly unsupported?",
        ));
    };

    let mut lhs: ParseNode = match &token.token_type {
        TokenType::Identifier => ParseNode::Value(Value::new(value)),
        TokenType::Operator => {
            let ((), r_bp) = pre_binding_power(&value)?;
            let rhs = construct_expr(tokens, r_bp)?;
            ParseNode::Unary(UnaryNode::new(token.clone(), rhs))
        }
        TokenType::GroupBegin => {
            let lhs = construct_expr(tokens, 0)?;
            assert_eq!(
                tokens.consume(),
                Token::new(TokenType::GroupEnd, ")".to_string())
            );
            lhs
        }
        _ => return Err(ParserError::new("Unsupported lexer type.")),
    };

    loop {
        let next_token: Option<String> = match tokens.peek().token_type {
            TokenType::EOF => break,
            TokenType::Separator => {
                if let Some(val) = tokens.peek().lexeme {
                    if val == ";" {
                        break;
                    } else {
                        // to implement
                        break;
                    }
                } else {
                    break;
                }
            }
            TokenType::GroupBegin | TokenType::GroupEnd => tokens.peek().lexeme.clone(),
            TokenType::Operator => tokens.peek().lexeme.clone(),
            _ => {
                return Err(ParserError::new(
                    "Bad, unsupported, or unrecognised lexer value.",
                ))
            }
        };

        if let Some((l_bp, ())) = post_binding_power(&value) {
            if l_bp < min_power {
                break;
            }
            _ = tokens.consume();

            lhs = if value == "[" {
                let rhs = construct_expr(tokens, 0)?;
                assert_eq!(
                    tokens.consume(),
                    Token::new(TokenType::GroupEnd, "]".to_string())
                );
                ParseNode::Expr(ExprNode::new(value, vec![lhs, rhs]))
            } else {
                ParseNode::Expr(ExprNode::new(value, vec![lhs]))
            };
            continue;
        }

        if let Some(value) = next_token {
            if let Some((l_bp, r_bp)) = set_binding_power(&value) {
                if l_bp < min_power {
                    break;
                }
                _ = tokens.consume();
                let rhs = construct_expr(tokens, r_bp)?;
                lhs = ParseNode::Expr(ExprNode::new(&value, vec![lhs, rhs]));
                continue;
            }
        }
        break;
    }
    Ok(lhs)
}

fn set_binding_power(op_str: &String) -> Option<(usize, usize)> {
    // determines operator order of precedence
    let op = op_str.as_str();
    match op {
        "=" => Some((1, 2)),
        "or" => Some((7, 8)),
        "and" => Some((9, 10)),
        "|" => Some((11, 12)),
        "^" => Some((13, 14)),
        "&" => Some((15, 16)),
        "==" | "!=" => Some((17, 18)),
        "<<" | ">>" => Some((21, 22)),
        "+" | "-" => Some((27, 28)),
        "*" | "/" | "%" => Some((31, 32)),
        _ => None,
    }
}

fn pre_binding_power(op_str: &String) -> Result<((), usize), ParserError> {
    let op = op_str.as_str();
    match op {
        "+" | "-" => Ok(((), 3)),
        "!" => Ok(((), 20)),
        _ => {
            println!("{}", op_str);
            return Err(ParserError::new(
                "Bad, unsupported, or unrecognised lexer operator",
            ));
        }
    }
}

fn post_binding_power(op_str: &String) -> Option<(usize, ())> {
    let op = op_str.as_str();
    match op {
        "[" => Some((36, ())),
        _ => return None,
    }
}
