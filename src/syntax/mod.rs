mod grammar;
mod io;
pub mod types;

use crate::token::types::TokenType;
use crate::token::{Lexer, Token};
use std::cmp::PartialEq;
use std::collections::HashSet;

pub struct Ast {
    // heap ptr because we recursively parse
    head: Box<AstNode>,
}

enum ExprType {
    Atom(String),
    Cons(String, Vec<ExprType>),
}

pub struct AstNode {
    expr_type: ExprType,
}

impl Ast {
    pub fn new(mut tokens: Lexer) -> Ast {
        return Ast {
            head: AstNode::new(construct_expr(&mut tokens, 0)),
        };
    }

    pub fn new_head(head: Box<AstNode>) -> Ast {
        return Ast { head };
    }
}

impl AstNode {
    fn new(expr_type: ExprType) -> Box<AstNode> {
        return Box::new(AstNode { expr_type });
    }

    fn new_atom(value: String) -> Box<AstNode> {
        return Box::new(AstNode {
            expr_type: ExprType::Atom(value),
        });
    }

    fn new_cons(value: String, next: Vec<ExprType>) -> Box<AstNode> {
        return Box::new(AstNode {
            expr_type: ExprType::Cons(value, next),
        });
    }
}

fn parse(mut tokens: Lexer) -> Ast {
    // todo!();
    let mut functions: HashSet<grammar::Function> = HashSet::new();
    let mut vars: HashSet<grammar::Variable> = HashSet::new();

    let itr = tokens.get_ref().iter().rev();
    for tok in itr {
        if let TokenType::EOF = tok.token_type {
            break;
        }

        let mut scan = false;
        let mut _return = false;
        let mut return_type: grammar::DType;
        match tok.token_type {
            TokenType::Function => scan = true,
            TokenType::Type => {
                if !scan {
                    continue;
                } else {
                    return_type = grammar::DType::from_str(tok.value.clone());
                }
            }
            _ => eprintln!("WARN: unsupported behaviour"),
        }
    }

    loop {
        let t = tokens.peek();
        if let TokenType::EOF = t.token_type {
            break;
        }

        match t.token_type {
            // TokenType::Function => _ = functions.insert(grammar::function_handler(&mut tokens)),
            TokenType::Operator | TokenType::Identifier => _ = construct_expr(&mut tokens, 0),
            _ => {
                eprintln!("Bad, unsupported, or unrecognised token. Exiting.");
                std::process::exit(-1);
            }
        }
        tokens.consume();
    }
    return Ast::new_head(AstNode::new(ExprType::Atom("test".to_string())));
}

fn construct_expr(tokens: &mut Lexer, min_power: u8) -> ExprType {
    // Pratt parser, with FP atom/cons types
    // based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    let token = tokens.consume();
    token.print();
    let mut lhs: ExprType = match token.token_type {
        TokenType::Identifier => {
            if let Some(value) = token.value {
                ExprType::Atom(value)
            } else {
                eprintln!("Bad or unrecognised identifier. Exiting.");
                std::process::exit(-1);
            }
        }
        _ => {
            eprintln!("Invalid token type. Exiting.");
            std::process::exit(-1);
        }
    };

    loop {
        let t = match tokens.peek().token_type {
            TokenType::EOF => break,
            TokenType::Separator => {
                if let Some(val) = tokens.peek().value {
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
            TokenType::Identifier => {
                eprintln!("ERR: at least two identifiers/literals used next to each other without an operator. Exiting.");
                std::process::exit(-1);
            }
            TokenType::Operator => tokens.peek().value.clone(),
            _ => {
                eprintln!("Bad, unsupported, or unrecognised token. Exiting.");
                std::process::exit(-1);
            }
        };

        if let Some(value) = t {
            let (l_bp, r_bp) = set_binding_power(&value);
            if l_bp < min_power {
                break;
            }
            _ = tokens.consume();
            let rhs: ExprType = construct_expr(tokens, r_bp);
            lhs = ExprType::Cons(value.clone(), vec![lhs, rhs]);
        }
    }
    return lhs;
}

fn lookahead(
    tokens: Vec<Token>,
    ind: usize,
    end_token_type: TokenType,
    end_token: String,
) -> usize {
    for i in ind..tokens.len() {
        if let Some(value) = &tokens[i].value {
            if tokens[i].token_type == end_token_type && *value == end_token {
                return i;
            } else {
                continue;
            }
        }
    }
    ind // not found
}

fn set_binding_power(op_str: &String) -> (u8, u8) {
    // determines operator order of precedence
    let op = op_str.as_str();
    match op {
        "or" => (1, 2),
        "and" => (3, 4),
        "|" => (5, 6),
        "^" => (7, 8),
        "&" => (9, 10),
        "==" | "!=" => (11, 12),
        "<<" | ">>" => (13, 14),
        "+" | "-" => (15, 16),
        "*" | "/" | "%" => (17, 18),
        "=" => (23, 24),
        _ => {
            eprintln!("Bad, unsupported, or unrecognised operator token. Exiting.");
            std::process::exit(-1);
        }
    }
}

fn pre_binding_power(op_str: &String) -> ((), u8) {
    let op = op_str.as_str();
    match op {
        "+" | "-" => ((), 19),
        _ => {
            eprintln!("Bad, unsupported, or unrecognised token. Exiting.");
            std::process::exit(-1);
        }
    }
}
