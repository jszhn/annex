mod grammar;
mod io;
pub mod types;

use crate::token::types::TokenType;
use crate::token::{Lexer, Token};
use std::any::Any;
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
    pub fn get_head_ref(&self) -> &Box<AstNode> {
        return &self.head;
    }

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
    pub fn print(&self) {
        self.expr_type.print();
    }

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

impl ExprType {
    fn print(&self) {
        match self {
            ExprType::Atom(s) => println!("Atom: {}", s),
            ExprType::Cons(s, vec) => {
                println!("Cons: {}, {} elems in vector (", s, vec.len());
                for e in vec {
                    e.print();
                }
                println!(")");
            }
        }
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
    let Some(value) = token.value else {
        eprintln!("ERR: invalid token value. Empty string?");
        std::process::exit(-1);
    };

    let mut lhs: ExprType = match token.token_type {
        TokenType::Identifier => ExprType::Atom(value.clone()),
        TokenType::Operator => {
            let ((), r_bp) = pre_binding_power(&value);
            let rhs = construct_expr(tokens, r_bp);
            ExprType::Cons(value.clone(), vec![rhs])
        }
        TokenType::GroupBegin => {
            // if value == "(" {
            let lhs = construct_expr(tokens, 0);
            assert_eq!(
                tokens.consume(),
                Token::new(TokenType::GroupEnd, ")".to_string())
            );
            lhs
            // }
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
            TokenType::GroupBegin | TokenType::GroupEnd => tokens.peek().value.clone(),
            // TokenType::Identifier => {
            //     eprintln!("ERR: at least two identifiers/literals used next to each other without an operator. Exiting.");
            //     std::process::exit(-1);
            // }
            TokenType::Operator => tokens.peek().value.clone(),
            _ => {
                eprintln!(
                    "ERR: Bad, unsupported, or unrecognised token {}. Exiting.",
                    value
                );
                std::process::exit(-1);
            }
        };

        if let Some((l_bp, ())) = post_binding_power(&value) {
            if l_bp < min_power {
                break;
            }
            _ = tokens.consume();

            lhs = if value == "[" {
                let rhs = construct_expr(tokens, 0);
                assert_eq!(
                    tokens.consume(),
                    Token::new(TokenType::GroupEnd, "]".to_string())
                );
                ExprType::Cons(value.clone(), vec![lhs, rhs])
            } else {
                ExprType::Cons(value.clone(), vec![lhs])
            };
            continue;
        }

        if let Some(value) = t {
            if let Some((l_bp, r_bp)) = set_binding_power(&value) {
                if l_bp < min_power {
                    break;
                }
                _ = tokens.consume();
                let rhs: ExprType = construct_expr(tokens, r_bp);
                lhs = ExprType::Cons(value.clone(), vec![lhs, rhs]);
                continue;
            }
        }
        break;
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

fn set_binding_power(op_str: &String) -> Option<(u8, u8)> {
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
        "<<" | ">>" => Some((19, 20)),
        "+" | "-" => Some((27, 28)),
        "*" | "/" | "%" => Some((31, 32)),
        _ => None,
    }
}

fn pre_binding_power(op_str: &String) -> ((), u8) {
    let op = op_str.as_str();
    match op {
        "+" | "-" => ((), 3),
        _ => {
            eprintln!(
                "ERR: Bad, unsupported, or unrecognised token {}. Exiting.",
                op_str
            );
            std::process::exit(-1);
        }
    }
}

fn post_binding_power(op_str: &String) -> Option<(u8, ())> {
    let op = op_str.as_str();
    match op {
        "!" | "[" => Some((36, ())),
        _ => return None,
    }
}
