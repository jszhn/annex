mod util;

use crate::lexer::{Lexer, Token, TokenType};
use crate::parse::util::ParserError;
use crate::parse::ParseNode::Function;
use std::error::Error;

pub struct Parser {
    head: ParseNode,
}

impl Parser {
    pub fn new(mut tokens: Lexer) -> Result<Parser, Box<dyn Error>> {
        parse(&mut tokens)
    }
}

trait Group {
    fn push_body(&mut self, node: ParseNode) -> Result<(), ParserError>;
}

enum ParseNode {
    Function(FunctionNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Value(Value),
    Expr(ExprNode),
    Control(ControlNode),
    Loop(LoopNode),
    Scope(ScopeNode),
    None,
}

impl Group for ParseNode {
    fn push_body(&mut self, node: ParseNode) -> Result<(), ParserError> {
        match self {
            ParseNode::Function(n) => n.push_body(node),
            ParseNode::Loop(n) => n.push_body(node),
            ParseNode::Scope(n) => n.push_body(node),
            _ => {
                return Err(ParserError::new(
                    "Internal code error. Unexpected parser node type.",
                ))
            }
        }
        Ok(())
    }
}

struct FunctionNode {
    name: Token,
    params: Vec<ParseNode>,
    body: Vec<ParseNode>,
}

impl FunctionNode {
    fn new(name: &Token) -> FunctionNode {
        FunctionNode {
            name: name.clone(),
            params: Vec::new(),
            body: Vec::new(),
        }
    }

    fn push_param(&mut self, param: ParseNode) {
        self.params.push(param)
    }

    fn push_body(&mut self, node: ParseNode) {
        self.body.push(node)
    }
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

struct ControlNode {
    cond: ExprNode,
    elif: Vec<ControlNode>,
    el: Option<ScopeNode>,
}

impl ControlNode {
    fn new(cond: ExprNode) -> ControlNode {
        ControlNode {
            cond,
            elif: Vec::new(),
            el: None,
        }
    }

    fn push_body(&mut self, node: ControlNode) {
        self.elif.push(node)
    }
}

struct LoopNode {
    _type: TokenType,
    cond: ExprNode,
    contents: Vec<ParseNode>,
}

impl LoopNode {
    fn new(_type: TokenType, cond: ExprNode) -> LoopNode {
        LoopNode {
            _type,
            cond,
            contents: Vec::new(),
        }
    }

    fn push_body(&mut self, node: ParseNode) {
        self.contents.push(node)
    }
}

struct ScopeNode {
    contents: Vec<ParseNode>,
}

impl ScopeNode {
    fn new() -> ScopeNode {
        ScopeNode {
            contents: Vec::new(),
        }
    }

    fn push_body(&mut self, node: ParseNode) {
        self.contents.push(node)
    }
}

/// Primary parser interface. Called by the ParseTree struct constructor.
fn parse(tokens: &mut Lexer) -> Result<Parser, Box<dyn Error>> {
    let head = construct_group_expr(tokens, true)?;
    Ok(Parser { head })
}

/// Parse tree generation for grouped constructs (functions, scopes).
fn construct_group_expr(tokens: &mut Lexer, global_scope: bool) -> Result<ParseNode, ParserError> {
    let mut node: ParseNode = ParseNode::None;
    if global_scope {
        node = ParseNode::Function(FunctionNode::new(&Token::new(
            TokenType::None,
            "__global__".to_string(),
        )));
    }

    loop {
        let next = tokens.peek();
        match next.token_type {
            TokenType::EOF => break,
            TokenType::Function => {
                assert_eq!(tokens.consume(), Token::new_blank(TokenType::Function));
                let mut fn_node = FunctionNode::new(&tokens.consume());
                // node = ParseNode::Function(FunctionNode::new(&tokens.consume()));
                _ = tokens.consume(); // get rid of starting parentheses

                while tokens.peek().token_type != TokenType::GroupEnd {
                    // function arguments
                    fn_node.push_param(construct_expr(tokens, 0)?);
                }

                // function body
                assert_eq!(
                    tokens.consume(),
                    Token::new(TokenType::GroupBegin, "{".to_string())
                );
                'inner: loop {
                    let next = tokens.peek();
                    let lexeme = next.lexeme.unwrap_or("".to_string());
                    match next.token_type {
                        TokenType::GroupBegin => {
                            if &lexeme == "{" || &lexeme == "(" {
                                // todo: possibly problematic wrt operator precedence
                                _ = tokens.consume();
                                fn_node.push_body(construct_group_expr(tokens, false)?);
                            }
                        }
                        TokenType::GroupEnd => {
                            if &lexeme == "}" || &lexeme == ")" {
                                _ = tokens.consume();
                                break 'inner;
                            }
                        }
                        _ => {
                            fn_node.push_body(construct_expr(tokens, 0)?);
                        }
                    }
                }
                node = Function(fn_node);
            }
            TokenType::Control => {
                // if or else
                let next = tokens.consume().token_type;
                // todo: everything!
            }
            _ => node.push_body(construct_expr(tokens, 0)?)?,
        }
    }
    Ok(node)
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
