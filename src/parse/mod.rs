use std::error::Error;

use log::{error, info, warn};

use crate::lexer::{Lexer, Token, TokenType};
use crate::parse::structs::*;
use crate::parse::util::ParserError;

pub mod structs;
mod util;

pub struct Parser {
    pub head: ParseNode,
}

impl Parser {
    pub fn new(mut tokens: Lexer) -> Result<Parser, Box<dyn Error>> {
        info!("Starting parser");
        parse(&mut tokens)
    }
}

/// Primary parser interface. Called by the ParseTree struct constructor.
fn parse(tokens: &mut Lexer) -> Result<Parser, Box<dyn Error>> {
    let head = parse_global_scope(tokens)?;
    assert_eq!(
        tokens.get_ref().len(),
        0,
        "Internal error: parser failed to successfully parse all tokens in stream"
    );
    info!("Program parsed successfully");
    Ok(Parser { head })
}

fn parse_global_scope(tokens: &mut Lexer) -> Result<ParseNode, ParserError> {
    let mut node = ParseNode::Scope(ScopeNode::new());
    while tokens.peek().token_type != TokenType::EOF {
        let next = tokens.peek();
        let stmt = match next.token_type {
            TokenType::Function => construct_func(tokens)?,
            TokenType::Specifier => construct_decl(tokens, false)?,
            _ => {
                error!("Found illegal token type: {}", next.token_type);
                return Err(ParserError::new("Illegal program statement"));
            }
        };
        node.push_body(stmt)?;
    }
    tokens.consume();
    Ok(node)
}

/// Pratt parsing
fn construct_expr(tokens: &mut Lexer, min_power: usize) -> Result<ParseNode, ParserError> {
    let token = tokens.consume();
    let Some(value) = &token.lexeme else {
        return Err(ParserError::new(
            "Invalid lexer value. Possibly unsupported?",
        ));
    };

    let mut lhs: ParseNode = match &token.token_type {
        TokenType::Identifier => {
            if tokens.peek().lexeme.as_deref() == Some("(") {
                parse_func_call(tokens, &value)?
            } else {
                ParseNode::Value(Value::new(value))
            }
        }
        TokenType::Boolean => {
            let val = match value.as_str() {
                "true" => true,
                "false" => false,
                _ => return Err(ParserError::new("Invalid boolean value")),
            };
            ParseNode::Constant(ConstantNode::Bool(val))
        }
        TokenType::Integer => {
            let val = value
                .parse::<i64>()
                .map_err(|_| ParserError::new("Invalid integer constant"))?;
            ParseNode::Constant(ConstantNode::Int(val))
        }
        TokenType::Decimal => {
            let val = value
                .parse::<f64>()
                .map_err(|_| ParserError::new("Invalid float constant"))?;
            ParseNode::Constant(ConstantNode::Float(val))
        }
        TokenType::Operator => {
            let ((), r_bp) = pre_binding_power(&value).unwrap_or_default();
            let rhs = construct_expr(tokens, r_bp)?;
            ParseNode::Unary(UnaryNode::new(token.clone(), rhs))
        }
        TokenType::GroupBegin => match token.lexeme.as_deref() {
            Some("(") => {
                let lhs = construct_expr(tokens, 0)?;
                assert_eq!(
                    tokens.consume(),
                    Token::new(TokenType::GroupEnd, ")".to_string())
                );
                lhs
            }
            _ => return Err(ParserError::new("possibly incorrect group begin placement")),
        },
        _ => {
            error!("Found token type {}", token.token_type);
            return Err(ParserError::new("unsupported parser type."));
        }
    };

    loop {
        let next_token = tokens.peek();
        let next_token_type = next_token.token_type;

        match next_token_type {
            TokenType::GroupEnd => match next_token.lexeme.as_deref() {
                Some("]") | Some("}") => {
                    // info!("Encountered group end, exiting from construct_expr");
                    break;
                }
                _ => return Err(ParserError::new("unexpected group end token")),
            },
            TokenType::Separator => {
                if let Some(val) = &next_token.lexeme {
                    match val.as_str() {
                        ";" | "," => break, // let caller handle consuming
                        _ => return Err(ParserError::new("unexpected separator")),
                    }
                }
            }
            TokenType::EOF => break,
            _ => {}
        }

        let next_token_lexeme: Option<String> = match next_token_type {
            TokenType::GroupBegin => {
                match next_token.lexeme.as_deref() {
                    Some("[") => {
                        let (l_bp, r_bp) = (36, 37);
                        if l_bp < min_power {
                            break;
                        }
                        _ = tokens.consume(); // [
                        let index = construct_expr(tokens, r_bp)?;
                        assert_eq!(tokens.consume().lexeme.as_deref(), Some("]"));

                        // info!("Parser found array access");
                        lhs = ParseNode::Binary(BinaryNode::new(
                            Token::new(TokenType::Operator, "[]".to_string()),
                            lhs,
                            index,
                        ));
                        continue;
                    }
                    Some("(") => {
                        lhs = parse_func_call(tokens, value)?;
                        continue;
                    }
                    _ => break,
                }
            }
            TokenType::Operator => tokens.peek().lexeme.clone(),
            _ => {
                warn!("{}", next_token_type);
                return Err(ParserError::new(
                    "Bad, unsupported, or unrecognised lexer value.",
                ));
            }
        };

        if let Some(value) = next_token_lexeme {
            if let Some((l_bp, r_bp)) = set_binding_power(&value) {
                if l_bp < min_power {
                    break;
                }
                _ = tokens.consume();
                let rhs = construct_expr(tokens, r_bp)?;
                lhs = ParseNode::Binary(BinaryNode::new(next_token, lhs, rhs));
                continue;
            }
        }
        break;
    }
    Ok(lhs)
}

enum Associativity {
    Left,
    Right,
}

/// Returns associativity, precedence
fn get_operator_info(op: &str) -> Option<(Associativity, usize)> {
    match op {
        "=" => Some((Associativity::Right, 2)),
        "or" => Some((Associativity::Left, 7)),
        "and" => Some((Associativity::Left, 9)),
        "|" => Some((Associativity::Left, 11)),
        "^" => Some((Associativity::Left, 13)),
        "&" => Some((Associativity::Left, 15)),
        "==" | "!=" => Some((Associativity::Left, 17)),
        ">" | "<" => Some((Associativity::Left, 19)),
        "<<" | ">>" => Some((Associativity::Left, 21)),
        "+" | "-" => Some((Associativity::Left, 27)),
        "*" | "/" | "%" => Some((Associativity::Left, 31)),
        _ => None,
    }
}

/// Pratt parsing binding power for binary operations.
fn set_binding_power(op_str: &String) -> Option<(usize, usize)> {
    if let Some((assoc, pow)) = get_operator_info(op_str) {
        match assoc {
            Associativity::Left => Some((pow, pow + 1)),
            Associativity::Right => Some((pow, pow - 1)),
        }
    } else {
        None
    }
}

/// Pratt parsing binding power for unary pre-operations.
fn pre_binding_power(op_str: &String) -> Option<((), usize)> {
    let op = op_str.as_str();
    match op {
        "+" | "-" => Some(((), 3)),
        "~" => Some(((), 20)),
        _ => None,
    }
}

pub enum ParseNode {
    Constant(ConstantNode),
    Function(FunctionNode),
    FunctionCall(FunctionCallNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Value(Value),
    ScalarDecl(ScalarDeclNode),
    ArrDecl(ArrayDeclNode),
    Return(ReturnNode),
    Control(ControlNode),
    While(WhileNode),
    For(ForNode),
    LoopControl(LoopControlNode), // break, continue
    Scope(ScopeNode),
    None,
}

/// Constant node. Stores value
pub enum ConstantNode {
    Bool(bool),
    Int(i64),
    Float(f64),
}

fn parse_func_call(tokens: &mut Lexer, value: &String) -> Result<ParseNode, ParserError> {
    _ = tokens.consume(); // (
    let mut args = Vec::new();

    while tokens.peek().lexeme.as_deref() != Some(")") {
        args.push(construct_expr(tokens, 0)?);
        if tokens.peek().lexeme.as_deref() == Some(",") {
            tokens.consume();
        }
    }
    _ = tokens.consume(); // )
    Ok(ParseNode::FunctionCall(FunctionCallNode::new(
        ParseNode::Value(Value::new(&value)),
        args,
    )))
}

/// Constructs a function node.
fn construct_func(tokens: &mut Lexer) -> Result<ParseNode, ParserError> {
    // info!("Parser found function");
    _ = tokens.consume(); // fn
    let mut node = FunctionNode::new(&tokens.consume());
    assert_eq!(
        tokens.consume().lexeme.as_deref(),
        Some("{"),
        "function declaration must have parameter list (even if empty)"
    );

    // function arguments
    while tokens.peek().lexeme.as_deref() != Some("}") {
        node.push_param(construct_decl(tokens, true)?);
        // info!("Parsed single function parameter");
        if tokens.peek().lexeme.as_deref() == Some(",") {
            tokens.consume(); // ,
        }
    }
    // info!("Function arguments parsed");
    assert_eq!(
        tokens.consume().lexeme.as_deref(),
        Some("}"),
        "parameter list missing closing brackets"
    );

    let return_type = tokens.consume();
    if return_type.token_type != TokenType::Type {
        return Err(ParserError::new(
            "function must have return type. If no return, use void",
        ));
    }
    node.set_return(return_type);

    // function body
    check_starting_bracket(tokens, "function");
    node.set_body(construct_group_expr(tokens)?);
    Ok(ParseNode::Function(node))
}

/// Constructs a {scalar, array} declaration node.
/// Function flag denotes a function declaration if true.
fn construct_decl(tokens: &mut Lexer, function: bool) -> Result<ParseNode, ParserError> {
    // info!("Constructing declaration");
    let specifier = tokens.consume();
    let type_token = tokens.consume();
    assert_eq!(
        type_token.token_type,
        TokenType::Type,
        "after a variable specifier, there must be a type annotation"
    );

    // check if array decl
    let result = if tokens.peek().lexeme.as_deref() == Some("[") {
        _ = tokens.consume(); // [
                              // info!("Parser found array declaration");
        let size = construct_expr(tokens, 0)?;
        // info!("Parser found array size");
        assert_eq!(
            tokens.consume().lexeme.as_deref(),
            Some("]"),
            "an array dereference must be closed by a corresponding square bracket"
        );
        let id = tokens.consume();
        assert_eq!(id.token_type, TokenType::Identifier);

        let init = if !function {
            _ = tokens.consume(); // =
            Some(construct_expr(tokens, 0)?)
        } else {
            None
        };
        let node = ArrayDeclNode::new(
            specifier,
            type_token,
            init,
            size,
            id.lexeme.unwrap_or_default(),
        );
        Ok(ParseNode::ArrDecl(node))
    } else {
        let id = tokens.consume();
        assert_eq!(id.token_type, TokenType::Identifier);
        let init = if !function {
            _ = tokens.consume(); // =
            Some(construct_expr(tokens, 0)?)
        } else {
            None
        };
        let node = ScalarDeclNode::new(specifier, type_token, init, id.lexeme.unwrap_or_default());
        Ok(ParseNode::ScalarDecl(node))
    }?;

    if !function {
        assert_eq!(
            tokens.consume().lexeme.as_deref(),
            Some(";"),
            "declaration must end with a semicolon"
        );
    }
    Ok(result)
}

fn construct_return(tokens: &mut Lexer) -> Result<ParseNode, ParserError> {
    _ = tokens.consume(); // return
    let next = tokens.peek();
    let expr: Option<ParseNode> = match next.token_type {
        TokenType::Separator => {
            assert_eq!(
                tokens.consume().lexeme.as_deref(),
                Some(";"),
                "invalid separator used for void return. Only semi-colons are permitted for void returns"
            );
            None
        }
        _ => {
            let result = Some(construct_expr(tokens, 0)?);
            assert_eq!(
                tokens.consume().lexeme.as_deref(),
                Some(";"),
                "invalid separator. lines should end with semi-colons"
            );
            result
        }
    };
    Ok(ParseNode::Return(ReturnNode::new(expr)))
}

pub struct WhileNode {
    pub cond: Box<ParseNode>,
    pub then: Box<ParseNode>,
}

fn construct_control(tokens: &mut Lexer) -> Result<ParseNode, ParserError> {
    let token = tokens.consume();
    return match token.lexeme.as_deref() {
        Some("if") => {
            check_starting_bracket(tokens, "if");
            let condition = construct_expr(tokens, 0)?;
            check_ending_bracket(tokens, "if");
            let then = construct_group_expr(tokens)?;
            let mut node = ControlNode::new(condition, then);

            // elif branches
            while tokens.peek().lexeme.as_deref() == Some("elif") {
                _ = tokens.consume(); // elif
                check_starting_bracket(tokens, "elif");
                let elif_condition = construct_expr(tokens, 0)?;
                check_ending_bracket(tokens, "elif");
                let elif_then = construct_group_expr(tokens)?;
                node.push_elif(elif_condition, elif_then);
            }

            // else branch
            if tokens.peek().lexeme.as_deref() == Some("else") {
                _ = tokens.consume(); // else
                check_starting_bracket(tokens, "else");
                let else_then = construct_group_expr(tokens)?;
                check_ending_bracket(tokens, "else");
                node.set_else(else_then);
            }
            Ok(ParseNode::Control(node))
        }
        Some("while") => {
            _ = tokens.consume();
            check_starting_bracket(tokens, "while");
            let cond = construct_expr(tokens, 0)?;
            check_ending_bracket(tokens, "while");

            // then
            check_starting_bracket(tokens, "while");
            let then = construct_group_expr(tokens)?;
            check_ending_bracket(tokens, "while");

            // construct
            let node = WhileNode::new(cond, then);
            Ok(ParseNode::While(node))
        }
        Some("for") => {
            _ = tokens.consume();
            check_starting_bracket(tokens, "for");
            let pre = construct_expr(tokens, 0)?;
            assert_eq!(tokens.consume().lexeme.as_deref(), Some(";"));
            let cond = construct_expr(tokens, 0)?;
            assert_eq!(tokens.consume().lexeme.as_deref(), Some(";"));
            let post = construct_expr(tokens, 0)?;
            check_ending_bracket(tokens, "for");

            // then
            check_starting_bracket(tokens, "for");
            let then = construct_group_expr(tokens)?;
            check_ending_bracket(tokens, "for");

            let node = ForNode::new(pre, cond, post, then);
            Ok(ParseNode::For(node))
        }
        Some("continue") | Some("break") => Ok(ParseNode::LoopControl(LoopControlNode::new(token))),
        _ => Err(ParserError::new(
            "unknown control/loop statement or mismatched order",
        )),
    };
}

trait Group {
    fn push_body(&mut self, node: ParseNode) -> Result<(), ParserError>;
}

impl Group for ParseNode {
    fn push_body(&mut self, node: ParseNode) -> Result<(), ParserError> {
        match self {
            ParseNode::Scope(n) => n.push_body(node),
            _ => {
                return Err(ParserError::new(
                    "Internal error: unexpected parser node type.",
                ))
            }
        }
        Ok(())
    }
}

/// Parse tree generation for grouped constructs (functions, scopes).
fn construct_group_expr(tokens: &mut Lexer) -> Result<ParseNode, ParserError> {
    let mut node = ParseNode::Scope(ScopeNode::new());
    loop {
        let next = tokens.peek();
        node.push_body(match next.token_type {
            TokenType::Specifier => construct_decl(tokens, false)?,
            TokenType::GroupBegin => match next.lexeme.as_deref() {
                Some("{") => construct_group_expr(tokens)?,
                _ => return Err(ParserError::new("illegal group begin")),
            },
            TokenType::Function => {
                return Err(ParserError::new(
                    "illegal expression. Cannot define a function within another function",
                ))
            }
            TokenType::Type | TokenType::Separator => {
                return Err(ParserError::new("illegal expression. Misplaced token type"));
            }
            TokenType::Integer
            | TokenType::Decimal
            | TokenType::Identifier
            | TokenType::Operator
            | TokenType::Boolean => construct_expr(tokens, 0)?,
            TokenType::Return => construct_return(tokens)?,
            TokenType::None => {
                return Err(ParserError::new(
                    "Internal parser error: received unexpected None token type",
                ))
            }
            TokenType::EOF => {
                return Err(ParserError::new(
                    "parser reached end of file before function body closing bracket",
                ))
            }
            TokenType::Control => construct_control(tokens)?,
            TokenType::GroupEnd => {
                // should be closing bracket for scope
                check_ending_bracket(tokens, "scope");
                break;
            }
        })
        .expect("Internal error: parser panicked on group expression construction");
    }
    Ok(node)
}

fn check_starting_bracket(tokens: &mut Lexer, stmt_type: &str) {
    assert_eq!(
        tokens.consume().lexeme.as_deref(),
        Some("{"),
        "expected an opening bracket for {} statement",
        stmt_type
    );
}

fn check_ending_bracket(tokens: &mut Lexer, stmt_type: &str) {
    assert_eq!(
        tokens.consume().lexeme.as_deref(),
        Some("}"),
        "expected a closing bracket for {} statement",
        stmt_type
    );
}
