use std::error::Error;

use log::info;

use crate::lexer::{Lexer, Token, TokenType};
use crate::parse::util::ParserError;

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
            _ => return Err(ParserError::new("Illegal program statement")),
        };
        node.push_body(stmt)?;
    }
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
        TokenType::Identifier => ParseNode::Value(Value::new(value)),
        TokenType::Boolean => {
            let val = match value.as_str() {
                "true" => true,
                "false" => false,
                _ => return Err(ParserError::new("Error: Invalid boolean value")),
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
            let ((), r_bp) = pre_binding_power(&value)?;
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
            _ => {
                return Err(ParserError::new(
                    "Error: possibly incorrect group begin placement",
                ))
            }
        },
        _ => return Err(ParserError::new("Error: unsupported parser type.")),
    };

    loop {
        let next_token = tokens.peek();
        let next_token_type = next_token.token_type;
        let next_token_lexeme: Option<String> = match next_token_type {
            TokenType::EOF => break,
            TokenType::Separator => {
                if let Some(val) = next_token.lexeme {
                    if val == ";" {
                        tokens.consume();
                        break;
                    } else {
                        // to implement
                        break;
                    }
                } else {
                    break;
                }
            }
            TokenType::GroupBegin => tokens.peek().lexeme.clone(),
            TokenType::GroupEnd => {
                if let Some(val) = &next_token.lexeme {
                    match val.as_str() {
                        "}" => break,
                        _ => tokens.peek().lexeme.clone(),
                    }
                } else {
                    return Err(ParserError::new("Bad match arm."));
                }
            }
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
fn pre_binding_power(op_str: &String) -> Result<((), usize), ParserError> {
    let op = op_str.as_str();
    match op {
        "+" | "-" => Ok(((), 3)),
        "~" => Ok(((), 20)),
        _ => {
            println!("{}", op_str);
            return Err(ParserError::new(
                "Bad, unsupported, or unrecognised lexer operator",
            ));
        }
    }
}

/// Pratt parsing binding power for array de-references.
fn post_binding_power(op_str: &String) -> Option<(usize, ())> {
    let op = op_str.as_str();
    match op {
        "[" => Some((36, ())),
        _ => return None,
    }
}

enum ParseNode {
    Constant(ConstantNode),
    Function(FunctionNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Value(Value),
    ScalarDecl(ScalarDeclNode),
    ArrayDecl(ArrayDeclNode),
    Expr(ExprNode),
    Return(ReturnNode),
    Control(ControlNode),
    While(WhileNode),
    For(ForNode),
    LoopControl(LoopControlNode), // break, continue
    Scope(ScopeNode),
    None,
}

/// Constant node. Stores value
enum ConstantNode {
    Bool(bool),
    Int(i64),
    Float(f64),
}

struct FunctionNode {
    name: Token,
    params: Vec<ParseNode>,
    body: Box<ParseNode>,
}

impl FunctionNode {
    fn new(name: &Token) -> FunctionNode {
        FunctionNode {
            name: name.clone(),
            params: Vec::new(),
            body: Box::new(ParseNode::None),
        }
    }

    fn set_body(&mut self, scope: ParseNode) {
        self.body = Box::new(scope);
    }

    fn push_param(&mut self, param: ParseNode) {
        self.params.push(param)
    }
}

/// Constructs a function node.
fn construct_func(tokens: &mut Lexer) -> Result<ParseNode, ParserError> {
    _ = tokens.consume(); // fn
    let mut node = FunctionNode::new(&tokens.consume());
    assert_eq!(
        tokens.consume().lexeme.as_deref(),
        Some("{"),
        "Error: function declaration must have parameter list (even if empty)"
    );

    // function arguments
    while tokens.peek().token_type != TokenType::GroupEnd {
        node.push_param(construct_decl(tokens, true)?);
        if tokens.peek().token_type == TokenType::Separator {
            tokens.consume(); // ,
        }
    }
    assert_eq!(
        tokens.consume().lexeme.as_deref(),
        Some("}"),
        "Error: parameter list missing closing brackets"
    );

    let return_type = tokens.consume();
    if return_type.token_type != TokenType::Type {
        return Err(ParserError::new(
            "Error: function must have return type. If no return, use void",
        ));
    }

    // function body
    check_starting_bracket(tokens, "function");
    node.set_body(construct_group_expr(tokens)?);
    Ok(ParseNode::Function(node))
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

struct ScalarDeclNode {
    specifier: Token,
    _type: Token,
    id: String,
}

impl ScalarDeclNode {
    fn new(specifier: Token, _type: Token, id: String) -> ScalarDeclNode {
        ScalarDeclNode {
            specifier,
            _type,
            id,
        }
    }
}

struct ArrayDeclNode {
    specifier: Token,
    _type: Token,
    size: Box<ParseNode>,
    id: String,
}

impl ArrayDeclNode {
    fn new(specifier: Token, _type: Token, size: ParseNode, id: String) -> ArrayDeclNode {
        ArrayDeclNode {
            specifier,
            _type,
            size: Box::new(size),
            id,
        }
    }
}

/// Constructs a {scalar, array} declaration node.
/// Function flag denotes a function declaration if true.
fn construct_decl(tokens: &mut Lexer, function: bool) -> Result<ParseNode, ParserError> {
    let specifier = tokens.consume();
    let type_token = tokens.consume();
    assert_eq!(
        type_token.token_type,
        TokenType::Type,
        "Error: after a variable specifier, there must be a type annotation"
    );

    // check if array decl
    return if tokens.peek().lexeme.as_deref() == Some("[") {
        _ = tokens.consume(); // [
        let size = construct_expr(tokens, 0)?;
        assert_eq!(
            tokens.consume().lexeme.as_deref(),
            Some("]"),
            "Error: an array dereference must be closed by a corresponding square bracket"
        );
        let id = tokens.consume();
        if !function {
            assert_eq!(
                tokens.consume().lexeme.as_deref(),
                Some(";"),
                "Error: a declaration must end with a semi-colon"
            );
        }
        let node = ArrayDeclNode::new(specifier, type_token, size, id.lexeme.unwrap());
        Ok(ParseNode::ArrayDecl(node))
    } else {
        let id = tokens.consume();
        if !function {
            assert_eq!(
                tokens.consume().lexeme.as_deref(),
                Some(";"),
                "Error: a declaration must end with a semi-colon"
            );
        }
        let node = ScalarDeclNode::new(specifier, type_token, id.lexeme.unwrap());
        Ok(ParseNode::ScalarDecl(node))
    };
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

struct ReturnNode {
    expr: Option<Box<ParseNode>>,
}

impl ReturnNode {
    fn new(expr: Option<ParseNode>) -> ReturnNode {
        return if expr.is_some() {
            ReturnNode {
                expr: Some(Box::new(expr.unwrap())),
            }
        } else {
            ReturnNode { expr: None }
        };
    }
}

fn construct_return(tokens: &mut Lexer) -> Result<ParseNode, ParserError> {
    _ = tokens.consume(); // return
    let next = tokens.peek();
    let expr: Option<ParseNode> = match next.token_type {
        TokenType::Separator => {
            assert_eq!(
                tokens.consume().lexeme.as_deref(),
                Some(";"),
                "Error: invalid separator used for void return. Only semi-colons are permitted for void returns"
            );
            None
        }
        _ => Some(construct_expr(tokens, 0)?),
    };
    Ok(ParseNode::Return(ReturnNode::new(expr)))
}

struct BasicControlNode {
    cond: Box<ParseNode>,
    then: Box<ParseNode>,
}

impl BasicControlNode {
    fn new(cond: ParseNode, then: ParseNode) -> BasicControlNode {
        BasicControlNode {
            cond: Box::new(cond),
            then: Box::new(then),
        }
    }
}

struct ControlNode {
    _if: BasicControlNode,
    elif: Option<Vec<BasicControlNode>>,
    el: Option<Box<ParseNode>>,
}

impl ControlNode {
    fn new(cond: ParseNode, then: ParseNode) -> ControlNode {
        ControlNode {
            _if: BasicControlNode::new(cond, then),
            elif: None,
            el: None,
        }
    }

    fn push_elif(&mut self, cond: ParseNode, then: ParseNode) {
        self.elif
            .get_or_insert(Vec::new())
            .push(BasicControlNode::new(cond, then))
    }

    fn set_else(&mut self, el: ParseNode) {
        self.el = Some(Box::new(el))
    }
}

struct WhileNode {
    cond: Box<ParseNode>,
    then: Box<ParseNode>,
}

impl WhileNode {
    fn new(cond: ParseNode, then: ParseNode) -> WhileNode {
        WhileNode {
            cond: Box::new(cond),
            then: Box::new(then),
        }
    }
}

struct ForNode {
    pre: Box<ParseNode>,
    cond: Box<ParseNode>,
    post: Box<ParseNode>,
    then: Box<ParseNode>,
}

impl ForNode {
    fn new(pre: ParseNode, cond: ParseNode, post: ParseNode, then: ParseNode) -> ForNode {
        ForNode {
            pre: Box::new(pre),
            cond: Box::new(cond),
            post: Box::new(post),
            then: Box::new(then),
        }
    }
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
            "Error: unknown control/loop statement or mismatched order",
        )),
    };
}

struct LoopControlNode {
    _type: Token,
}

impl LoopControlNode {
    fn new(_type: Token) -> LoopControlNode {
        LoopControlNode { _type }
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
                _ => return Err(ParserError::new("Error: illegal group begin")),
            },
            TokenType::Function => {
                return Err(ParserError::new(
                    "Error: illegal expression. Cannot define a function within another function",
                ))
            }
            TokenType::Type | TokenType::Separator => {
                return Err(ParserError::new(
                    "Error: illegal expression. Misplaced token type",
                ));
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
                    "Error: parser reached end of file before function body closing bracket",
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
        "Error: expected an opening bracket for {} statement",
        stmt_type
    );
}

fn check_ending_bracket(tokens: &mut Lexer, stmt_type: &str) {
    assert_eq!(
        tokens.consume().lexeme.as_deref(),
        Some("}"),
        "Error: expected a closing bracket for {} statement",
        stmt_type
    );
}
