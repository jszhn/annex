use crate::lexer::{Token, TokenStream, TokenType};
use crate::parse::structs::*;
use crate::parse::util::ParserError;
use log::{error, info, warn};
use std::error::Error;

pub mod structs;
mod util;

pub struct ParseTree {
    pub head: ParseNode,
}

impl ParseTree {
    pub fn new(tokens: TokenStream) -> Result<Self, Box<dyn Error>> {
        let mut parser = Parser::new(tokens);
        let head = parser.start()?;

        assert!(
            parser.tokens.is_empty(),
            "Internal error: parser failed to successfully parse all tokens in stream"
        );

        Ok(ParseTree { head })
    }
}

struct Parser {
    tokens: TokenStream,
}

impl Parser {
    fn new(tokens: TokenStream) -> Self {
        Self { tokens }
    }

    /// Wrapper method for the TokenStream consume() method.
    fn consume(&mut self) -> Token {
        self.tokens.consume()
    }

    /// Wrapper method for the TokenStream peek() method.
    fn peek(&mut self) -> Token {
        self.tokens.peek()
    }

    /// Parses the program's global scope. Only either function or symbol definitions are permitted here.
    fn start(&mut self) -> Result<ParseNode, ParserError> {
        let mut node = ParseNode::Scope(ScopeNode::new());

        loop {
            let next = self.peek();
            let stmt = match &next.typ {
                typ if typ.is_specifier() => self.decl_stmt(false)?,
                TokenType::Function => self.function()?,
                TokenType::EOF => break,
                _ => {
                    error!("[Parser]: found illegal token type: {}", next.typ);
                    return Err(ParserError::new("Illegal program statement"));
                }
            };
            node.push_body(stmt)?;
        }

        self.consume(); // EOF
        Ok(node)
    }

    fn check_starting_bracket(&mut self, stmt_type: &str) {
        assert_eq!(
            match self.consume().typ {
                TokenType::GroupBegin(grp) => Some(grp),
                _ => None,
            },
            Some('{'),
            "expected an opening bracket for {} statement",
            stmt_type
        );
    }

    fn check_ending_bracket(&mut self, stmt_type: &str) {
        assert_eq!(
            match self.consume().typ {
                TokenType::GroupEnd(grp) => Some(grp),
                _ => None,
            },
            Some('}'),
            "expected a closing bracket for {} statement",
            stmt_type
        );
    }
}

trait ParseGroups {
    fn groups(&mut self) -> Result<ParseNode, ParserError>;
    fn function(&mut self) -> Result<ParseNode, ParserError>;
}

impl ParseGroups for Parser {
    /// Parse tree generation for grouped constructs (functions, scopes).
    fn groups(&mut self) -> Result<ParseNode, ParserError> {
        let mut node = ParseNode::Scope(ScopeNode::new());

        loop {
            let next = self.peek();
            let body = match next.typ {
                // type categories
                typ if typ.is_specifier() => self.decl_stmt(false)?,
                typ if typ.is_literal() => {
                    let result = self.expr(0)?;
                    self.consume();
                    result
                }
                typ if typ.is_control() => self.control()?,
                // delineators
                TokenType::GroupBegin(val) => match val {
                    '{' => self.groups()?,
                    _ => return Err(ParserError::new("illegal group begin")),
                },
                TokenType::GroupEnd(_) => {
                    // should be closing bracket for scope
                    self.check_ending_bracket("scope");
                    break;
                }
                // others
                TokenType::Return => self.return_stmt()?,
                TokenType::EOF => {
                    break;
                }
                // illegal keywords
                TokenType::Function => {
                    return Err(ParserError::new(
                        "illegal expression. Cannot define a function within another function",
                    ))
                }
                TokenType::Type(val) => {
                    error!("[Parser]: found token {}", val);
                    return Err(ParserError::new("illegal expression. Misplaced token type"));
                }
                TokenType::Separator(val) => {
                    error!("[Parser]: found token {}", val);
                    return Err(ParserError::new("illegal expression. Misplaced token type"));
                }
                _ => unreachable!(),
            };
            node.push_body(body)?;
        }
        Ok(node)
    }

    /// Constructs a function node.
    fn function(&mut self) -> Result<ParseNode, ParserError> {
        match self.consume().typ {
            TokenType::Function => {}
            _ => return Err(ParserError::new("Illegal keyword")),
        }

        let id_tok = self.consume();
        let id = match id_tok.typ {
            TokenType::Identifier(id) => id,
            _ => return Err(ParserError::new("Functions must have an identifier symbol")),
        };

        assert_eq!(
            self.consume().get_char(),
            Some('{'),
            "function declaration must have parameter list (even if empty)"
        );

        let mut args: Vec<ParseNode> = Vec::new();
        while self.peek().get_char() != Some('}') {
            args.push(self.decl_stmt(true)?);
            if self.peek().get_char() == Some(',') {
                self.consume();
            }
        }

        assert_eq!(
            self.consume().get_char(),
            Some('}'),
            "parameter list missing closing brackets"
        );

        let ret_tok = self.consume();
        let ret = match ret_tok.typ {
            TokenType::Type(typ) => typ,
            _ => {
                return Err(ParserError::new(
                    "function must have return type. If no return, use void",
                ))
            }
        };

        // function body
        self.check_starting_bracket("function");
        let body = self.groups()?;

        Ok(ParseNode::Function(FunctionNode::new(id, ret, args, body)))
    }
}

/// Single-use trait to group parser methods that construct expression nodes.
trait ParseExpr {
    fn expr(&mut self, min_power: usize) -> Result<ParseNode, ParserError>;
    fn function_call(&mut self, value: &String) -> Result<ParseNode, ParserError>;
}

impl ParseExpr for Parser {
    /// Pratt parsing for expressions. Adapted from this
    /// [great article](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
    /// by matklad.
    fn expr(&mut self, min_power: usize) -> Result<ParseNode, ParserError> {
        let token = self.consume();
        match token.typ {
            TokenType::Function | TokenType::Return | TokenType::EOF => {
                return Err(ParserError::new(
                    "Invalid lexer value. Possibly unsupported?",
                ))
            }
            _ => {}
        }
        let mut id_val: String = Default::default();

        let mut lhs: ParseNode = match &token.typ {
            TokenType::Boolean(val) => ParseNode::Constant(ConstantNode::Bool(*val)),
            TokenType::Integer(val) => ParseNode::Constant(ConstantNode::Int(*val)),
            TokenType::Decimal(val) => ParseNode::Constant(ConstantNode::Float(*val)),
            TokenType::Identifier(val) => {
                id_val = val.clone();
                match self.peek().typ {
                    TokenType::GroupBegin(grp) => match grp {
                        '(' => self.function_call(val)?,
                        _ => unreachable!(), // todo: panic!
                    },
                    _ => ParseNode::Value(Value::new(val)),
                }
            }
            TokenType::Operator(val) => {
                let ((), r_bp) = pre_binding_power(val).unwrap_or_default();
                let rhs = self.expr(r_bp)?;
                ParseNode::Unary(UnaryNode::new(val.clone(), rhs))
            }
            TokenType::GroupBegin(val) => match val {
                '(' => {
                    let lhs = self.expr(0)?;
                    assert!(matches!(self.consume().get_char(), Some(')')));
                    lhs
                }
                _ => return Err(ParserError::new("possibly incorrect group begin placement")),
            },
            _ => {
                error!("[Parser]: found token type {}", token.typ);
                return Err(ParserError::new("unsupported parser type."));
            }
        };

        loop {
            let next = self.peek();
            match next.typ {
                TokenType::GroupEnd(val) => match val {
                    ']' | '}' => {
                        info!("[Parser]: encountered group end, exiting from self.expr");
                        break;
                    }
                    _ => return Err(ParserError::new("unexpected group end token")),
                },
                TokenType::Separator(val) => match val {
                    ';' | ',' => break, // let caller handle consuming
                    _ => return Err(ParserError::new("unexpected separator")),
                },
                TokenType::EOF => break,
                _ => {}
            }

            let next_lexeme: Option<String> = match &next.typ {
                TokenType::Operator(val) => Some(val.clone()),
                TokenType::GroupBegin(val) => match val {
                    '[' => {
                        let (l_bp, r_bp) = (36, 37);
                        if l_bp < min_power {
                            break;
                        }
                        _ = self.consume(); // [
                        let index = self.expr(r_bp)?;
                        assert_eq!(self.consume().get_char(), Some(']'));

                        lhs = ParseNode::Binary(BinaryNode::new("[]".to_string(), lhs, index));
                        continue;
                    }
                    '(' => {
                        assert_ne!(id_val, String::default());
                        lhs = self.function_call(&id_val)?;
                        continue;
                    }
                    _ => break,
                },
                _ => {
                    warn!("[Parser]: {}", next.typ);
                    return Err(ParserError::new(
                        "Bad, unsupported, or unrecognised lexer value.",
                    ));
                }
            };

            if let Some(value) = next_lexeme {
                if let Some((l_bp, r_bp)) = set_binding_power(&value) {
                    if l_bp < min_power {
                        break;
                    }
                    _ = self.consume();
                    let rhs = self.expr(r_bp)?;
                    lhs = ParseNode::Binary(BinaryNode::new(value, lhs, rhs));
                    continue;
                }
            }
            break;
        }
        Ok(lhs)
    }

    /// Parses a function call.
    fn function_call(&mut self, value: &String) -> Result<ParseNode, ParserError> {
        _ = self.consume(); // (
        let mut args = Vec::new();

        // todo: make this more functional
        // while typ == TokenType::GroupBegin && val == ')'
        while match self.peek().typ {
            TokenType::GroupBegin(val) => !matches!(val, ')'),
            _ => true,
        } {
            args.push(self.expr(0)?);
            if match self.peek().typ {
                TokenType::Separator(sep) => matches!(sep, ','),
                _ => false,
            } {
                self.consume();
            }
        }
        _ = self.consume(); // )

        Ok(ParseNode::FunctionCall(FunctionCallNode::new(
            ParseNode::Value(Value::new(value)),
            args,
        )))
    }
}

trait ParseStmt {
    fn return_stmt(&mut self) -> Result<ParseNode, ParserError>;
    fn decl_stmt(&mut self, function: bool) -> Result<ParseNode, ParserError>;
}

impl ParseStmt for Parser {
    fn return_stmt(&mut self) -> Result<ParseNode, ParserError> {
        // return token
        let ret = self.consume();
        match ret.typ {
            TokenType::Return => {}
            _ => return Err(ParserError::new("expected return statement")),
        };

        // parse next statement
        let next = self.peek();
        let expr = match next.typ {
            TokenType::Separator(sep) => {
                assert_eq!(
                    sep,
                    ';',
                    "invalid separator used for void return. Only semi-colons are permitted for void returns"
                );
                None
            }
            _ => {
                let result = Some(self.expr(0)?);
                assert_eq!(
                    match self.consume().typ {
                        TokenType::Separator(sep) => {
                            Some(sep)
                        }
                        _ => None,
                    },
                    Some(';'),
                    "invalid separator. lines should end with semi-colons"
                );
                result
            }
        };

        Ok(ParseNode::Return(ReturnNode::new(expr)))
    }

    /// Constructs a {scalar, array} declaration node.
    /// Function flag denotes a function declaration if true.
    fn decl_stmt(&mut self, function: bool) -> Result<ParseNode, ParserError> {
        let specifier = self.consume();

        let typ_tok = self.consume();
        let typ = match typ_tok.typ {
            TokenType::Type(val) => val,
            _ => return Err(ParserError::new("expected type after specifier")),
        };

        // check if array decl
        let result = if self.peek().get_char() == Some('[') {
            _ = self.consume(); // [
            let size = self.expr(0)?;
            assert_eq!(
                self.consume().get_char(),
                Some(']'),
                "an array dereference must be closed by a corresponding square bracket"
            );

            let id_tok = self.consume();
            let id = match id_tok.typ {
                TokenType::Identifier(id) => Some(id),
                _ => None,
            };
            assert!(id.is_some(), "symbol definition should be named");

            let init = if !function {
                _ = self.consume(); // =
                Some(self.expr(0)?)
            } else {
                None
            };

            let node = ArrayDeclNode::new(specifier, typ, init, size, id.unwrap_or_default());

            Ok(ParseNode::ArrDecl(node))
        } else {
            let id_tok = self.consume();
            let id = match id_tok.typ {
                TokenType::Identifier(id) => Some(id),
                _ => None,
            };
            assert!(id.is_some(), "symbol definition should be named");

            let init = if !function {
                _ = self.consume(); // =
                Some(self.expr(0)?)
            } else {
                None
            };
            let node = ScalarDeclNode::new(specifier, typ, init, id.unwrap_or_default());
            Ok(ParseNode::ScalarDecl(node))
        }?;

        if !function {
            assert_eq!(
                self.consume().get_char(),
                Some(';'),
                "declaration must end with a semicolon"
            );
        }
        Ok(result)
    }
}

trait ParseControl {
    fn control(&mut self) -> Result<ParseNode, ParserError>;
    fn control_if(&mut self) -> Result<ParseNode, ParserError>;
    fn control_for(&mut self) -> Result<ParseNode, ParserError>;
    fn control_while(&mut self) -> Result<ParseNode, ParserError>;
}

impl ParseControl for Parser {
    /// Entry point for all control-flow and looping mechanisms.
    fn control(&mut self) -> Result<ParseNode, ParserError> {
        let token = self.consume();

        match token.typ {
            TokenType::If => self.control_if(),
            TokenType::While => self.control_while(),
            TokenType::For => self.control_for(),
            TokenType::Continue | TokenType::Break => {
                Ok(ParseNode::LoopControl(LoopControlNode::new(token)))
            }
            _ => Err(ParserError::new(
                "unknown control/loop statement or mismatched order",
            )),
        }
    }

    fn control_if(&mut self) -> Result<ParseNode, ParserError> {
        self.check_starting_bracket("if");
        let condition = self.expr(0)?;
        self.check_ending_bracket("if");

        let then = self.groups()?;

        let mut node = ControlNode::new(condition, then);

        // elif branches
        while matches!(self.peek().typ, TokenType::Elif) {
            _ = self.consume(); // elif

            self.check_starting_bracket("elif");
            let elif_condition = self.expr(0)?;
            self.check_ending_bracket("elif");

            let elif_then = self.groups()?;
            node.push_elif(elif_condition, elif_then);
        }

        // else branch
        if matches!(self.peek().typ, TokenType::Else) {
            _ = self.consume(); // else

            self.check_starting_bracket("else");
            let else_then = self.groups()?;
            self.check_ending_bracket("else");

            node.set_else(else_then);
        }

        Ok(ParseNode::Control(node))
    }

    fn control_for(&mut self) -> Result<ParseNode, ParserError> {
        self.check_starting_bracket("for");
        let pre = self.expr(0)?;
        assert!(
            matches!(self.consume().get_char(), Some(';')),
            "must end in semi-colon"
        );

        let cond = self.expr(0)?;
        assert!(
            matches!(self.consume().get_char(), Some(';')),
            "must end in semi-colon"
        );
        let post = self.expr(0)?;
        self.check_ending_bracket("for");

        // then
        self.check_starting_bracket("for");
        let then = self.groups()?;
        self.check_ending_bracket("for");

        let node = ForNode::new(pre, cond, post, then);
        Ok(ParseNode::For(node))
    }

    fn control_while(&mut self) -> Result<ParseNode, ParserError> {
        // condition
        self.check_starting_bracket("while");
        let cond = self.expr(0)?;
        self.check_ending_bracket("while");

        // then
        self.check_starting_bracket("while");
        let then = self.groups()?;
        self.check_ending_bracket("while");

        // construct and return
        let node = WhileNode::new(cond, then);
        Ok(ParseNode::While(node))
    }
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
fn set_binding_power(op: &str) -> Option<(usize, usize)> {
    if let Some((assoc, pow)) = get_operator_info(op) {
        match assoc {
            Associativity::Left => Some((pow, pow + 1)),
            Associativity::Right => Some((pow, pow - 1)),
        }
    } else {
        None
    }
}

/// Pratt parsing binding power for unary pre-operations.
fn pre_binding_power(op: &str) -> Option<((), usize)> {
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
