use phf::{phf_map, phf_set};
use std::error::Error;
pub mod io;

/// Public interface for token stream.
pub struct TokenStream {
    vector: Vec<Token>,
}

impl TokenStream {
    /// Constructs a new token stream. Attempts to tokenise the input file.
    pub fn new(file: String) -> Result<Self, Box<dyn Error>> {
        let vector = tokenise(file);
        Ok(Self { vector })
    }

    /// Pops the next token from the stack.
    pub fn consume(&mut self) -> Token {
        self.vector.pop().unwrap_or_default()
    }

    /// Returns token at the top of the stack.
    pub fn peek(&self) -> Token {
        self.vector.last().cloned().unwrap_or_default()
    }

    /// Returns true if the token stack is empty.
    pub fn is_empty(&self) -> bool {
        self.vector.is_empty()
    }
}

/// Type denoting an individual lexical unit in the language.
#[derive(Clone, Default)] // todo: deprecate Default when position is implemented
pub struct Token {
    pub typ: TokenType,
}

impl Token {
    pub fn new(typ: TokenType) -> Self {
        Self { typ }
    }

    pub fn get_char(&self) -> Option<char> {
        match self.typ {
            TokenType::GroupBegin(val) | TokenType::GroupEnd(val) | TokenType::Separator(val) => {
                Some(val)
            }
            _ => None,
        }
    }
}

#[derive(Clone, Default)]
pub enum TokenType {
    // literals
    Identifier(String),
    Boolean(bool),
    Integer(u64),
    Decimal(f64),
    // keywords
    Type(String),
    Operator(String),
    Function,
    Return,
    // specifiers
    Variable,
    Constant,
    Volatile,
    // control
    If,
    Elif,
    Else,
    // loops
    For,
    While,
    Continue,
    Break,
    // delineators
    Separator(char),
    GroupBegin(char),
    GroupEnd(char),
    // special
    #[default]
    EOF,
}

/// These TokenType methods group relevant TokenTypes together. This helps facilitate
/// pattern-matching later in the compiler pipeline, especially for parsing.
impl TokenType {
    pub fn is_literal(&self) -> bool {
        match self {
            TokenType::Identifier(_)
            | TokenType::Integer(_)
            | TokenType::Decimal(_)
            | TokenType::Boolean(_) => true,
            _ => false,
        }
    }

    pub fn is_control(&self) -> bool {
        match self {
            // control
            TokenType::If
            | TokenType::Elif
            | TokenType::Else
            // loops
            | TokenType::For
            | TokenType::While
            | TokenType::Continue
            | TokenType::Break => true,
            _ => false,
        }
    }

    pub fn is_specifier(&self) -> bool {
        match self {
            TokenType::Variable | TokenType::Constant | TokenType::Volatile => true,
            _ => false,
        }
    }
}

/// State object for lexical analysis.
struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    line_num: usize,
}

impl<'a> Lexer<'a> {
    fn new(file: &'a String) -> Self {
        Self {
            chars: file.char_indices().peekable(),
            line_num: 1,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let (_pos, c) = self.chars.next()?; // todo: use position

        // check if single-line comment
        if c == '/' && self.peek() == Some('/') {
            self.chars.next(); // consume second '/'
            self.skip_line();
            return self.next_token();
        }

        let typ = if let Some(op_typ) = self.id_multichar_operator(c) {
            op_typ
        } else if let Some(single_typ) = id_singlechar(c.clone()) {
            single_typ
        } else {
            id_multichar_lexeme(self.construct_lexeme(c.clone()).as_str())
        };
        Some(Token::new(typ))
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| c).cloned()
    }

    fn construct_lexeme(&mut self, c: char) -> String {
        std::iter::once(c)
            .chain(std::iter::from_fn(|| self.yield_char()))
            .collect()
    }

    fn yield_char(&mut self) -> Option<char> {
        if let Some(&(_, c)) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                Some(self.chars.next().map(|(_, c)| c).unwrap())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn id_multichar_operator(&mut self, first_char: char) -> Option<TokenType> {
        let second_char = self.peek()?;
        let operator = match (first_char, second_char) {
            ('>', '>') => ">>",
            ('<', '<') => "<<",
            ('=', '=') => "==",
            ('!', '=') => "!=",
            ('<', '=') => "<=",
            ('>', '=') => ">=",
            ('&', '&') => "&&",
            ('|', '|') => "||",
            _ => return None,
        };

        self.chars.next();
        Some(TokenType::Operator(operator.to_string()))
    }

    fn skip_whitespace(&mut self) {
        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn skip_line(&mut self) {
        while let Some(&(_, ch)) = self.chars.peek() {
            self.chars.next();
            if ch == '\n' {
                break;
            }
        }
    }
}

fn tokenise(file: String) -> Vec<Token> {
    let mut lexer = Lexer::new(&file);
    let mut tokens: Vec<Token> = std::iter::from_fn(|| lexer.next_token()).collect();
    tokens.push(Default::default()); // EOF
    tokens.reverse();
    tokens
}

fn id_singlechar(c: char) -> Option<TokenType> {
    match c {
        '(' | '{' | '[' => Some(TokenType::GroupBegin(c)),
        ')' | '}' | ']' => Some(TokenType::GroupEnd(c)),
        ';' | ',' | '.' | ':' | '#' => Some(TokenType::Separator(c)),
        '+' | '-' | '=' | '*' | '?' | '|' | '&' | '^' | '>' | '<' | '~' | '/' => {
            Some(TokenType::Operator(c.to_string()))
        }
        _ => None,
    }
}

/// Compile-time mapping between string literals and TokenType.
static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    // specifiers
    "var" => TokenType::Variable,
    "const" => TokenType::Constant,
    "vol" => TokenType::Volatile,
    // functions
    "fn" => TokenType::Function,
    "return" => TokenType::Return,
    // control
    "if" => TokenType::If,
    "elif" => TokenType::Elif,
    "else" => TokenType::Else,
    // loops
    "for" => TokenType::For,
    "while" => TokenType::While,
    "continue" => TokenType::Continue,
    "break" => TokenType::Break,
};

/// Converts a multi-character lexeme into a token. By default, any unreserved lexemes will be
/// converted into an identifier token.
fn id_multichar_lexeme(lexeme: &str) -> TokenType {
    KEYWORDS
        .get(lexeme)
        .cloned()
        .or_else(|| id_type(lexeme))
        .or_else(|| id_literal(lexeme))
        .unwrap_or(TokenType::Identifier(lexeme.to_string()))
}

/// Compile-time set containing all possible type values.
static TYPES: phf::Set<&'static str> = phf_set! {
    "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64",
    "f64", "f32", "void", "bool",
};

/// Determines if a given lexeme is a valid type.
fn id_type(lexeme: &str) -> Option<TokenType> {
    match TYPES.contains(lexeme) {
        true => Some(TokenType::Type(lexeme.to_string())),
        false => None,
    }
}

/// Determines if a given lexeme is a literal, and if so, parses and returns it.
fn id_literal(lexeme: &str) -> Option<TokenType> {
    if let Ok(val) = lexeme.parse::<u64>() {
        Some(TokenType::Integer(val))
    } else if let Ok(val) = lexeme.parse::<f64>() {
        Some(TokenType::Decimal(val))
    } else if let Ok(val) = lexeme.parse::<bool>() {
        Some(TokenType::Boolean(val))
    } else {
        None
    }
}
