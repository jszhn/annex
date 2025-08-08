pub trait Operator {
    fn from_token(token: &str) -> Option<Self>
    where
        Self: Sized + std::fmt::Display;
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    // Logical
    And,
    Or,

    // Comparison
    Eq,
    NotEq,
    Greater,
    Less,
    GreaterEq,
    LessEq,

    // Miscellaneous
    Assign,
    Array,
}

impl Operator for BinaryOperator {
    fn from_token(token: &str) -> Option<Self> {
        match token {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "&" => Some(Self::BitAnd),
            "|" => Some(Self::BitOr),
            "^" => Some(Self::BitXor),
            "<<" => Some(Self::ShiftLeft),
            ">>" => Some(Self::ShiftRight),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "=" => Some(Self::Assign),
            "==" => Some(Self::Eq),
            "!=" => Some(Self::NotEq),
            ">" => Some(Self::Greater),
            "<" => Some(Self::Less),
            ">=" => Some(Self::GreaterEq),
            "<=" => Some(Self::LessEq),
            "[]" => Some(Self::Array),
            _ => None,
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::BitAnd => write!(f, "&"),
            BinaryOperator::BitOr => write!(f, "|"),
            BinaryOperator::BitXor => write!(f, "^"),
            BinaryOperator::ShiftLeft => write!(f, "<<"),
            BinaryOperator::ShiftRight => write!(f, ">>"),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Or => write!(f, "or"),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::NotEq => write!(f, "!="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::GreaterEq => write!(f, ">="),
            BinaryOperator::LessEq => write!(f, "<="),
            BinaryOperator::Assign => write!(f, "="),
            BinaryOperator::Array => write!(f, "[]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Neg,    // -
    BitNot, // ~
    Not,    // !
}

impl Operator for UnaryOperator {
    fn from_token(token: &str) -> Option<Self> {
        match token {
            "-" => Some(Self::Neg),
            "~" => Some(Self::BitNot),
            "!" => Some(Self::Not),
            _ => None,
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::BitNot => write!(f, "~"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}
