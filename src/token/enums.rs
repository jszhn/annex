pub enum CLiteral {
    Integer
}

pub enum CType {
    Int,
    Char,
}

pub enum COperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub enum CTokenType {
    Type,
    Keyword,
    Operator,
    Literal(CLiteral),
    Separator,
}