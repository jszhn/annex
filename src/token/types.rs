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

#[derive(Clone, Copy)]
pub enum CTokenType {
    Type,
    Control,
    Specifier,
    Operator,
    Literal,
    Separator,
    GroupBegin,
    GroupEnd,
}