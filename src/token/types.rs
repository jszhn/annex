#[derive(Clone, Copy, PartialEq)]
pub enum TokenType {
    Type,
    Control,
    Specifier,
    Operator,
    Identifier, // catch-all for non keyword tokens
    Separator,
    Return,
    Boolean,
    GroupBegin,
    GroupEnd,
    None,
    EOF,
}
