#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub enum TokenType {
    Type,
    Control,
    Function,
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
