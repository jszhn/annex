#[derive(Clone, Copy)]
pub enum CTokenType {
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
}
