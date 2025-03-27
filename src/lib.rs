use self::lexer::Token;

pub mod ast;
pub mod kind;
pub mod lexer;
pub mod parser;
pub mod span;

pub(crate) mod error;

pub trait NeoChar {
    /// returns true if the char matches with any syntax char of neorg
    fn is_special_char(&self) -> bool;
}

pub trait ParseTools {
    fn peek(&self) -> &Token;
    fn advance(&mut self) -> Token;
    fn previous(&mut self) -> Token;
    fn is_at_end(&self) -> bool;
}
