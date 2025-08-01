pub(crate) mod errors;

mod grammer;
pub mod highlight;
mod kind;
mod lex;
mod node;
mod parser;
mod set;
mod span;

pub use grammer::*;
pub use kind::*;
pub use lex::*;
pub use node::*;
pub use parser::*;
pub use set::*;
pub use span::*;

#[macro_export]
macro_rules! hl {
    ($str:literal) => {
        Some($str)
    };
    () => {
        None
    };
}
