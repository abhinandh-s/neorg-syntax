#![allow(unused)]

use self::kind::SyntaxKind;
use self::lexer::Token;
use neorg_syntax::*;

fn main() {
    let italics = include_str!("../examples/tests/italics.norg");
    let underline = include_str!("../examples/tests/underline.norg");
    let heading = include_str!("../examples/tests/star.norg");
    let input = " / this is not italics text /";
    let lexed = lexer::Lexer::new(input.into()).lex();
    for tok in &lexed {
        // println!("{}", tok);
    }
    parser::Parser::new(lexed.clone())
        .parse()
        .iter()
        .for_each(|n| {
            // println!("{}", n);
        });
    // crate::ast::print_ast(input);
}

fn print_lexed(l: Vec<Token>) {
    let parsed = l;

    for i in parsed {
        println!("{}", i)
    }
}
