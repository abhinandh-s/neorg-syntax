#![allow(unused)]

use self::kind::SyntaxKind;
use neorg_syntax::*;

fn main() {
    let italics = include_str!("../examples/tests/italics.norg");
    let underline = include_str!("../examples/tests/underline.norg");
    let heading = include_str!("../examples/tests/star.norg");
    let input = " / *this *is not italics text /?|$(){}[]<>=-_+";
     table_lex(input).iter().for_each(|tok| {
         println!("{}", tok);
     });
    // let lexed = lex::Lexer::new(input.into()).lex();
    // for tok in &lexed {
    //     // println!("{}", tok);
    // }
    // print_lexed(lexed);
    // parser::Parser::new(lexed.clone())
    //     .parse()
    //     .iter()
    //     .for_each(|n| {
    //         // println!("{}", n);
    //     });
    // crate::ast::print_ast(input);
}

// fn print_lexed(l: Vec<Token>) {
//     let parsed = l;
// 
//     for i in parsed {
//         println!("{}", i)
//     }
// }
