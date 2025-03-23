// use crate::kind::SyntaxKind;
// use crate::lexer;
// use crate::parser::{self};
// 
// pub fn print_ast(source: &str) {
//     let lexed = lexer::Lexer::new(source.into()).lex();
//     let mut nodes = parser::Parser::new(lexed.clone());
//     println!("DOCUMENT:");
//     println!("  PARAGRAPH:");
//     nodes.parse().iter().for_each(|node| {
//         if node.kind() == SyntaxKind::NewLine {
//             println!("  PARAGRAPH:");
//         } else {
//             println!("    {}: {:?}  {}", node.kind(), node.text(), node.span())
//         }
//     });
// }
