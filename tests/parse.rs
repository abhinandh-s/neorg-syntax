// #[test]
// fn parsing_001() {
//     let tokens = vec![
//         TokenData::new("".to_string(), SyntaxKind::At, 0).into(),
//         TokenData::new("".to_string(), SyntaxKind::KW_Document, 0).into(),
//         TokenData::new("".to_string(), SyntaxKind::Dot, 0).into(),
//         TokenData::new("".to_string(), SyntaxKind::KW_Meta, 0).into(),
//         TokenData::new("".to_string(), SyntaxKind::NewLine, 0).into(),
//         TokenData::new("".to_string(), SyntaxKind::WhiteSpace, 0).into(),
//         TokenData::new("".to_string(), SyntaxKind::At, 0).into(),
//         TokenData::new("".to_string(), SyntaxKind::KW_End, 0).into(),
//     ];
//
//     let mut p = Parser::new(tokens);
//     let p = p.collect();
//
//     let re = SyntaxNode::new(
//         SyntaxKind::Document,
//         vec![
//             SyntaxNode::new(SyntaxKind::At, Vec::new()),
//             SyntaxNode::new(SyntaxKind::KW_Document, Vec::new()),
//             SyntaxNode::new(SyntaxKind::Dot, Vec::new()),
//             SyntaxNode::new(SyntaxKind::KW_Meta, Vec::new()),
//             SyntaxNode::new(SyntaxKind::NewLine, Vec::new()),
//             SyntaxNode::new(SyntaxKind::WhiteSpace, Vec::new()),
//             SyntaxNode::new(SyntaxKind::At, Vec::new()),
//             SyntaxNode::new(SyntaxKind::KW_End, Vec::new()),
//         ],
//     );
//
//     assert_eq!(re, p.clone());
// }
