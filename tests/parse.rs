#[macro_export]
macro_rules! set_insta_env {
    () => {
        fn set() {
            let key = "INSTA_UPDATE";
            unsafe {
                std::env::set_var(key, "allow");
            }
        }
    };
}

// #[cfg(test)]
// mod test {
//     assert_tree!(verbatim_05, parse_verbatim, "|this is a test");
//     assert_tree!(verbatim_01, parse_verbatim, "|this is a test|");
//     assert_tree!(verbatim_02, parse_verbatim, "| this is a test |");
//     assert_tree!(
//         verbatim_03,
//         parse_verbatim,
//         r###"|~ this is a test !"#$%&'()*+,-./:;<=>?@[]^_`{}~\thre|"###
//     );
//     assert_tree!(verbatim_04, parse_verbatim, r###"|~ `{}~\thre|"###);
//     assert_tree!(text_chunk_01, parse_atmod_text_chunk, "this is text chunk");
//     assert_tree!(
//         text_chunk_01_err,
//         parse_atmod_text_chunk,
//         "this is text chunk / not this"
//     );
//     assert_tree!(
//         para_segment_01,
//         parse_para_segment,
//         "this is |a verbatim text chunk|"
//     );
//     assert_tree!(
//         para_segment_01_err,
//         parse_para_segment,
//         "this is |a verbatim text chunk"
//     );
//     assert_tree!(
//         para_segment_02_err,
//         parse_para_segment,
//         "this is |a verbatim text chunk|\nthis is next"
//     );
//     assert_tree!(
//         para_segment_03_err,
//         parse_para_segment,
//         "this isrbatim text chunk\nthis is next"
//     );
//     assert_tree!(
//         paragraph_01,
//         parse_paragraph,
//         "this isrbatim text chunk\nthis is next"
//     );
//     assert_tree!(
//         paragraph_02,
//         parse_paragraph,
//         r#"I am a paragraph segment
// I am another paragraph segment
//  Together we form a paragraph"#
//     );
//     assert_tree!(
//         paragraph_03,
//         parse_paragraph,
//         r#"I am a paragraph segment.
// I am another paragraph segment.
//     Together we form a paragraph."#
//     );
//     assert_tree!(
//         paragraph_with_verbatim_01,
//         parse_paragraph,
//         r#"I am a paragraph segment.
// I am another paragraph segment.
// this |is verbatim| content
//     Together we form a paragraph."#
//     );
//     assert_tree!(
//         paragraph_with_verbatim_emph_01,
//         parse_paragraph,
//         r#"I am a paragraph segment.
// I am another paragraph segment.
// this |is verbatim| content *this is bold*
//     Together we form a paragraph."#
//     );
//
//     // ParaBreak
//     //
//     // [case:1/2]
//     //
//     // `LineEnding` after a `LineEnding`
//     assert_tree!(
//         parabreak_01,
//         parse_paragraph,
//         r#" I am a paragraph segment.
// I am another paragraph segment.
//
// this |is verbatim| content *this is bold*
//     Together we form a paragraph."#
//     );
//
//     // ParaBreak
//     //
//     // [case:2/2]
//     //
//     // empty line with many `WhiteSpace` followed by `LineEnding`
//     assert_tree!(
//         parabreak_02,
//         parse_paragraph,
//         r#" I am a paragraph segment.
// I am another paragraph segment.
//
// this |is verbatim| content *this is bold*
//     Together we form a paragraph."#
//     );
//     assert_tree!(
//         parse_02,
//         parse_paragraph,
//         r#" I am a paragraph segment.
//
// I am another paragraph segment.
//  are _attached_ to one another.
// ([{}])
//   *bold*
//   /italic/
//   _underline_
//   |-strike-through-|
//   !spoiler!
//   ^superscript^
//   ,subscript,
//   `inline code`
//   `%null modifier%`
//   &variable&
// this |is verbatim| content *this is bold*
//     Together we form a paragraph."#
//     );
//
//     quickcheck::quickcheck! {
//         #[test]
//         #[ignore]
//         fn no_panic_on_random_input(input: String) -> bool {
//             let mut parser = crate::Parser::new(&input);
//             crate::parse_paragraph(&mut parser);
//             true // test passes if no panic
//         }
//     }
//
//     proptest::proptest! {
//         #[test]
//         #[ignore]
//         fn no_panic_prop(input in ".*") {
//             let mut parser = crate::Parser::new(&input);
//             crate::parse_paragraph(&mut parser);
//         }
//     }
// }
