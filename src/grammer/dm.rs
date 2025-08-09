//! # Dettached Modifiers

use super::*;

// *** Unordered Lists
//     |example
//     - Unordered list level 1
//     -- Unordered list level 2
//     --- Unordered list level 3
//     ---- Unordered list level 4
//     ----- Unordered list level 5
//     ------ Unordered list level 6
//     ------- Unordered list level 7 (falls back to level 6 in the tree-sitter parser)
//
//     - Unordered list level 1
//       This text is still part of the level 1 list item.
//     -- Unordered list level 2
//        This text is still part of the level 2 list item.
//     --- Unordered list level 3
//         This text is still part of the level 3 list item.
//     ---- Unordered list level 4
//          This text is still part of the level 4 list item.
//     ----- Unordered list level 5
//           This text is still part of the level 5 list item.
//     ------ Unordered list level 6
//            This text is still part of the level 6 list item.
//     ------- Unordered list level 7 (falls back to level 6 in the tree-sitter parser)
//             This text is still part of the level 7 list item.
//     |end
//
//     Unordered lists provide an easy way to enumerate items in an unordered fashion. Useful for data
//     that's categorically similar but doesn't need to follow a strict order.
fn unorderedlists(p: &mut Parser) {
    while !p.is_at_eof() && p.at(SyntaxKind::Hyphen) {
        unorderedlist(p);
    }
}

/* # treesitter impl

  (generic_list ; [4, 0] - [5, 9]
    (unordered_list2 ; [4, 0] - [5, 9]
      (unordered_list2_prefix) ; [4, 0] - [4, 3]             --*
      content: (paragraph ; [4, 3] - [5, 9]
        (paragraph_segment) ; [4, 3] - [4, 8]                list title
        (paragraph_segment)))) ; [5, 0] - [5, 9]             & its contents
*/
pub(super) fn unorderedlist(p: &mut Parser) {
    let m = p.start();
    let _n = p.eat_many_counted(T![Hyphen]);
    // if n > 6 { /* do something */ }
    p.expect(T![WhiteSpace]);
    paragraph(p);
    p.wrap(m, T![UnOrderedList]);
}
