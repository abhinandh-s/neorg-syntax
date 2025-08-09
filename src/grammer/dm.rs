//! # Dettached Modifiers
//!
//! ## Nestable Detached Modifiers
//!
//! - Unordered Lists (x)
//!

use super::*;

fn unorderedlists(p: &mut Parser) {
    let m = p.start();
    while !p.is_at_eof() && p.at(SyntaxKind::Hyphen) {
        unorderedlist(p);
    }
    // this will help when we auto gen list number
    p.wrap(m, T![GenericList]);
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
    // Unordered list level 7 (falls back to level 6 in the tree-sitter parser)
    // if n > 6 { /* do something */ }
    p.expect(T![WhiteSpace]);
    paragraph(p);
    p.wrap(m, T![UnOrderedList]);
}
