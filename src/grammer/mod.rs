#![allow(dead_code)]

use crate::*;

mod am;
use am::*;

mod dm;
use dm::*;

pub enum NeorgLint {
    EmptyAtModifiers,
    EmptyLink,
}

// document
//
// - paragraph_break and line_break (x) // lexer will handle both line_break & paragraph_break
// - paragraph (x)
// - heading (x)
//       | nestable_detached_modifier
//             | rangeable_detached_modifier
//             | table
//             | tag
//             | horizontal_line (X)
//             | strong_paragraph_delimiter
//
pub fn document(p: &mut Parser) -> SyntaxNode {
    let m = p.start();

    // stops on Eof
    p.iter_while(None, |p| {
        p.skip_whitespace();

        match p.current() {
            T![Asterisk] => heading(p),
            T![GreaterThan] => quote(p),
            T![Hyphen] => unorderedlist(p),
            _ => paragraph(p),
        }
    });
    p.bump(T![Eof]);
    p.wrap(m, T![Document]);
    std::mem::take(&mut p.nodes[0])
}

// // A paragraph segment can contain any paragraph element.
// paragraph_segment: $ =>
// prec.right(0,
//   seq(
//     optional($.weak_carryover_set),
//     repeat1(
//       choice(
//         $._paragraph_element,
//         alias($._conflict_open, "_word"),
//       ),
//     ),
//   ),
// ),
fn paragraph_segment(p: &mut Parser) {
    let m = p.start();

    while !p.is_at_eof() {
        match p.current() {
            SyntaxKind::Eof | SyntaxKind::LineEnding => {
                p.bump_line();
                p.eat();
                break;
            }
            SyntaxKind::ParaBreak => break,
            SyntaxKind::LCurly => linkable(p),
            any if DELIMITER_PAIR.contains(&any) => parse_attached_modifiers(p),
            _ => p.eat(),
        }
    }
    p.wrap(m, T![ParaSegment]);
}

// paragraph_element => word
//                      | space
//                      | trailing_modifier
//                      | link
//                      | anchor_declaration
//                      | anchor_definition
//                      | inline_link_target
//                      | escape_sequence
//                      | link_modifier?
//                      | attached_modifier?
//
fn paragraph_element(p: &mut Parser) {
    let c = p.current();
    match c {
        T![Word] | T![WhiteSpace] | T![Tab] => p.eat(),
        SyntaxKind::RParen => (),
        SyntaxKind::RCurly => (),
        SyntaxKind::RSquare => (),
        any if ATTACHED_MODIFIERS.contains(any) => parse_attached_modifiers(p),
        _ => (),
    }
}
fn eat_breaks(p: &mut Parser) {
    while !p.is_at_eof() {
        match p.current() {
            SyntaxKind::LineEnding => {
                p.eat();
                p.bump_line();
            }
            SyntaxKind::ParaBreak => {
                p.eat();
                p.bump_line();
                p.bump_line();
            }
            _ => break,
        }
    }
}

fn paragraph(p: &mut Parser) {
    let m = p.start();
    let last_cursor = p.cursor;
    looper!(!p.is_at_eof(), {
        match p.current() {
            SyntaxKind::ParaBreak | SyntaxKind::LineEnding => {
                eat_breaks(p);
                break;
            }
            _ => paragraph_segment(p),
        }
    });

    p.assert_movement(last_cursor);

    p.wrap(m, SyntaxKind::Paragraph);
}

fn quote(p: &mut Parser) {
    let m = p.start();
    p.eat_many_in_set(syntax_set!(GreaterThan));
    p.expect(T![WhiteSpace]);
    paragraph_segment(p);
    p.wrap(m, T![Quote]);
}

fn heading(p: &mut Parser) {
    let m = p.start();
    p.eat_many_in_set(syntax_set!(Asterisk));
    p.expect(T![WhiteSpace]);
    p.eat_until(syntax_set!(LineEnding, ParaBreak));
    p.wrap(m, SyntaxKind::Heading);
}

fn linkable(p: &mut Parser) {
    // {./README.md}[readme]
    let m = p.start();

    let current = p.current();
    p.eat(); // `{`
    p.eat_until(syntax_set!(Eof, LineEnding, ParaBreak, RCurly));
    p.expect_closing_delimiter(m, current.corresponding_pair_unchecked());
    let current = p.current();
    p.expect(T![LSquare]);
    p.eat_until(syntax_set!(Eof, LineEnding, ParaBreak, RSquare));
    p.expect_closing_delimiter(m, current.corresponding_pair_unchecked());
    p.wrap(m, SyntaxKind::Link);
}
