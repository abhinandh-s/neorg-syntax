#![allow(dead_code)]

use crate::*;

mod am;
use am::*;

mod dm;

// document => paragraph_break (x) // lexer will handle both line_break & paragraph_break
//             | line_break (x)
//             | heading (x)
//             | nestable_detached_modifier
//             | rangeable_detached_modifier
//             | table
//             | tag
//             | horizontal_line (X)
//             | strong_paragraph_delimiter
//             | paragraph (x)
//
#[track_caller]
pub fn document(p: &mut Parser) -> SyntaxNode {
    let m = p.start();

    while !p.is_at_eof() {
        p.skip_whitespace();

        match p.current_unchecked() {
            T![Asterisk] => heading(p),
            _ => paragraph(p),
        }
    }
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
    while !p.at_set(syntax_set!(Eof, LineEnding, ParaBreak)) {
        match p.current_unchecked() {
            SyntaxKind::LCurly => p.eat(),
            other => {
                if ATTACHED_MODIFIERS.contains(other) {
                    parse_attached_modifiers(p);
                } else {
                    p.eat();
                }
            }
        }
    }
    match p.is_at_eof() {
        true => {
            p.eat();
        }
        false => {
            let kind = p.current_unchecked();
            match kind {
                SyntaxKind::LineEnding | SyntaxKind::ParaBreak => {
                    eat_breaks(p);
                }
                _ => p.unexpected(),
            }
            p.bump_line();
        }
    }
    p.wrap(m, T![ParaSegment]);
}

fn eat_breaks(p: &mut Parser) {
    while !p.is_at_eof() {
        match p.current_unchecked() {
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
        if p.current_unchecked() == SyntaxKind::ParaBreak {
            p.bump(T![ParaBreak]);
            p.bump_line();
            p.bump_line();
            break;
        } else {
             paragraph_segment(p);
        }
    });

    p.is_stuck(last_cursor).then(|| panic!("Parser is stuck"));

    p.wrap(m, SyntaxKind::Paragraph);
}

fn quote(p: &mut Parser) {
    let m = p.start();
    p.eat_many(syntax_set!(GreaterThan));
    p.expect(T![WhiteSpace]);
    paragraph_segment(p);
    p.wrap(m, T![Quote]);
}

fn heading(p: &mut Parser) {
    let m = p.start();
    p.eat_many(syntax_set!(Asterisk));
    p.expect(T![WhiteSpace]);
    paragraph_segment(p);
    p.wrap(m, SyntaxKind::Heading);
}
