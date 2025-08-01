//! # Attached Modifiers

use super::*;

/// Their name should be rather self-explanatory - both the opening and closing modifier
/// are _attached_ to one another.
///
/// The following attached modifiers exist and have respective meaning:
///
/// - *bold*      - 1
/// - /italic/      - 2
/// - _underline_ - 3
/// - -strike-through- - 4
/// - !spoiler! - 5
/// - ^superscript^ (cannot be nested into `subscript`) - 6
/// - ,subscript, (cannot be nested into `superscript`) -7
/// - `inline code` (disables any nested markup - verbatim) -8
/// - `%null modifier%` -9
/// - $f(x) = y$ (verbatim) -10
/// - &variable& (verbatim) -11
pub(super) fn parse_attached_modifiers(p: &mut Parser) {
    if let Some(kind) = p.current() {
        match kind {
            SyntaxKind::Asterisk => parse_bold(p),         // 1
            SyntaxKind::Slash => parse_italics(p),         // 2
            SyntaxKind::Underscore => parse_underline(p),  // 3
            SyntaxKind::Exclamation => parse_spoiler(p),   // 4
            SyntaxKind::Caret => parse_superscript(p),     // 5
            SyntaxKind::Comma => parse_subscript(p),       // 6
            SyntaxKind::Backtick => parse_inline_code(p),  // 7
            SyntaxKind::Percent => parse_null_modifier(p), // 8
            SyntaxKind::Dollar => parse_inline_maths(p),   // 9
            SyntaxKind::Ampersand => parse_variable(p),    // 10
            SyntaxKind::Hyphen => parse_strike_through(p), // 12
            SyntaxKind::Pipe => parse_verbatim(p),         // 11
            _ => parse_text_chunk(p),
        }
    }
}

fn parse_text_chunk(p: &mut Parser) {
    looper!(!p.is_at_eof(), {
        if p.at_set(ATTACHED_MODIFIERS) {
            break;
        } else {
            p.eat();
        }
    });
}

pub(super) fn parse_inline_code(p: &mut Parser) {
    parse_modifier_block(p, T![Backtick], T![InlineCode], false);
}

pub(super) fn parse_null_modifier(p: &mut Parser) {
    parse_modifier_block(p, T![Percent], T![NullModifier], true);
}

fn parse_inline_maths(p: &mut Parser) {
    parse_modifier_block(p, T![Dollar], T![InlineMath], false);
}

fn parse_variable(p: &mut Parser) {
    parse_modifier_block(p, T![Ampersand], T![Variable], true);
}

fn parse_strike_through(p: &mut Parser) {
    parse_modifier_block(p, T![Hyphen], T![StrikeThrough], true);
}

fn parse_verbatim(p: &mut Parser) {
    parse_modifier_block(p, T![Pipe], T![Verbatim], false);
}

fn parse_subscript(p: &mut Parser) {
    parse_modifier_block(p, T![Comma], T![Subscript], true);
}

fn parse_superscript(p: &mut Parser) {
    parse_modifier_block(p, T![Caret], T![Superscript], true);
}

fn parse_spoiler(p: &mut Parser) {
    parse_modifier_block(p, T![Exclamation], T![Spoiler], true);
}

fn parse_underline(p: &mut Parser) {
    parse_modifier_block(p, T![Underscore], T![UnderLine], true);
}

fn parse_italics(p: &mut Parser) {
    parse_modifier_block(p, T![Slash], T![Italics], true);
}

fn parse_bold(p: &mut Parser) {
    parse_modifier_block(p, T![Asterisk], T![Bold], true);
}

#[track_caller]
fn parse_modifier_block(p: &mut Parser, kind: SyntaxKind, result: SyntaxKind, trimmed: bool) {
    let m = p.start();
    p.bump(kind);
    if p.at(kind) {
        p.unexpected_hinted("Italics block is empty");
    }
    if trimmed && p.at(T![WhiteSpace]) {
        p.unexpected_hinted(format!("`{result}` should not start with `WhiteSpace`"));
    }
    looper!(!p.at(T![Eof]), {
        match p.at(kind) {
            true if p
                .next()
                .filter(|k| syntax_set!(WhiteSpace, LineEnding, Eof).contains(*k))
                .is_some() =>
            {
                break;
            }
            true => {
                p.unexpected();
                continue;
            }
            false => parse_attached_modifiers(p),
        }
    });
    p.expect_closing_delimiter(m, kind);
    p.wrap(m, result);
}

assert_tree!(
    // [case:1/9] perfect bold
    italics,
    italics_01,
    parse_italics,
    "/an italics text chunk/ blah blah blah"
);

assert_tree!(
    // [case:2/9] not so perfect italics
    // error: Unclosed delimiter
    italics,
    italics_02,
    parse_italics,
    "/an italics text chunk blah blah blah"
);

assert_tree!(
    // [case:3/9] not so perfect italics
    // error: WhiteSpace at beginning
    italics,
    italics_03,
    parse_italics,
    "/ an italics text chunk/ blah blah blah"
);

assert_tree!(
    // [case:4/9] not so perfect italics
    // error: WhiteSpace at end
    italics,
    italics_04,
    parse_italics,
    "/an italics text chunk / blah blah blah"
);

assert_tree!(
    // [case:5/9] not so perfect italics
    // error: WhiteSpace at both end
    italics,
    italics_05,
    parse_italics,
    "/ an italics text chunk / blah blah blah"
);

assert_tree!(
    // [case:6/9] not so perfect italics
    // error: `LineEnding` inside text chunk
    italics,
    italics_06,
    parse_italics,
    "/an italics \n text chunk/ blah blah blah"
);

assert_tree!(
    // [case:7/9] not so perfect italics
    // feat: other inline elements inside this ATACHED_MODIFIERS
    italics,
    italics_07,
    parse_italics,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/ blah blah blah"
);

assert_tree!(
    // [case:8/9] not so perfect italics
    // feat: /this/is still italics/ - cuz no space after `/`
    italics,
    italics_08,
    parse_italics,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/blah/ blah blah"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics,
    italics_09,
    parse_italics,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/ blah/ blah blah"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics,
    italics_10,
    parse_italics,
    "//this//"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics,
    italics_11,
    parse_italics,
    "//this is *bold* //"
);
