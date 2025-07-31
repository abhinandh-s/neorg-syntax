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
    match p.current() {
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
        _ => parse_atmod_text_chunk(p),
    }
}

pub(super) fn parse_inline_code(_: &mut Parser) {
    todo!()
}

pub(super) fn parse_null_modifier(_: &mut Parser) {
    todo!()
}

fn parse_inline_maths(_: &mut Parser) {
    todo!()
}

fn parse_variable(_: &mut Parser) {
    todo!()
}

fn parse_strike_through(_: &mut Parser) {
    todo!()
}

fn parse_verbatim(p: &mut Parser) {
    let m = p.start();
    p.bump(T![Pipe]);
    if p.at(T![Pipe]) {
        p.unexpected();
    }

    let text = p.start();
    time_bound_while!(
        !p.at_set(SyntaxSet::new().add(SyntaxKind::Eof).add(SyntaxKind::Pipe)),
        {
            p.eat();
        }
    );
    p.wrap(text, SyntaxKind::TextChunk);

    p.expect_closing_delimiter(m, T![Pipe]);
    p.wrap(m, SyntaxKind::Verbatim);
}

fn parse_subscript(_: &mut Parser) {
    todo!()
}

fn parse_superscript(_: &mut Parser) {
    todo!()
}

fn parse_spoiler(_: &mut Parser) {
    todo!()
}

fn parse_underline(_: &mut Parser) {
    todo!()
}

fn parse_italics(p: &mut Parser) {
    let m = p.start();
    p.bump(T!['/']);
    if p.at(T![Slash]) {
        p.unexpected();
    }
    if p.at(T![WhiteSpace]) {
        p.unexpected();
    }

    time_bound_while!(!p.at(T![Eof]), {
        if p.at(T![Slash]) {
            if p.next()
                .filter(|k| syntax_set!(WhiteSpace, LineEnding, Eof).contains(*k))
                .is_some()
            {
                break;
            } else {
                p.unexpected();
                continue;
            }
        } else {
            parse_attached_modifiers(p);
        }
    });
    p.expect_closing_delimiter(m, T!['/']);
    p.wrap(m, SyntaxKind::Italics);
}

fn parse_bold(p: &mut Parser) {
    let m = p.start();
    let kind = T![Asterisk];
    p.bump(kind);
    if p.at(kind) {
        p.unexpected();
    }
    if p.at(T![WhiteSpace]) {
        p.unexpected();
    }

    time_bound_while!(!p.at(T![Eof]), {
        if p.at(kind) {
            if p.next()
                .filter(|k| syntax_set!(WhiteSpace, LineEnding, Eof).contains(*k))
                .is_some()
            {
                break;
            } else {
                p.unexpected();
                continue;
            }
        } else {
            parse_attached_modifiers(p);
        }
    });
    p.expect_closing_delimiter(m, kind);
    p.wrap(m, SyntaxKind::Bold);
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
