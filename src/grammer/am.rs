//! # Attached Modifiers [line 1046 of official spec]
//!  
//!  This section discusses attached modifiers (which originally gave rise to
//!  the name of [detached modifiers] as their natural counter-parts).
//!  Attached modifiers encapsulate some text within a [paragraph]
//!  and change the way it is displayed in the document.
//!  
//!  An attached modifier consists of two parts:
//!     1.the opening modifier and
//!     2.the closing modifier.
//!
//! ## General rules for attached modifiers:
//!    
//!    - An opening modifier may only be preceded by whitespace or punctuation
//!    - An opening modifier may _NOT_ be succeeded by whitespace
//!    - A closing modifier may _NOT_ be preceded by whitespace
//!    - A closing modifier may only be succeeded by whitespace or punctuation
//!
//!    Max span = a paragraph TODO: []
//!    ie, is terminated at ParaBreak in case of non closure
//!
//!    Nesting is allowed. (must be in order) eg: =>   */_-like this-_/*
//!
//!  Two or more consecutive attached modifiers of the same type (i.e. `**`, `//` etc.)
//!  should be instantly "disqualified" and parsed as raw text in /all/ circumstances
//!  and without any exceptions. TODO: []
//!
//! Their name should be rather self-explanatory - both the opening and closing modifier
//! are _attached_ to one another.
//!
//! The following attached modifiers exist and have respective meaning:
//!
//!  1)  *bold*      
//!  2)  /italic/      
//!  3)  _underline_
//!  4)  -strike-through-
//!  5)  !spoiler!
//!  6)  ^superscript^  (cannot be nested into `subscript`)
//!  7)  ,subscript,  (cannot be nested into `superscript`)
//!  8)  `inline code`  (disables any nested markup - verbatim)
//!  9)  `%null modifier%`
//! 10)  $f(x) = y$ `inline math`  (verbatim)
//! 11)  &variable&  (verbatim)
//!
//! ## Strategy
//!
//! 01) ignore punctuations surrounded by word `ignore_this`
//! 02) ensure there is no Two or more consecutive attached modifiers of the same type.

use super::*;

pub const DELIMITER_PAIR: [SyntaxKind; 12] = [
    SyntaxKind::Asterisk,
    SyntaxKind::Slash,
    SyntaxKind::Underscore,
    SyntaxKind::Exclamation,
    SyntaxKind::Caret,
    SyntaxKind::Comma,
    SyntaxKind::Backtick,
    SyntaxKind::Percent,
    SyntaxKind::Dollar,
    SyntaxKind::Ampersand,
    SyntaxKind::Hyphen,
    SyntaxKind::Pipe,
];

const SPAN_HINT: &str = r#"can only span at maximum a single `paragraph`,
i.e. they get terminated as soon as they encounter a `paragraph break`."#;

// # Rules
//
//  - An opening modifier may only be preceded by whitespace or punctuation
//    ie, prev token must not be a word
//  - An opening modifier may _NOT_ be succeeded by whitespace
fn is_valid_op_delimeter(p: &mut Parser) -> bool {
    let prev_no_word = p.prev().filter(|k| *k != T![Word]).is_some();
    let next_no_white_sp = p.next().filter(|k| *k != T![WhiteSpace]).is_some();
    matches!((prev_no_word, next_no_white_sp), (true, true))
}

// # Rules
//
//  - A closing modifier may _NOT_ be preceded by whitespace
//  - A closing modifier may only be succeeded by whitespace or punctuation
//    ie, next token must not be a word
fn is_valid_cl_delimeter(p: &mut Parser) -> bool {
    let prev_no_white_sp = p.prev().filter(|k| *k != T![WhiteSpace]).is_some();
    let next_no_word = p.next().filter(|k| *k != T![Word]).is_some();
    matches!((next_no_word, prev_no_white_sp), (true, true))
}

///   [  ][punctuation char][  ]
///    ^          ^          ^  
///  word <<current token>> word
///
///  both prev and next token are word
fn is_surrounded_by_word(p: &mut Parser) -> bool {
    let prev = p.prev().filter(|k| *k == T![Word]).is_some();
    let next = p.next().filter(|k| *k == T![Word]).is_some();
    matches!((next, prev), (true, true))
}

/// # Rule
///
///  Two or more consecutive attached modifiers of the same type (i.e. `**`, `//` etc.)
///  should be instantly "disqualified" and parsed as raw text in all circumstances
///  and without any exceptions.
///
///  job is to only check it not consume it
fn is_valid_delimeter(p: &mut Parser) -> bool {
    let current = p.current();
    let next = p.next();
    Some(current) != next
}

/// ## New
pub(super) fn parse_attached_modifiers(p: &mut Parser) {
    let delimiter_kind = p.current();
    let modifier_kind = delimiter_kind.as_attached_modifers_unchecked();
    let m = p.start();

    // `ignore_this`
    match is_surrounded_by_word(p) {
        false => {
            // ignore `******` this
            match is_valid_delimeter(p) {
                true if is_valid_op_delimeter(p) => {
                    match delimiter_kind.is_verbatim() {
                        true => {
                            p.eat_until(syntax_set!(ParaBreak, LineEnding).add(delimiter_kind));
                        }
                        false => {
                            p.eat();
                            // TODO: we should add other inline elements in `linkable` here
                            let set = ATTACHED_MODIFIERS.add(T![ParaBreak]).add(T![LineEnding]);
                            p.eat_until(set);
                        }
                    }
                    if is_valid_cl_delimeter(p) {
                        p.eat();
                        p.wrap(m, modifier_kind);
                    } else if p.current().is_punctuation() {
                        parse_attached_modifiers(p);
                    } else {
                        p.expect_closing_delimiter(m, delimiter_kind);
                    }
                }
                _ => {
                    while p.current() == delimiter_kind {
                        p.covert_and_eat(T![Word]);
                    }
                }
            }
        }
        true => p.covert_and_eat(T![Word]),
    }
}

assert_tree!(
    // [case:1/9] perfect italics (x)
    italics,
    italics_01,
    parse_attached_modifiers,
    "/an italics text chunk/ blah blah blah"
);

assert_tree!(
    // [case:2/9] not so perfect italics
    // error: Unclosed delimiter
    italics,
    italics_02,
    parse_attached_modifiers,
    "/an italics text chunk blah blah blah"
);

assert_tree!(
    // [case:3/9] not so perfect italics
    // error: WhiteSpace at beginning
    italics,
    italics_03,
    parse_attached_modifiers,
    "/ an italics text chunk/ blah blah blah"
);

assert_tree!(
    // [case:4/9] not so perfect italics
    // error: WhiteSpace at end
    italics,
    italics_04,
    parse_attached_modifiers,
    "/an italics text chunk / blah blah blah"
);

assert_tree!(
    // [case:5/9] not so perfect italics
    // error: WhiteSpace at both end (2 errors)
    italics,
    italics_05,
    parse_attached_modifiers,
    "/ an italics text chunk / blah blah blah"
);

assert_tree!(
    // [case:6/9] not so perfect italics
    // error: `LineEnding` inside text chunk
    italics,
    italics_06,
    parse_attached_modifiers,
    "/an italics \n text chunk/ blah blah blah"
);

assert_tree!(
    // [case:6/9] not so perfect italics
    // error: `LineEnding` inside text chunk
    italics,
    italics_12,
    parse_attached_modifiers,
    "/an italics \n\n text chunk/ blah blah blah"
);

assert_tree!(
    // [case:6/9] not so perfect italics
    // error: `LineEnding` inside text chunk
    italics,
    italics_13,
    parse_attached_modifiers,
    "/an italics | !\"#$%&'()*+,-./:;<=>?@[\\]^_`{}~verbatim | text chunk/ blah blah blah"
);

assert_tree!(
    // [case:7/9] not so perfect italics
    // feat: other inline elements inside this ATACHED_MODIFIERS
    italics,
    italics_07,
    parse_attached_modifiers,
    "/an italics text chunk and it have a !`/$&%'*,-^_|verbatim |_^-,*'%&$/`! chunk in it :)/ blah blah blah"
);

assert_tree!(
    // [case:8/9] not so perfect italics
    // feat: /this/is still italics/ - cuz no space after `/`
    italics,
    italics_08,
    parse_attached_modifiers,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/blah/ blah blah"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics,
    italics_09,
    parse_attached_modifiers,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/ blah/ blah blah"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics,
    italics_10,
    parse_attached_modifiers,
    "//this//"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics,
    italics_11,
    parse_attached_modifiers,
    "//this is *bold* //"
);

#[cfg(test)]
mod test {
    proptest::proptest! {
        #[test]
        fn no_panic_prop(input in ".*") {
            let mut parser = crate::Parser::new(&input);
            super::parse_attached_modifiers(&mut parser);
        }
    }
}
