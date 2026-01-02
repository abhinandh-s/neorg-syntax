//! # Attached Modifiers

use std::collections::HashMap;

use super::*;

const DELIMITER_PAIR: [SyntaxKind; 12] = [
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

fn is_valid_op_delimeter(p: &mut Parser) -> bool {
    // - condition 1: An opening modifier may only be preceded by [whitespace] or [punctuation]
    let prev_no_word = p.prev().filter(|k| *k != T![Word]).is_some();
    // - condition 2: An opening modifier may _NOT_ be succeeded by [whitespace]
    let next_no_white_sp = p.next().filter(|k| *k != T![WhiteSpace]).is_some();
    matches!((prev_no_word, next_no_white_sp), (true, true))
}

fn is_valid_cl_delimeter(p: &mut Parser) -> bool {
    // - condition 1: A closing modifier may _NOT_ be preceded by [whitespace]
    let prev_no_white_sp = p.prev().filter(|k| *k != T![WhiteSpace]).is_some();
    // - condition 2: A closing modifier may only be succeeded by [whitespace] or [punctuation]
    let next_no_word = p.next().filter(|k| *k != T![Word]).is_some();
    matches!((next_no_word, prev_no_white_sp), (true, true))
}

/// [  ][punctuation char][  ]
///  ^                     ^  
///  both prev and next are word
fn is_word(p: &mut Parser) -> bool {
    let prev = p.prev().filter(|k| *k == T![Word]).is_some();
    let next = p.next().filter(|k| *k == T![Word]).is_some();
    matches!((next, prev), (true, true))
}

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
#[track_caller]
pub(super) fn parse_attached_modifiers(p: &mut Parser) {
    match is_word(p) {
        true => {
            p.covert_and_eat(T![Word]);
        }
        false => {
            match is_valid_op_delimeter(p) {
                true => {
                    let mut deli_stack: HashMap<SyntaxKind, usize> = HashMap::new();

                    let kind = p.current();
                    if DELIMITER_PAIR.contains(&kind) {
                        parse_delimetered(p, kind, &mut deli_stack);
                    }
                }
                false => p.unexpected(), // with hint later
            }
        }
    }
}

fn parse_verbatim_block(p: &mut Parser) {
    while !p.at_set(syntax_set!(Eof, Pipe, ParaBreak)) {
        p.eat()
    }
}

// # Attached Modifiers
//
// Attached modifiers encapsulate some text within a [paragraph] and change
// the way it is displayed in the document.
// An attached modifier consists of two parts:
//  - the opening modifier
//  - the closing modifier
//
// Below are the general rules for attached modifiers:
// - An opening modifier may only be preceded by [whitespace] or [punctuation]
// - An opening modifier may _NOT_ be succeeded by [whitespace]
// - A closing modifier may _NOT_ be preceded by [whitespace]
// - A closing modifier may only be succeeded by [whitespace] or [punctuation]
fn parse_delimetered(
    p: &mut Parser,
    kind: SyntaxKind,
    deli_stack: &mut HashMap<SyntaxKind, usize>,
) {
    if is_word(p) {
        p.eat();
    }
    deli_stack.insert(kind, p.cursor);
    println!(
        ">> entered parse_delimetered for parsing {} at {}",
        kind, p.cursor
    );

    println!("{deli_stack:#?}");

    let trimmed = !matches!(kind, T![Pipe] | T![InlineMath] | T![InlineCode]);
    let _nestable = !matches!(kind, T![Caret] | T![Comma]);
    let result = kind.as_attached_modifers_unchecked();

    let prev_token_cond = |k: SyntaxKind| !matches!(k, | SyntaxKind::Word);

    // let is_closing_delimiter = |k: SyntaxKind| {
    //     match p.next() {
    //         Some(k) => k == SyntaxKind,
    //         None => true,
    //     }
    // };

    let m = p.start();
    // == opening delimiter ==
    let prev_token = p.prev().filter(|k| prev_token_cond(*k)).is_some();

    match prev_token {
        false if p.cursor != 0 => {
            if let Some(n) = p.nodes.last_mut() {
                if n.kind() != SyntaxKind::WhiteSpace {
                    n.unexpected_with_hint(format!(
                        "An opening modifier may only be preceded by `whitespace` or `punctuation`"
                    ));
                }
            }
        }
        _ => {} // is at allowed token so skip
    }
    p.bump(kind);

    if p.at(T![WhiteSpace]) && trimmed {
        p.unexpected_with_hint(format!(
            "`{result}` should not start with `whitespace`\nconsider removing the `whitespace`"
        ));
    } else if p.at(kind) {
        p.unexpected_with_hint(format!(
            "a single `{}` is nessessary\nconsider removing redundant `{}`",
            kind.text(),
            kind.text()
        ));
    }

    // == content ==
    if kind == SyntaxKind::Pipe {
        parse_verbatim_block(p);
    } else {
        while !p.is_at_eof() {
            match p.current() {
                // current is `Slash` & next is an ATTACHED_MODIFIERS or WhiteSpace or Eof
                SyntaxKind::ParaBreak => {
                    p.unexpected_with_hint(format!("{result} {SPAN_HINT}"));
                    p.recover_until(kind);
                    return;
                }
                SyntaxKind::Eof => break,
                SyntaxKind::Pipe => {
                    println!("mathed pipe at {}", p.cursor);
                    if deli_stack.get(&SyntaxKind::Pipe).is_none() {
                        parse_delimetered(p, SyntaxKind::Pipe, deli_stack);
                    } else {
                        break;
                    }
                }
                k if ATTACHED_MODIFIERS.contains(k) => {
                    if kind == k {
                        break;
                    } else if deli_stack.get(&k).is_some() {
                        parse_delimetered(p, k, deli_stack);
                    } else {
                        break;
                    }
                }
                _ => p.eat(),
            }
        }
    }

    // == closing delimiter ==
    if trimmed {
        if let Some(n) = p.nodes.last_mut() {
            if n.kind() == SyntaxKind::WhiteSpace {
                n.unexpected(); // set the `WhiteSpace` before closing modifiers as unexpected
            }
        }
    }

    p.expect_closing_delimiter(m, kind); // eat the closing modifier.
    p.wrap(m, result);
    deli_stack.remove(&kind);
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
