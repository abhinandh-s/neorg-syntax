#![allow(dead_code)]

use crate::*;

mod am;
use am::*;

mod dm;

#[doc = "
document => paragraph_break (x) // lexer will handle both line_break & paragraph_break
            | line_break (x)
            | heading (x)
            | nestable_detached_modifier
            | rangeable_detached_modifier
            | table
            | tag
            | horizontal_line (X)
            | strong_paragraph_delimiter
            | paragraph (x)
"]
#[track_caller]
pub fn document(p: &mut Parser) -> SyntaxNode {
    let m = p.start();

    while !p.is_at_eof() {
        p.skip_whitespace();

        match p.current() {
            T![Asterisk] => heading(p),
            T![Hyphen] => horizontal_line(p),
            T![GreaterThan] => quote(p), // comes under nestable_detached_modifier
            _ => paragraph(p),
        }
    }
    p.wrap(m, T![Document]);
    std::mem::take(&mut p.nodes[0])
}

#[doc = "
Any of the following choices are valid IN-LINE elements. Any
multitude of these are combined to form a `paragraph_segment`.

paragraph_element => word 
                     | space
                     | trailing_modifier
                     | link
                     | anchor_declaration
                     | anchor_definition
                     | inline_link_target
                     | escape_sequence
                     | link_modifier?
                     | attached_modifier?
"]
fn paragraph_element(p: &mut Parser) {
    let c = p.current();
    match c {
        SyntaxKind::RParen => (),
        SyntaxKind::RCurly => (),
        SyntaxKind::RSquare => (),
        _ => (),
    }
}

// link: $ =>
// prec.right(2,
//   seq(
//     $.link_location,
//     optional(
//       $.link_description,
//     ),
//     optional(
//       $.attribute,
//     ),
//   ),
// ),
fn link(p: &mut Parser) {
    let m = p.start();
    link_location(p);
    if p.current() == SyntaxKind::LSquare {
        link_description(p);
    }
    // attributes(p);
    p.wrap(m, SyntaxKind::Link);
}

fn attributes(_: &mut Parser) {
    todo!()
}

// # Link Description
//
// link_description ::= "[" { word | whitespace } "]"
//
// Syntax usage:
//
// (link_location | anchor_declaration) link_description
// link_description anchor_definition
// link_description     // standalone
fn link_description(p: &mut Parser) {
    let m = p.start();
    p.bump(T!['[']);
    while !p.at_set(syntax_set!(Eof, LineEnding, RSquare)) {
        p.eat();
    }
    p.expect(T![']']);
    p.wrap(m, SyntaxKind::LinkDescription);
}

// # File Location
//
// The file location is a construct that allows you to specify the /target file/ into which you
// want to link to. This allows you to *link to targets within other files* or just link to other
// Norg files entirely.
//
// When standalone, the link syntax will simply point to another `.norg` file relative to the
// current file the link is contained in:
//
// |example
// {:path/to/other-file:}
// |end
//
// Note that you do *not* provide the `.norg` extension within the path.
// +name path modifiers
// You may use traditional modifiers in your path, like `/` (in e.g. `/my/file`) to signify the
// root of your file system, `~` (in e.g. `~/Documents/my-file`) to signify the current user's home
// directory, /or/ you can use the [Neorg]-specific `$` (in e.g. `$/my/file`) to signify the _root_
// of the [Neorg] workspace. Since not all Norg files will be used strictly by [Neorg], the
// workspace root can be implementation-specific - for git repos the workspace root could be simply
// the root of the repository, and for other note-taking apps it could simply be the root of the
// directory where all the notes are stored.
// When multiple workspaces are present, the `$name` syntax may be used (e.g. `$notes/my/file`) to
// link to a file from another workspace (in the example case named `notes`). When only a single
// workspace is supported by the application running Neorg or the workspace is not found the user
// should be met with an error.
//
// A file location may /only/ be accompanied by a {# detached modifier}, {# line number} or {# the
// magic char (`#`)}[the magic char], in which case the links look like so:
//
// |example
// {:path/to/file:123}
// {:path/to/file:# Location within that file}
// {:path/to/file:** Level 2 heading}
// |end
//
// `/`, `@` and URLs are not allowed in combination with file locations:
//
// |example
// {:path:/ file} <- invalid
// {:path:@ timestamp} <- invalid
// {:path:https://my-url} <- also invalid
// |end
//
fn file_location(_: &mut Parser) {}
// # Link Location
//
// The link location is defined through curly braces (`{}`) and contains the physical location
// that the user would like to link to. Inside these curly braces you can find one (or more; with
// limited inter-compatibility) of the following types of data:
//
// - A {# file location}
// - A {# line number}
// - A {# URL} (most commonly to an external resource)
// - A {# detached modifier} followed by the name of the linkable
// - {# nestable detached modifiers} can:*NOT* be linked to
// - A {# custom detached modifiers}[custom detached modifier] specifically made for links (`/`,
//   `#`, `?`, `=`)
// - A {**** Timestamps (`@`)}[timestamp]
// -- TODO: incomplete
fn link_location(p: &mut Parser) {
    let m = p.start();
    p.bump(T!['{']);
    while !p.at_set(syntax_set!(Eof, LineEnding, RCurly)) {
        p.eat();
    }
    p.expect(T!['}']);
    p.wrap(m, SyntaxKind::LinkLocation);
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
    while !p.at_set(syntax_set!(Eof, LineEnding)) {
        match p.current() {
            SyntaxKind::LCurly => link(p),
            other => {
                if ATTACHED_MODIFIERS.contains(other) {
                    parse_attached_modifiers(p);
                } else {
                    p.eat();
                }
            }
        }
    }
    if !p.is_at_eof() {
        p.expect(T![LineEnding]);
        p.bump_line();
    }
    p.wrap(m, T![ParaSegment]);
}

fn paragraph(p: &mut Parser) {
    let m = p.start();
    while !p.is_at_eof() {
        if p.current() == SyntaxKind::ParaBreak {
            p.bump(T![ParaBreak]);
            p.bump_line();
            p.bump_line();
            break;
        } else {
            paragraph_segment(p);
        }
    }
    p.wrap(m, SyntaxKind::Paragraph);
}

fn quote(p: &mut Parser) {
    let m = p.start();
    p.eat_many(syntax_set!(GreaterThan));
    p.expect(T![WhiteSpace]);
    paragraph_segment(p);
    p.wrap(m, T![Quote]);
}

fn strong_paragraph_delimiter() {
    todo!()
}

fn horizontal_line(p: &mut Parser) {
    let m = p.start();
    p.eat_many(syntax_set!(Hyphen));
    p.expect(T![LineEnding]);
    p.wrap(m, T![HorizontalRule]);
}

fn tag() {
    todo!()
}

fn table() {
    todo!()
}

fn rangeable_detached_modifier() {
    todo!()
}

fn nestable_detached_modifier() {
    todo!()
}

fn heading(p: &mut Parser) {
    let m = p.start();
    p.eat_many(syntax_set!(Asterisk));
    p.expect(T![WhiteSpace]);
    paragraph_segment(p);
    p.wrap(m, SyntaxKind::Heading);
}

fn parse_atmod_text_chunk(p: &mut Parser) {
    let m = p.start();
    while !p.at(SyntaxKind::Eof) {
        if p.at(SyntaxKind::LineEnding) {
            p.unexpected();
        } else if p.at_set(ATTACHED_MODIFIERS.add(SyntaxKind::Pipe)) {
            if p.prev().filter(|k| *k == SyntaxKind::WhiteSpace).is_some() {
                break;
            } else if p.next().filter(|k| *k == SyntaxKind::Word).is_some() {
                p.eat();
                continue;
            } else {
                break;
            }
        } else {
            p.eat();
        }
    }
    if p.at_set(ATTACHED_MODIFIERS)
        && p.next()
            .filter(|k| syntax_set!(WhiteSpace, LineEnding, Eof).contains(*k))
            .is_some()
    {
        if let Some(n) = p.nodes.last_mut() {
            if n.kind() == SyntaxKind::WhiteSpace {
                n.unexpected();
            }
        }
    }

    p.wrap(m, SyntaxKind::TextChunk);
}

#[macro_export]
macro_rules! assert_tree {
  ($test_name:ident, $parse_fn:ident, $input:literal) => {
        #[test]
        fn $test_name() {
            let snapshot_path = {
                let root = env!("CARGO_MANIFEST_DIR");
                std::path::Path::new(root)
                    .join("tests")
                    .join("snapshots")
            };

            let mut p = $crate::Parser::new($input);
            $parse_fn(&mut p);
            assert_eq!(p.nodes().len(), 1);

            let output = p.nodes()[0].display();

            // puts it in tests/snapshots/
            // and do not prepend path before snaps name
            insta::with_settings!({ snapshot_path => snapshot_path, prepend_module_to_snapshot => false }, {
                insta::assert_snapshot!(output);
            });
        }
    };

    ($dir_name:ident, $test_name:ident, $parse_fn:ident, $input:literal) => {
        #[test]
        fn $test_name() {
            let snapshot_path = {
                let root = env!("CARGO_MANIFEST_DIR");
                std::path::Path::new(root)
                    .join("tests")
                    .join("snapshots").join(stringify!($dir_name))
            };

            let mut p = $crate::Parser::new($input);
            $parse_fn(&mut p);
            assert_eq!(p.nodes().len(), 1);

            let output = p.nodes()[0].display();

            // puts it in tests/snapshots/
            // and do not prepend path before snaps name
            insta::with_settings!({ snapshot_path => snapshot_path, prepend_module_to_snapshot => false }, {
                insta::assert_snapshot!(output);
            });
        }
    };
}
