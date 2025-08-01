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
    let c = p.current_unchecked();
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
    if p.current_unchecked() == SyntaxKind::LSquare {
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
    let cond = !p.at_set(syntax_set!(Eof, LineEnding, RSquare));
    looper(|| cond, &mut || {
        p.eat();
    });

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
    let cond = !p.at_set(syntax_set!(Eof, LineEnding, RCurly));
    looper(|| cond, &mut || {
        p.eat();
    });
    p.expect(T!['}']);
    p.wrap(m, SyntaxKind::LinkLocation);
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

