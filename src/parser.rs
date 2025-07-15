#![allow(dead_code, unused)]
#![deny(clippy::print_stdout, clippy::print_stderr)]

use crate::node::SyntaxNode;
use crate::*;

/// A marker representing a node's position in the parser. Mainly used for
/// wrapping, but can also index into the parser to access the node, like
/// `p[m]`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Marker(usize);

// Index into the parser with markers.
impl std::ops::Index<Marker> for Parser {
    type Output = SyntaxNode;

    fn index(&self, m: Marker) -> &Self::Output {
        &self.nodes[m.0]
    }
}

impl std::ops::IndexMut<Marker> for Parser {
    fn index_mut(&mut self, m: Marker) -> &mut Self::Output {
        &mut self.nodes[m.0]
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    /// Whether the parser has the expected set of open/close delimiters. This
    /// only ever transitions from `true` to `false`.
    balanced: bool,
    loc: Location,
    /// Nodes representing the concrete syntax tree of previously parsed text.
    /// In Code and Math, includes previously parsed trivia, but not `token`.
    nodes: Vec<SyntaxNode>,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let tokens = Lexer::new(source).lex();
        Self {
            tokens,
            cursor: 0,
            balanced: true,
            loc: Location::default(),
            nodes: Vec::new(),
        }
    }

    fn eat_line_ending(&mut self) {
        self.eat();
        self.loc.bump_line(1);
    }

    /// A marker that will point to the current token in the parser once it's
    /// been eaten.
    fn start(&self) -> Marker {
        Marker(self.nodes.len())
    }

    #[track_caller]
    fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind);
    }

    #[track_caller]
    fn bump(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind);
        self.eat();
    }

    fn current(&self) -> SyntaxKind {
        self.tokens[self.cursor].kind()
    }

    fn next(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.cursor + 1).map(|f| f.kind())
    }

    fn prev(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.cursor - 1).map(|f| f.kind())
    }

    fn eat(&mut self) {
        // Token `foobar`
        //
        // start_offset = 0,
        // end_offset = 6,
        // start_line = 0,
        // end_line = 0,
        // start_col = 0,
        // end_col = 6,
        //
        // line won't get updated here. [see eat_line_ending]
        let start_offset = self.loc.offsets();

        let current = self.tokens[self.cursor].clone();
        self.loc.bump_col(current.len());
        self.loc.bump_offset(current.len());
        let node = SyntaxNode::leaf(
            current.clone(),
            Location::new(start_offset, self.loc.line(), self.loc.character()),
        );
        self.nodes.push(node);
        self.cursor += 1;
    }

    fn eat_many(&mut self, eatable: SyntaxSet) {
        while self.at_set(eatable) {
            self.eat();
        }
    }

    /// Eat the token if at `kind`. Returns `true` if eaten.
    ///
    /// Note: In Math and Code, this will ignore trivia in front of the
    /// `kind`, To forbid skipping trivia, consider using `eat_if_direct`.
    fn eat_if(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.eat();
        }
        at
    }

    /// Consume the given `kind` or produce an error.
    fn expect(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.eat();
        } else {
            // FIX: eat?
            //  self.nodes().push(SyntaxNode::error(
            //      SyntaxError::new(format!("expected {}, found {}", kind, self.current().text())),
            //      self.current().text(),
            //  ));
            //  self.eat();
        }
        at
    }

    /// Consume the given closing delimiter or produce an error for the matching
    /// opening delimiter at `open`.
    #[track_caller]
    fn expect_closing_delimiter(&mut self, open: Marker, kind: SyntaxKind) {
        if !self.eat_if(kind) {
            self.nodes[open.0].convert_to_error("unclosed delimiter");
        }
    }

    /// Eat the current node and return a reference for in-place mutation.
    #[track_caller]
    fn eat_and_get(&mut self) -> &mut SyntaxNode {
        let offset = self.nodes.len();
        self.eat();
        &mut self.nodes[offset]
    }
    /// Consume the next token (if any) and produce an error stating that it was
    /// unexpected.
    fn unexpected(&mut self) {
        self.eat_and_get().unexpected();
    }
    /// Produce an error that the given `thing` was expected at the position
    /// of the marker `m`.
    fn expected_at(&mut self, m: Marker, thing: &str) {
        let error = SyntaxNode::error(SyntaxError::new(format!("expected {thing}")), "");
        self.nodes.insert(m.0, error);
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    fn at_set(&self, set: SyntaxSet) -> bool {
        set.contains(self.current())
    }

    fn is_at_eof(&self) -> bool {
        self.current() == T![Eof]
    }

    // Range:
    //  Pos: line, col
    //  Pos: line, col
    //  Span: start, end
    fn wrap(&mut self, m: Marker, kind: SyntaxKind) {
        let drained: Vec<SyntaxNode> = self.nodes.drain(m.0..).collect();

        let first = drained.first().map_or(Location::detached(), |f| f.loc());
        let last = drained.last().map_or(Location::detached(), |f| f.loc());

        tracing::debug!(?drained, "wrap node");
        let node = SyntaxNode::inner(
            kind,
            drained,
            Location::new(first.offsets(), first.line(), first.character()),
        );
        tracing::debug!(?m, ?kind, "wrap node");
        self.nodes.push(node);
    }

    pub fn nodes(&self) -> Vec<SyntaxNode> {
        self.nodes.clone()
    }
}

/// If an Operation in this while loop took more than 1 second it will panic
#[macro_export]
macro_rules! time_bound_while {
    ($cond:expr, $body:block) => {{
        #[cfg(debug_assertions)]
        {
            let start = std::time::Instant::now();
            while $cond {
                if start.elapsed() >= std::time::Duration::from_secs(1) {
                    panic!("Operation took more than 1 sec, possible infinite loop.");
                }
                $body
            }
        }
        #[cfg(not(debug_assertions))]
        {
            while $cond {
                $body
            }
        }
    }};
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
fn parse_attached_modifiers(p: &mut Parser) {
    dbg!("from parse_attached_modifiers:", p.current());
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

fn parse_inline_code(p: &mut Parser) {
    todo!()
}

fn parse_null_modifier(p: &mut Parser) {
    todo!()
}

fn parse_inline_maths(p: &mut Parser) {
    todo!()
}

fn parse_variable(p: &mut Parser) {
    todo!()
}

fn parse_strike_through(p: &mut Parser) {
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

// document: $ => repeat1(
//   choice(
//     prec(1,
//       choice(
//         alias($.paragraph_break, "_paragraph_break"),
//         alias($.line_break, "_line_break"),
//         $.heading,
//         $.nestable_detached_modifier,
//         $.rangeable_detached_modifier,
//         $.table,
//         $.tag,
//         $.horizontal_line,
//         $.strong_paragraph_delimiter,
//       )
//     ),
//     $.paragraph,
//   )
// ),
//
// TODO: heading, nestable_detached_modifier, rangeable_detached_modifier, table, tag,
// horizontal_line, strong_paragraph_delimiter
pub fn document(p: &mut Parser) -> SyntaxNode {
    let m = p.start();

    while !p.is_at_eof() {
        p.eat_many(syntax_set!(WhiteSpace, Tab));

        match p.current() {
            T![LineEnding] => {
                let _ = para_break(p);
                // we must eat every WhiteSpace and Tab after newline before making any assumptions
                p.eat_many(syntax_set!(WhiteSpace, Tab));
            }
            T![Asterisk] => heading(p),
            _ => paragraph(p),
        }
    }
    /*
        paragraph_break(),
        line_break(),
        heading(),
        nestable_detached_modifier(),
        rangeable_detached_modifier(),
        table(),
        tag(),
        horizontal_line(),
        strong_paragraph_delimiter(),
        paragraph(),
    */
    p.wrap(m, T![Document]);
    std::mem::take(&mut p.nodes[0])
}

// // Any of the following choices are valid IN-LINE elements. Any
// // multitude of these are combined to form a `paragraph_segment`.
// _paragraph_element: $ =>
// choice(
//   alias($.word, "_word"),
//   alias($.space, "_space"),
//   alias($.trailing_modifier, "_trailing_modifier"),
//   $.link,
//   $.anchor_declaration,
//   $.anchor_definition,
//   $.inline_link_target,
//   $.escape_sequence,
//   seq(
//     optional($.link_modifier),
//     $.attached_modifier,
//     optional($.link_modifier),
//   ),
//   alias($.link_modifier, "_word"),
//   alias($._conflict_close, "_word"),
// ),
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

fn attributes(p: &mut Parser) {
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
fn file_location(p: &mut Parser) {}
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
            _ => p.eat(),
        }
    }
    p.wrap(m, T![ParaSegment]);
}

fn paragraph(p: &mut Parser) {
    let m = p.start();
    while !p.is_at_eof() {
        if p.current() == SyntaxKind::LineEnding {
            if para_break(p) == SyntaxKind::ParaBreak {
                break;
            }
        } else {
            paragraph_segment(p);
        }
    }
    p.wrap(m, SyntaxKind::Paragraph);
}

// every newline ('\n') a.k.a `LineEnding` must only be parsed with para_break
// cuz we reset span here [ line += 1 && col = 0 ] via ['eat_line_ending']
fn para_break(p: &mut Parser) -> SyntaxKind {
    //
    // ParaBreak = LineEnding + LineEnding | LineEnding + WhiteSpace + LineEnding
    //
    // checks if this is a ParaBreak else return LineEnding
    let m = p.start();
    p.eat_line_ending();
    if let Some(k) = p.next() {
        if p.at(SyntaxKind::LineEnding) {
            p.eat_line_ending();
            p.wrap(m, SyntaxKind::ParaBreak);
            return SyntaxKind::ParaBreak;
        } else if p.at(SyntaxKind::WhiteSpace) && k == SyntaxKind::LineEnding {
            p.eat();
            p.eat_line_ending();
            p.wrap(m, SyntaxKind::ParaBreak);
            return SyntaxKind::ParaBreak;
        }
    }
    SyntaxKind::LineEnding
}

fn quote(p: &mut Parser) {
    let m = p.start();
    p.eat_many(syntax_set!(LessThan));
    p.expect(T![WhiteSpace]);
    paragraph_segment(p);
    p.wrap(m, T![Quote]);
}

fn strong_paragraph_delimiter() {
    todo!()
}

fn horizontal_line() {
    todo!()
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

fn parse_subscript(p: &mut Parser) {
    todo!()
}

fn parse_superscript(p: &mut Parser) {
    todo!()
}

fn parse_spoiler(p: &mut Parser) {
    todo!()
}

fn parse_underline(p: &mut Parser) {
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

assert_tree!(
    // [case:1/9] perfect bold
    italics_01,
    parse_italics,
    "/an italics text chunk/ blah blah blah"
);

assert_tree!(
    // [case:2/9] not so perfect italics
    // error: Unclosed delimiter
    italics_02,
    parse_italics,
    "/an italics text chunk blah blah blah"
);

assert_tree!(
    // [case:3/9] not so perfect italics
    // error: WhiteSpace at beginning
    italics_03,
    parse_italics,
    "/ an italics text chunk/ blah blah blah"
);

assert_tree!(
    // [case:4/9] not so perfect italics
    // error: WhiteSpace at end
    italics_04,
    parse_italics,
    "/an italics text chunk / blah blah blah"
);

assert_tree!(
    // [case:5/9] not so perfect italics
    // error: WhiteSpace at both end
    italics_05,
    parse_italics,
    "/ an italics text chunk / blah blah blah"
);

assert_tree!(
    // [case:6/9] not so perfect italics
    // error: `LineEnding` inside text chunk
    italics_06,
    parse_italics,
    "/an italics \n text chunk/ blah blah blah"
);

assert_tree!(
    // [case:7/9] not so perfect italics
    // feat: other inline elements inside this ATACHED_MODIFIERS
    italics_07,
    parse_italics,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/ blah blah blah"
);

assert_tree!(
    // [case:8/9] not so perfect italics
    // feat: /this/is still italics/ - cuz no space after `/`
    italics_08,
    parse_italics,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/blah/ blah blah"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics_09,
    parse_italics,
    "/an italics text chunk and it have a | verbatim | chunk in it :)/ blah/ blah blah"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics_10,
    parse_italics,
    "//this//"
);

assert_tree!(
    // [case:9/9] not so perfect italics
    // feat: /this/ is not fully italics/ - cuz space after `/`
    italics_11,
    parse_italics,
    "//this is *bold* //"
);

#[tracing::instrument(skip_all)]
fn parse_atmod(p: &mut Parser) {}

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
            $crate::parser::$parse_fn(&mut p);
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
