#![allow(dead_code)]
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
            nodes: Vec::new(),
        }
    }

    /// A marker that will point to the current token in the parser once it's
    /// been eaten.
    fn start(&self) -> Marker {
        Marker(self.nodes.len())
    }

    #[track_caller]
    fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind);
        self.eat();
    }

    fn current(&self) -> SyntaxKind {
        self.tokens[self.cursor].kind()
    }

    fn next(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.cursor + 1).map(|f| f.kind())
    }

    fn eat(&mut self) {
        let node = SyntaxNode::leaf(self.tokens[self.cursor].clone());
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

    fn wrap(&mut self, m: Marker, kind: SyntaxKind) {
        let drained = self.nodes.drain(m.0..).collect();
        dbg!("called wrap");
        tracing::debug!(?drained, "wrap node");
        let node = SyntaxNode::inner(kind, drained);
        tracing::debug!(?m, ?kind, "wrap node");
        self.nodes.push(node);
    }

    pub fn nodes(&self) -> Vec<SyntaxNode> {
        self.nodes.clone()
    }
}

#[tracing::instrument(skip_all)]
fn parse_variable(p: &mut Parser) {
    let m = p.start();
    p.assert(T!('&'));
    if p.current() == SyntaxKind::WhiteSpace {
        p.unexpected();
    }
    parse_atmod_text_chunk(p);

    p.expect_closing_delimiter(m, T!('&'));
    p.wrap(m, SyntaxKind::Variable);
}

#[tracing::instrument(skip_all)]
fn parse_inline_maths(p: &mut Parser) {
    let m = p.start();
    p.assert(T!('$'));
    if p.current() == SyntaxKind::WhiteSpace {
        p.unexpected();
    }
    parse_atmod_text_chunk(p);

    p.expect_closing_delimiter(m, T!('$'));
    p.wrap(m, SyntaxKind::InlineMath);
}

#[tracing::instrument(skip_all)]
fn parse_inline_code(p: &mut Parser) {
    let m = p.start();
    p.assert(T!('`'));
    if p.current() == SyntaxKind::WhiteSpace {
        p.unexpected();
    }
    parse_atmod_text_chunk(p);

    p.expect_closing_delimiter(m, T!('`'));
    p.wrap(m, SyntaxKind::InlineCode);
}

#[tracing::instrument(skip_all)]
fn parse_subscript(p: &mut Parser) {
    let m = p.start();
    p.assert(T!(','));
    if p.current() == SyntaxKind::WhiteSpace {
        p.unexpected();
    }
    parse_atmod_text_chunk(p);

    p.expect_closing_delimiter(m, T!(','));
    p.wrap(m, SyntaxKind::Subscript);
}

#[tracing::instrument(skip_all)]
fn parse_superscript(p: &mut Parser) {
    let m = p.start();
    p.assert(T!('^'));
    if p.current() == SyntaxKind::WhiteSpace {
        p.unexpected();
    }
    parse_atmod_text_chunk(p);

    p.expect_closing_delimiter(m, T!('^'));
    p.wrap(m, SyntaxKind::Superscript);
}

define_attached_modifier!(parse_bold, Asterisk, Bold);
define_attached_modifier!(parse_underline, Underscore, UnderLine);
define_attached_modifier!(parse_strike_through, Hyphen, StrikeThrough);
define_attached_modifier!(parse_spoiler, Exclamation, Spoiler);
define_attached_modifier!(parse_null_modifier, Percent, NullModifier);
define_attached_modifier!(parse_italics, Slash, Italics);

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

#[macro_export]
macro_rules! define_attached_modifier {
    ($fn_name:ident, $kind:ident, $wrapin:ident) => {
        #[tracing::instrument(skip_all)]
        fn $fn_name(p: &mut Parser) {
            let m = p.start();
            p.assert(SyntaxKind::$kind);
            if p.current() == SyntaxKind::WhiteSpace {
                p.unexpected();
            }
            time_bound_while!(!p.at_set(syntax_set!(Eof)), {
                if p.at_set(ATACHED_MODIFIERS.add(SyntaxKind::Pipe)) {
                    if p.at(SyntaxKind::$kind) && (p.next() == Some(SyntaxKind::Word)) {
                        p.eat();
                        continue;
                    } else if p.at(SyntaxKind::$kind) && (p.next() == Some(SyntaxKind::WhiteSpace))
                    {
                        break;
                    }
                    parse_attached_modifiers(p);
                } else {
                    parse_atmod_text_chunk(p);
                }
            });

            assert!(
                p.at(SyntaxKind::$kind) && (p.next() == Some(SyntaxKind::Word))
                    || p.at_set(ATACHED_MODIFIERS.add(SyntaxKind::Eof))
            );
            p.expect_closing_delimiter(m, SyntaxKind::$kind);
            p.wrap(m, SyntaxKind::$wrapin);
        }
    };
}

assert_tree!(
    // [case:1/7] perfect bold
    bold_01,
    parse_bold,
    "*a bold text chunk* blah blah blah"
);

assert_tree!(
    // [case:2/7] not so perfect bold
    // error: Unclosed delimiter
    bold_02,
    parse_bold,
    "*a bold text chunk blah blah blah"
);

assert_tree!(
    // [case:3/7] not so perfect bold
    // error: WhiteSpace at beginning
    bold_03,
    parse_bold,
    "* a bold text chunk* blah blah blah"
);

assert_tree!(
    // [case:4/7] not so perfect bold
    // error: WhiteSpace at end
    bold_04,
    parse_bold,
    "*a bold text chunk * blah blah blah"
);

assert_tree!(
    // [case:5/7] not so perfect bold
    // error: WhiteSpace at both end
    bold_05,
    parse_bold,
    "* a bold text chunk * blah blah blah"
);

assert_tree!(
    // [case:6/7] not so perfect bold
    // error: `LineEnding` inside text chunk
    bold_06,
    parse_bold,
    "*a bold \n text chunk* blah blah blah"
);

assert_tree!(
    // [case:7/7] not so perfect bold
    // feat: other inline elements inside this ATACHED_MODIFIERS
    bold_07,
    parse_bold,
    "*a bold text chunk and it have a | verbatim | chunk in it :)* blah blah blah"
);

/// # Deals with:
///
/// - normal TextChunk
/// - inline TextChunk
/// - inline elements
///
///
/// # Paragraph Break
///
/// A paragraph break is defined as an _empty line_. In the simplest case that means two consecutive
/// `line endings` but since Neorg is a /free-form/ markup language, a line which only contains
/// whitespace is also considered empty.
#[tracing::instrument(skip_all)]
pub fn parse_paragraph(p: &mut Parser) {
    let m = p.start();

    time_bound_while!(!p.at(SyntaxKind::Eof), {
        // a paragraph segment stops at `LineEnding`.
        // ie, after a loop if p is at `LineEnding` or `WhiteSpace`
        if let Some(k) = p.next() {
            if p.at(SyntaxKind::LineEnding) {
                let m = p.start();
                p.eat();
                p.wrap(m, SyntaxKind::ParaBreak);
            } else if p.at(SyntaxKind::WhiteSpace) && k == SyntaxKind::LineEnding {
                let m = p.start();
                p.eat();
                p.eat();
                p.wrap(m, SyntaxKind::ParaBreak);
            } else {
                // still p is at WhiteSpace
                parse_para_segment(p);
            }
        }
    });
    p.wrap(m, SyntaxKind::Paragraph);
}

/// # Verbatim Paragraph Segments
///  
/// These are structurally equivalent to regular `paragraph segments` with a single exception.
/// Verbatim paragraph segments are built up from _only_ `words`. This means that attached
/// modifiers and linkables are simply parsed as raw text within a verbatim paragraph segment.
#[tracing::instrument(skip_all)]
fn parse_verbatim(p: &mut Parser) {
    let m = p.start();
    p.assert(T!('|'));

    parse_verbatim_chunk(p);

    p.expect_closing_delimiter(m, T!('|'));
    p.wrap(m, SyntaxKind::Verbatim);
}

#[tracing::instrument(skip_all)]
fn parse_verbatim_chunk(p: &mut Parser) {
    let m = p.start();
    let set = syntax_set!(Pipe, Eof);
    while !p.at_set(set) {
        p.eat();
    }
    p.wrap(m, SyntaxKind::TextChunk);
}

#[tracing::instrument(skip_all)]
fn parse_nm_text_chunk(p: &mut Parser) {
    let m = p.start();

    while p.at_set(syntax_set!(Word, WhiteSpace, Dot)) {
        p.eat();
    }
    p.wrap(m, SyntaxKind::TextChunk);
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

#[tracing::instrument(skip_all)]
fn parse_atmod_text(p: &mut Parser, stop_at: SyntaxSet) {
    let m = p.start();

    while !p.at_set(stop_at.add(SyntaxKind::Eof)) {
        if p.at(SyntaxKind::LineEnding) {
            p.unexpected();
        }
        p.eat();
    }

    if p.at_set(ATACHED_MODIFIERS) {
        if let Some(n) = p.nodes.last_mut() {
            if n.kind() == SyntaxKind::WhiteSpace {
                n.unexpected();
            }
        }
    }

    p.wrap(m, SyntaxKind::TextChunk);
}

#[tracing::instrument(skip_all)]
fn parse_atmod_text_chunk(p: &mut Parser) {
    let m = p.start();
    let set = ATACHED_MODIFIERS.add(SyntaxKind::Pipe);

    while !p.at_set(set.add(SyntaxKind::Eof)) {
        if p.at(SyntaxKind::LineEnding) {
            //           p.balanced = false;
            p.unexpected();
        }
        p.eat();
    }

    if p.at_set(ATACHED_MODIFIERS) {
        //  if p.balanced {
        if let Some(n) = p.nodes.last_mut() {
            if n.kind() == SyntaxKind::WhiteSpace {
                n.unexpected();
            }
        }
    }
    // } else {
    //     p.balanced = true
    // }
    p.wrap(m, SyntaxKind::TextChunk);
}
/// # Paragraph Segments
///
/// `Words` are first combined into *paragraph segments*. A paragraph segment
/// may then contain any inline element of type:
///
/// - attached modifiers
/// - linkables
///
/// Usually, a [`line ending`] terminates the paragraph segment.
/// This means that a paragraph segment is simply a line of text:
///
/// # example
///
/// I am a paragraph segment.
/// I am another paragraph segment.
/// Together we form a paragraph.
///
#[tracing::instrument(skip_all)]
fn parse_para_segment(p: &mut Parser) {
    let m = p.start();

    time_bound_while!(!p.at_set(syntax_set!(LineEnding, Eof)), {
        if p.at_set(ATACHED_MODIFIERS.add(SyntaxKind::Pipe)) {
            parse_attached_modifiers(p);
        } else {
            let amod_stoper = ATACHED_MODIFIERS
                .add(SyntaxKind::Pipe)
                .add(SyntaxKind::LineEnding)
                .add(SyntaxKind::Eof);
            while !p.at_set(amod_stoper) {
                p.eat();
            }
        }
    });
    p.expect(SyntaxKind::LineEnding);
    p.wrap(m, SyntaxKind::ParaSegment);
}

assert_tree!(
    long_para_segment_01,
    parse_para_segment,
    "this is a *bold* /italic/ _underline_   -strike-through- !spoiler! ^superscript^ ,subscript, `inline code` %null modifier% &variable& "
);

/// Their name should be rather self-explanatory - both the opening and closing modifier
/// are _attached_ to one another.
///
/// The following attached modifiers exist and have respective meaning:
/// - *bold*
/// - /italic/
/// - _underline_
/// - -strike-through-
/// - !spoiler!
/// - ^superscript^ (cannot be nested into `subscript`)
/// - ,subscript, (cannot be nested into `superscript`)
/// - `inline code` (disables any nested markup - verbatim)
/// - `%null modifier%`
/// - $f(x) = y$ (verbatim)
/// - &variable& (verbatim)
fn parse_attached_modifiers(p: &mut Parser) {
    dbg!("from parse_attached_modifiers:", p.current());
    match p.current() {
        SyntaxKind::Asterisk => parse_bold(p),
        SyntaxKind::Slash => parse_italics(p),
        SyntaxKind::Underscore => parse_underline(p),
        SyntaxKind::Exclamation => parse_spoiler(p),
        SyntaxKind::Caret => parse_superscript(p),
        SyntaxKind::Comma => parse_subscript(p),
        SyntaxKind::Backtick => parse_inline_code(p),
        SyntaxKind::Percent => parse_null_modifier(p),
        SyntaxKind::Dollar => parse_inline_maths(p),
        SyntaxKind::Ampersand => parse_variable(p),
        SyntaxKind::Pipe => parse_verbatim(p),
        SyntaxKind::Hyphen => parse_strike_through(p),
        _ => panic!("unimplemented inline!"),
    }
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

#[cfg(test)]
mod test {
    assert_tree!(verbatim_05, parse_verbatim, "|this is a test");
    assert_tree!(verbatim_01, parse_verbatim, "|this is a test|");
    assert_tree!(verbatim_02, parse_verbatim, "| this is a test |");
    assert_tree!(
        verbatim_03,
        parse_verbatim,
        r###"|~ this is a test !"#$%&'()*+,-./:;<=>?@[]^_`{}~\thre|"###
    );
    assert_tree!(verbatim_04, parse_verbatim, r###"|~ `{}~\thre|"###);
    assert_tree!(text_chunk_01, parse_atmod_text_chunk, "this is text chunk");
    assert_tree!(
        text_chunk_01_err,
        parse_atmod_text_chunk,
        "this is text chunk / not this"
    );
    assert_tree!(
        para_segment_01,
        parse_para_segment,
        "this is |a verbatim text chunk|"
    );
    assert_tree!(
        para_segment_01_err,
        parse_para_segment,
        "this is |a verbatim text chunk"
    );
    assert_tree!(
        para_segment_02_err,
        parse_para_segment,
        "this is |a verbatim text chunk|\nthis is next"
    );
    assert_tree!(
        para_segment_03_err,
        parse_para_segment,
        "this isrbatim text chunk\nthis is next"
    );
    assert_tree!(
        paragraph_01,
        parse_paragraph,
        "this isrbatim text chunk\nthis is next"
    );
    assert_tree!(
        paragraph_02,
        parse_paragraph,
        r#"I am a paragraph segment
I am another paragraph segment
 Together we form a paragraph"#
    );
    assert_tree!(
        paragraph_03,
        parse_paragraph,
        r#"I am a paragraph segment.
I am another paragraph segment.
    Together we form a paragraph."#
    );
    assert_tree!(
        paragraph_with_verbatim_01,
        parse_paragraph,
        r#"I am a paragraph segment.
I am another paragraph segment.
this |is verbatim| content
    Together we form a paragraph."#
    );
    assert_tree!(
        paragraph_with_verbatim_emph_01,
        parse_paragraph,
        r#"I am a paragraph segment.
I am another paragraph segment.
this |is verbatim| content *this is bold*
    Together we form a paragraph."#
    );

    // ParaBreak
    //
    // [case:1/2]
    //
    // `LineEnding` after a `LineEnding`
    assert_tree!(
        parabreak_01,
        parse_paragraph,
        r#" I am a paragraph segment.
I am another paragraph segment.

this |is verbatim| content *this is bold*
    Together we form a paragraph."#
    );

    // ParaBreak
    //
    // [case:2/2]
    //
    // empty line with many `WhiteSpace` followed by `LineEnding`
    assert_tree!(
        parabreak_02,
        parse_paragraph,
        r#" I am a paragraph segment.
I am another paragraph segment.
      
this |is verbatim| content *this is bold*
    Together we form a paragraph."#
    );
    assert_tree!(
        parse_02,
        parse_paragraph,
        r#" I am a paragraph segment.

I am another paragraph segment.
 are _attached_ to one another.
([{}])
  *bold*
  /italic/
  _underline_
  |-strike-through-|
  !spoiler!
  ^superscript^
  ,subscript,
  `inline code`
  `%null modifier%`
  &variable& 
this |is verbatim| content *this is bold*
    Together we form a paragraph."#
    );

    quickcheck::quickcheck! {
        #[test]
        #[ignore]
        fn no_panic_on_random_input(input: String) -> bool {
            let mut parser = crate::Parser::new(&input);
            crate::parse_paragraph(&mut parser);
            true // test passes if no panic
        }
    }

    proptest::proptest! {
        #[test]
        #[ignore]
        fn no_panic_prop(input in ".*") {
            let mut parser = crate::Parser::new(&input);
            crate::parse_paragraph(&mut parser);
        }
    }
}
