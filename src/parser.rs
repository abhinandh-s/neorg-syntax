#![allow(dead_code)]

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
            balanced: false,
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

    fn eat(&mut self) {
        let node = SyntaxNode::leaf(self.tokens[self.cursor].clone());
        self.nodes.push(node);
        self.cursor += 1;
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
        // 
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
        println!("{:?}", drained);
        tracing::debug!(?drained, "wrap node");
        let node = SyntaxNode::inner(kind, drained);
        tracing::debug!(?m, ?kind, "wrap node");
        self.nodes.push(node);
    }

    pub fn nodes(&self) -> Vec<SyntaxNode> {
        self.nodes.clone()
    }
}

/// # cases
///
/// all these cases are covered.
///
/// ```ignore
/// /this is italics/  
/// / this is not italics/
/// /this is not italics /
/// / this is not italics /
/// ```
#[tracing::instrument(skip_all)]
pub fn parse_italics(p: &mut Parser) {
    let m = p.start();
    p.assert(T!('/')); // assert & consume '/'
    p.eat();
    let _tok = p.current();
    // if tok.is_inline_expr() {
    //     parse_inline(p);
    // } else {
    parse_text_chunk(p);
    // }

    p.expect_closing_delimiter(m, T!('/'));
    p.wrap(m, SyntaxKind::Emph);
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
fn parse_text_chunk(p: &mut Parser) {
    let m = p.start();
    let set = syntax_set!(Pipe, Eof);

    while !p.at_set(set) && p.at_set(syntax_set!(Word, WhiteSpace)) {
        p.eat();
    }
    if let Some(n) = p.nodes.last_mut() {
        if n.kind() == SyntaxKind::WhiteSpace {
            n.unexpected();
        }
    }
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

    while !p.at_set(syntax_set!(LineEnding, Eof)) {
        if p.at_set(PUNCTUATION) {
            parse_inline(p);
        } else {
            parse_text_chunk(p);
        }
    }

    p.wrap(m, SyntaxKind::ParaSegment);
}

fn parse_inline(p: &mut Parser) {
    match p.current() {
        SyntaxKind::Pipe => parse_verbatim(p),
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
    assert_tree!(parse_verbatim_05, parse_verbatim, "|this is a test");
    assert_tree!(parse_verbatim_01, parse_verbatim, "|this is a test|");
    assert_tree!(parse_verbatim_02, parse_verbatim, "| this is a test |");
    assert_tree!(
        parse_verbatim_03,
        parse_verbatim,
        r###"|~ this is a test !"#$%&'()*+,-./:;<=>?@[]^_`{}~\thre|"###
    );
    assert_tree!(parse_verbatim_04, parse_verbatim, r###"|~ `{}~\thre|"###);
    assert_tree!(parse_text_chunk_01, parse_text_chunk, "this is text chunk");
    assert_tree!(
        parse_text_chunk_01_err,
        parse_text_chunk,
        "this is text chunk / not this"
    );
    assert_tree!(
        parse_para_segment_01,
        parse_para_segment,
        "this is |a verbatim text chunk|"
    );
      assert_tree!(
        parse_para_segment_01_err,
        parse_para_segment,
        "this is |a verbatim text chunk"
    );

}
