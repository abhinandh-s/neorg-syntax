#![deny(clippy::print_stdout, clippy::print_stderr)]

use crate::node::SyntaxNode;
use crate::*;

/// A marker representing a node's position in the parser. Mainly used for
/// wrapping, but can also index into the parser to access the node, like
/// `p[m]`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) struct Marker(usize);

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
    pub(crate) tokens: Vec<Token>,
    pub(crate) cursor: usize,
    /// Whether the parser has the expected set of open/close delimiters. This
    /// only ever transitions from `true` to `false`.
    pub(crate) balanced: bool,
    pub(crate) loc: Location,
    /// Nodes representing the concrete syntax tree of previously parsed text.
    /// In Code and Math, includes previously parsed trivia, but not `token`.
    pub(crate) nodes: Vec<SyntaxNode>,
}

#[allow(dead_code)]
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

    pub(crate) fn eat_line_ending(&mut self) {
        self.eat();
        self.loc.bump_line(1);
    }

    /// A marker that will point to the current token in the parser once it's
    /// been eaten.
    pub(crate) fn start(&self) -> Marker {
        Marker(self.nodes.len())
    }

    #[track_caller]
    pub(crate) fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind);
    }

    #[track_caller]
    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind);
        self.eat();
    }

    #[track_caller]
    pub(crate) fn get_current(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.cursor).map(|t| t.kind())
    }

    #[track_caller]
    pub(crate) fn current(&self) -> SyntaxKind {
        self.tokens[self.cursor].kind()
    }

    pub(crate) fn next(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.cursor + 1).map(|f| f.kind())
    }

    pub(crate) fn prev(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.cursor - 1).map(|f| f.kind())
    }

    #[track_caller]
    pub(crate) fn eat(&mut self) {
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
        let start_char = self.loc.character();

        let current = self.tokens[self.cursor].clone();
        self.loc.bump_col(current.len() as u32);
        self.loc.bump_offset(current.len());
        let node = SyntaxNode::leaf(
            current.clone(),
            Location::new(start_offset, self.loc.line(), start_char),
        );
        self.nodes.push(node);
        self.cursor += 1;
    }

    pub(crate) fn eat_many(&mut self, eatable: SyntaxSet) {
        while self.at_set(eatable) {
            self.eat();
        }
    }

    // eats many `WhiteSpace` & `Tab`
    pub(crate) fn skip_whitespace(&mut self) {
        while self.at_set(syntax_set!(WhiteSpace, Tab)) {
            self.eat();
        }
    }
    /// Eat the token if at `kind`. Returns `true` if eaten.
    ///
    /// Note: In Math and Code, this will ignore trivia in front of the
    /// `kind`, To forbid skipping trivia, consider using `eat_if_direct`.
    pub(crate) fn eat_if(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.eat();
        }
        at
    }

    /// Consume the given `kind` or produce an error.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        let at = self.at(kind);
        if at {
            self.eat();
        } else {
            let prev = self.prev().map_or("".to_owned(), |f| f.text());
            let node = self.eat_and_get();
            node.convert_to_error(format!("expected `{}`", kind.text()));
            node.hint(format!(
                "consider using a `{}` after `{}`",
                kind.text(),
                prev
            ));
        }
        at
    }

    /// Consume the given closing delimiter or produce an error for the matching
    /// opening delimiter at `open`.
    #[track_caller]
    pub(crate) fn expect_closing_delimiter(&mut self, open: Marker, kind: SyntaxKind) {
        if !self.eat_if(kind) {
            self.nodes[open.0].convert_to_error("unclosed delimiter");
        }
    }

    /// Eat the current node and return a reference for in-place mutation.
    #[track_caller]
    pub(crate) fn eat_and_get(&mut self) -> &mut SyntaxNode {
        let offset = self.nodes.len();
        self.eat();
        &mut self.nodes[offset]
    }
    /// Consume the next token (if any) and produce an error stating that it was
    /// unexpected.
    pub(crate) fn unexpected(&mut self) {
        self.eat_and_get().unexpected();
    }
    /// Produce an error that the given `thing` was expected at the position
    /// of the marker `m`.
    pub(crate) fn expected_at(&mut self, m: Marker, thing: &str) {
        let error = SyntaxNode::error(SyntaxError::new(format!("expected {thing}"), self.loc), "");
        self.nodes.insert(m.0, error);
    }

    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    pub(crate) fn at_set(&self, set: SyntaxSet) -> bool {
        set.contains(self.current())
    }

    #[track_caller]
    pub(crate) fn is_at_eof(&self) -> bool {
        self.current() == T![Eof]
    }

    // Range:
    //  Pos: line, col
    //  Pos: line, col
    //  Span: start, end
    pub(crate) fn wrap(&mut self, m: Marker, kind: SyntaxKind) {
        let drained: Vec<SyntaxNode> = self.nodes.drain(m.0..).collect();

        let first = drained.first().map_or(Location::detached(), |f| f.loc());

        let node = SyntaxNode::inner(
            kind,
            drained,
            Location::new(first.offsets(), first.line(), first.character()),
        );
        self.nodes.push(node);
    }

    pub fn nodes(&self) -> Vec<SyntaxNode> {
        self.nodes.clone()
    }

    pub fn balanced(&self) -> bool {
        self.balanced
    }

    pub fn set_balanced(&mut self, balanced: bool) {
        self.balanced = balanced;
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
