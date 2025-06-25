#![allow(dead_code)]

use crate::node::SyntaxNode;
use crate::{kind, syntax_set, token, Lexer, SyntaxKind, SyntaxSet, Token};

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
    fn assert(&self, kind: SyntaxKind) {
        assert_eq!(self.current(), kind);
    }

    pub fn current(&self) -> SyntaxKind {
        self.tokens[self.cursor].kind()
    }

    fn eat(&mut self) {
        let node = SyntaxNode::leaf(self.tokens[self.cursor].clone());
        self.nodes.push(node);
        self.cursor += 1;
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

#[tracing::instrument(skip_all)]
pub fn parse_verbatim(p: &mut Parser) {
    let m = p.start();
    let set = syntax_set!(Pipe, Eof);
    while !p.at_set(set) {
        p.eat();
    }
    p.wrap(m, SyntaxKind::Verbatim);
}

#[macro_export]
macro_rules! set_insta_env {
    () => {
        fn set() {
            let key = "INSTA_UPDATE";
            unsafe {
                std::env::set_var(key, "allow");
            }
        }
    }
}

#[macro_export]
macro_rules! assert_tree {
    ($test_name:ident, $parse_fn:ident, $input:literal) => {
        #[test]
        fn $test_name() {
            let mut p = $crate::Parser::new($input);
            $crate::$parse_fn(&mut p);
            assert_eq!(p.nodes().len(), 1);
            let vec = p.nodes();
            let str = vec[0].pretty_string();
        insta::with_settings!({ remove_input_file => , snapshot_path => "tests/snapshots"}, {
        insta::assert_snapshot!(stringify!($test_name), str);
});    
        }
    };
}


#[cfg(test)]
mod test {
    
    // set_insta_env!();
    
    assert_tree!(parse_verbatim001, parse_verbatim, "this is a test");
    assert_tree!(parse_verbatim002, parse_verbatim, " this is a test ");
}
