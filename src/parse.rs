use std::sync::Arc;

use crate::Token;
use crate::kind::SyntaxKind;

type Node = Arc<SyntaxNode>;

#[derive(Debug)]
pub enum SyntaxElemet {
    Node(Node),
    Token(Token),
}

// Parses emphasized content: `_Emphasized_`.
fn emph(p: &mut Parser) {
     let m = p.cursor;
     p.assert(SyntaxKind::Underscore);
//     markup(p, false, true, syntax_set!(Underscore, RightBracket, End));
//     p.expect_closing_delimiter(m, SyntaxKind::Underscore);
//     p.wrap(m, SyntaxKind::Emph);
}

pub struct Parser {
    pub source: Vec<Token>,
    pub cursor: usize,
    pub nodes: SyntaxNode,
}

// -- TODO: remove all these pub items
impl Parser {
    pub fn eat(&mut self) {
        let t = self.source[self.cursor].clone();
        let s = SyntaxElemet::Token(t);
        self.nodes.children.push(s);
    }
    // Assert that we are at the given [`SyntaxKind`] and eat it. This should
    // be used when moving between functions that expect to start with a
    // specific token.
    #[track_caller]
    pub fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.source[self.cursor].kind, kind);
        self.eat();
    }
     /// Consume the given closing delimiter or produce an error for the matching
    /// opening delimiter at `open`.
    #[track_caller]
    fn expect_closing_delimiter(&mut self, open: Marker, kind: SyntaxKind) {
        if !self.eat_if(kind) {
            self.nodes[open.0].convert_to_error("unclosed delimiter");
        }
    }
}
impl Parser {
    pub fn new(source: Vec<Token>) -> Self {
        Self {
            source,
            cursor: 0,
            nodes: SyntaxNode {
                kind: SyntaxKind::Document,
                children: vec![],
            },
        }
    }

    //   /// Eat the current token by saving it to the `nodes` vector, then move
    // /// the lexer forward to prepare a new token.
    // fn eat(&mut self) {
    //     self.nodes.children.push(self.source[self.cursor]);
    //     self.token = Self::lex(&mut self.nodes, &mut self.lexer, self.nl_mode);
    // }
}

#[derive(Debug)]
pub struct SyntaxNode {
    pub kind: SyntaxKind,
    pub children: Vec<SyntaxElemet>,
}

impl SyntaxNode {
    pub fn new(kind: SyntaxKind) -> Self {
        Self {
            kind,
            children: Vec::new(),
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn set_kind(&mut self, kind: SyntaxKind) {
        self.kind = kind;
    }
}

// impl std::fmt::Display for SyntaxNode {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.fmt_with_indent(f, 0)
//     }
// }
//
// impl SyntaxNode {
//     fn fmt_with_indent(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
//         // Write indentation
//         for _ in 0..indent {
//             write!(f, "  ")?; // 2 spaces per level
//         }
//
//         // Write current node's kind
//         writeln!(f, "{}", self.kind)?;
//
//         // Recursively write children
//         for child in &self.children {
//             child.fmt_with_indent(f, indent + 1)?;
//         }
//
//         Ok(())
//     }
// }
