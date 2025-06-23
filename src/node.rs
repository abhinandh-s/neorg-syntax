use std::fmt::Write;
use std::sync::Arc;

use crate::{Span, SyntaxKind, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SyntaxElement {
    Inner(Arc<InnerNode>),
    Leaf(LeafNode),
    Error(Arc<ErrorNode>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNode(SyntaxElement);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorNode {
    pub kind: SyntaxKind,
    pub text: String,
    pub hint: String,
    pub span: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LeafNode {
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct InnerNode {
    pub(crate) kind: SyntaxKind,
    /// Whether this node or any of its children are erroneous.
    pub(crate) erroneous: bool,
    pub(crate) children: Vec<SyntaxElement>,
}


#[allow(dead_code)]
impl SyntaxElement {
    pub fn span(&self) -> Span {
        match self {
            SyntaxElement::Leaf(leaf) => {
                let start = leaf.token.offset();
                let end = start + leaf.token.text().len();
                Span { start, end }
            }
            SyntaxElement::Inner(inner) => inner.span(),
            SyntaxElement::Error(err) => Span {
                start: err.span,
                end: err.span,
            },
        }
    }
}

impl InnerNode {
    pub fn pretty_string(&self) -> String {
        let mut out = String::new();
        let _ = writeln!(&mut out, "{}", self.kind);
        for child in &self.children {
            child.write_pretty(&mut out, 1);
        }
        out
    }
    pub fn span(&self) -> Span {
        let start = self.children.first().map_or(1_usize, |_| 1_usize);
        Span { start, end: start }
    }
}

impl SyntaxElement {
    pub fn write_pretty(&self, out: &mut String, indent: usize) {
        let pad = "│   ".repeat(indent);

        match self {
            SyntaxElement::Leaf(leaf) => {
                let token = &leaf.token;
                let _ = writeln!(
                    out,
                    "{}{}@{} {:?}",
                    pad,
                    token.kind(),
                    token.offset(),
                    token.text()
                );
            }
            SyntaxElement::Inner(inner) => {
                let _ = writeln!(out, "{}{}", pad, inner.kind);
                for child in &inner.children {
                    child.write_pretty(out, indent + 1);
                }
            }
            SyntaxElement::Error(err) => {
                let _ = writeln!(
                    out,
                    "{}{}@{} > {} > {}",
                    pad, err.kind, err.span, err.text, err.hint
                );
            }
        }
    }
}

impl InnerNode {
    pub fn new(kind: SyntaxKind) -> Self {
        Self {
            kind,
            erroneous: false,
            children: Vec::new(),
        }
    }
}
