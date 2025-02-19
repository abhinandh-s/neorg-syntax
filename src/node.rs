#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxElement {
    Inner(Arc<InnerNode>),
    Leaf(LeafNode),
    Error(Arc<ErrorNode>),
}

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

use std::fmt::Write;
use std::sync::Arc;

use crate::{Span, SyntaxKind, Token};

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
                    "{}<ERROR: {} - {} @ {}>",
                    pad, err.text, err.hint, err.span
                );
            }
        }
    }
}

impl SyntaxElement {
    pub fn fmt_pretty(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        let pad = "│   ".repeat(indent);

        match self {
            SyntaxElement::Leaf(leaf) => {
                let token = &leaf.token;
                writeln!(
                    f,
                    "{}{}@{} {:?}",
                    pad,
                    token.kind(),
                    token.offset(),
                    token.text()
                )
            }
            SyntaxElement::Inner(inner) => {
                writeln!(f, "{}{}", pad, inner.kind)?;
                for child in &inner.children {
                    child.fmt_pretty(f, indent + 1)?;
                }
                Ok(())
            }
            SyntaxElement::Error(err) => {
                writeln!(
                    f,
                    "{}<ERROR: {} - {} - @{}>",
                    pad, err.text, err.hint, err.span
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNode(pub SyntaxElement);

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
pub struct InnerNode {
    pub kind: SyntaxKind,
    /// Whether this node or any of its children are erroneous.
    pub erroneous: bool,
    pub children: Vec<SyntaxElement>,
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
