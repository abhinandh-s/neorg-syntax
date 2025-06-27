#![allow(dead_code)]

use std::sync::Arc;

use crate::{Span, SyntaxKind, Token, token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNode(Repr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Repr {
    Leaf(LeafNode),
    Inner(Arc<InnerNode>),
    Error(Arc<ErrorNode>),
}

impl SyntaxNode {
    /// Create a new leaf node.
    pub fn leaf(tok: Token) -> Self {
        Self(Repr::Leaf(LeafNode::new(tok)))
    }

    /// Create a new inner node with children.
    pub fn inner(kind: SyntaxKind, children: Vec<SyntaxNode>) -> Self {
        Self(Repr::Inner(Arc::new(InnerNode::new(kind, children))))
    }

    /// Create a new error node.
    pub fn error(error: SyntaxError, text: impl Into<String>) -> Self {
        Self(Repr::Error(Arc::new(ErrorNode::new(error, text))))
    }

    /// Create a dummy node of the given kind.
    ///
    /// Panics if `kind` is `SyntaxKind::Error`.
    #[track_caller]
    pub const fn placeholder(kind: SyntaxKind) -> Self {
        if matches!(kind, SyntaxKind::Error) {
            panic!("cannot create error placeholder");
        }
        Self(Repr::Leaf(LeafNode {
            kind,
            text: String::new(),
            span: Span::detached(),
        }))
    }

    /// The type of the node.
    pub fn kind(&self) -> SyntaxKind {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.kind,
            Repr::Inner(inner) => inner.kind,
            Repr::Error(_) => SyntaxKind::Error,
        }
    }

    /// The span of the node.
    pub fn span(&self) -> Span {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.span,
            Repr::Inner(inner) => inner.span,
            Repr::Error(node) => node.error.span,
        }
    }

    /// The text of the node if it is a leaf or error node.
    ///
    /// Returns the empty string if this is an inner node.
    pub fn text(&self) -> &String {
        static EMPTY: String = String::new();
        match &self.0 {
            Repr::Leaf(leaf) => &leaf.text,
            Repr::Inner(_) => &EMPTY,
            Repr::Error(node) => &node.text,
        }
    }

    /// Extract the text from the node.
    ///
    /// Builds the string if this is an inner node.
    pub fn into_text(self) -> String {
        match self.0 {
            Repr::Leaf(leaf) => leaf.text,
            Repr::Inner(inner) => inner
                .children
                .iter()
                .cloned()
                .map(Self::into_text)
                .collect(),
            Repr::Error(node) => node.text.clone(),
        }
    }

    /// The node's children.
    pub fn children(&self) -> std::slice::Iter<'_, SyntaxNode> {
        match &self.0 {
            Repr::Leaf(_) | Repr::Error(_) => [].iter(),
            Repr::Inner(inner) => inner.children.iter(),
        }
    }

    /// Whether the node or its children contain an error.
    pub fn erroneous(&self) -> bool {
        match &self.0 {
            Repr::Leaf(_) => false,
            Repr::Inner(inner) => inner.erroneous,
            Repr::Error(_) => true,
        }
    }

    /// The error messages for this node and its descendants.
    pub fn errors(&self) -> Vec<SyntaxError> {
        if !self.erroneous() {
            return vec![];
        }

        if let Repr::Error(node) = &self.0 {
            vec![node.error.clone()]
        } else {
            self.children()
                .filter(|node| node.erroneous())
                .flat_map(|node| node.errors())
                .collect()
        }
    }
    /// Add a user-presentable hint if this is an error node.
    pub fn hint(&mut self, hint: impl Into<String>) {
        if let Repr::Error(node) = &mut self.0 {
            Arc::make_mut(node).hint(hint);
        }
    }

    /// Convert the child to an error, if it isn't already one.
    pub(super) fn convert_to_error(&mut self, message: impl Into<String>) {
        if !self.kind().is_error() {
            let text = std::mem::take(self).into_text();
            *self = SyntaxNode::error(SyntaxError::new(message), text);
        }
    }
    /// Convert the child to an error stating that the given thing was
    /// expected, but the current kind was found.
    pub(super) fn expected(&mut self, expected: &str) {
        let kind = self.kind();
        self.convert_to_error(format!("expected {expected}, found {}", kind.text()));
    }

    /// Convert the child to an error stating it was unexpected.
    pub(super) fn unexpected(&mut self) {
        self.convert_to_error(format!("unexpected {}", self.kind().text()));
    }
}

// for 'convert_to_error'
impl Default for SyntaxNode {
    fn default() -> Self {
        Self::leaf(token!(SyntaxKind::Eof, "", 0))
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct LeafNode {
    /// What kind of node this is (each kind would have its own struct in a
    /// strongly typed AST).
    kind: SyntaxKind,
    /// The source text of the node.
    text: String,
    /// The node's span.
    span: Span,
}

impl LeafNode {
    /// Create a new leaf node.
    #[track_caller]
    fn new(token: Token) -> Self {
        debug_assert!(!token.kind().is_error());
        Self {
            kind: token.kind(),
            text: token.text().into(),
            span: Span {
                start: token.len(),
                end: token.len() + token.len(),
            },
        }
    }

    /// The byte length of the node in the source text.
    fn len(&self) -> usize {
        self.text.len()
    }

    /// Whether the two leaf nodes are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.text == other.text
    }
}

impl std::fmt::Debug for LeafNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.kind, self.text)
    }
}

/// An inner node in the untyped syntax tree.
#[derive(Clone, Eq, PartialEq, Hash)]
struct InnerNode {
    /// What kind of node this is (each kind would have its own struct in a
    /// strongly typed AST).
    kind: SyntaxKind,
    /// The node's span.
    span: Span,
    /// Whether this node or any of its children are erroneous.
    erroneous: bool,
    /// This node's children, losslessly make up this node.
    children: Vec<SyntaxNode>,
}

impl InnerNode {
    /// Create a new inner node with the given kind and children.
    #[track_caller]
    fn new(kind: SyntaxKind, children: Vec<SyntaxNode>) -> Self {
        debug_assert!(!kind.is_error());

        let mut erroneous = false;

        for child in &children {
            erroneous |= child.erroneous();
        }

        Self {
            kind,
            span: Span::default(),
            erroneous,
            children,
        }
    }
}

impl std::fmt::Debug for InnerNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)?;
        if !self.children.is_empty() {
            f.write_str(" ")?;
            f.debug_list().entries(&self.children).finish()?;
        }
        Ok(())
    }
}

/// An error node in the untyped syntax tree.
#[derive(Clone, Eq, PartialEq, Hash)]
struct ErrorNode {
    /// The source text of the node.
    text: String,
    /// The syntax error.
    error: SyntaxError,
}

impl ErrorNode {
    /// Create new error node.
    fn new(error: SyntaxError, text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            error,
        }
    }

    /// The byte length of the node in the source text.
    fn len(&self) -> usize {
        self.text.len()
    }

    /// Add a user-presentable hint to this error node.
    fn hint(&mut self, hint: impl Into<String>) {
        self.error.hints.push(hint.into());
    }

    /// Whether the two leaf nodes are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.text == other.text && self.error.spanless_eq(&other.error)
    }
}

impl std::fmt::Debug for ErrorNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Error: {:?} ({})", self.text, self.error.message)
    }
}

/// A syntactical error.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SyntaxError {
    /// The node's span.
    pub span: Span,
    /// The error message.
    pub message: String,
    /// Additional hints to the user, indicating how this error could be avoided
    /// or worked around.
    pub hints: Vec<String>,
}

impl SyntaxError {
    /// Create a new detached syntax error.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            span: Span::default(),
            message: message.into(),
            hints: vec![],
        }
    }

    /// Whether the two errors are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.message == other.message && self.hints == other.hints
    }
}

impl SyntaxNode {
    pub fn display(&self) -> String {
        fn fmt(node: &SyntaxNode, indent: usize, output: &mut String) {
            let padding = "  ".repeat(indent);
            match &node.0 {
                Repr::Leaf(leaf) => {
                    output.push_str(&format!("{padding}{:?}: {:?}\n", leaf.kind, leaf.text));
                }
                Repr::Inner(inner) => {
                    output.push_str(&format!("{padding}{:?}\n", inner.kind));
                    for child in &inner.children {
                        fmt(child, indent + 1, output);
                    }
                }
                Repr::Error(err) => {
                    output.push_str(&format!(
                        "{padding}Error: {:?} ({})\n",
                        err.text, err.error.message
                    ));
                }
            }
        }

        let mut out = String::new();
        fmt(self, 0, &mut out);
        out
    }
}

