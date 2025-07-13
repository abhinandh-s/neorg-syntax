#![allow(dead_code)]

use std::sync::Arc;

use crate::{Location, Position, SyntaxKind, Token, position, token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNode(Repr);

#[macro_export]
macro_rules! node {
    ($token:expr, $pos:expr) => {
        LeafNode::new(token, $pos) as SyntaxNode
    };
}

impl From<InnerNode> for SyntaxNode {
    fn from(value: InnerNode) -> Self {
        SyntaxNode(Repr::Inner(value.into()))
    }
}

impl From<LeafNode> for SyntaxNode {
    fn from(val: LeafNode) -> Self {
        SyntaxNode(Repr::Leaf(val))
    }
}

impl From<ErrorNode> for SyntaxNode {
    fn from(value: ErrorNode) -> Self {
        SyntaxNode(Repr::Error(value.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Repr {
    Leaf(LeafNode),
    Inner(Arc<InnerNode>),
    Error(Arc<ErrorNode>),
}

impl SyntaxNode {
    /// Create a new leaf node.
    pub fn leaf(tok: Token, pos: Location) -> Self {
        Self(Repr::Leaf(LeafNode::new(tok, pos)))
    }

    /// Create a new inner node with children.
    pub fn inner(kind: SyntaxKind, children: Vec<SyntaxNode>, pos: Location) -> Self {
        Self(Repr::Inner(Arc::new(InnerNode::new(kind, children, pos))))
    }

    /// Create a new error node.
    pub fn error(error: SyntaxError, text: impl Into<String>) -> Self {
        Self(Repr::Error(Arc::new(ErrorNode::new(error, text))))
    }

    /// Create a dummy node of the given kind.
    ///
    /// Panics if `kind` is `SyntaxKind::Error`.
    #[track_caller]
    pub fn placeholder(kind: SyntaxKind) -> Self {
        if matches!(kind, SyntaxKind::Error) {
            panic!("cannot create error placeholder");
        }
        Self(Repr::Leaf(LeafNode {
            loc: Location::detached(),
            token: token!(SyntaxKind::TombStone, "", 0),
        }))
    }

    /// The type of the node.
    pub fn kind(&self) -> SyntaxKind {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.kind(),
            Repr::Inner(inner) => inner.kind,
            Repr::Error(_) => SyntaxKind::Error,
        }
    }

    /// The span of the node.
    pub fn loc(&self) -> Location {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.loc,
            Repr::Inner(inner) => inner.loc,
            Repr::Error(node) => node.error.loc,
        }
    }

    /// The text of the node if it is a leaf or error node.
    ///
    /// Returns the empty string if this is an inner node.
    pub fn text(&self) -> &str {
        static EMPTY: String = String::new();
        match &self.0 {
            Repr::Leaf(leaf) => leaf.text(),
            Repr::Inner(_) => &EMPTY,
            Repr::Error(node) => &node.text,
        }
    }

    /// Extract the text from the node.
    ///
    /// Builds the string if this is an inner node.
    pub fn into_text(self) -> String {
        match self.0 {
            Repr::Leaf(leaf) => leaf.text().to_owned(),
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
        Self::leaf(token!(SyntaxKind::Eof, "", 0), Location::detached())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct LeafNode {
    /// The node's span.
    loc: Location,
    token: Token,
}

impl LeafNode {
    /// Create a new leaf node.
    #[track_caller]
    fn new(token: Token, pos: Location) -> Self {
        debug_assert!(!token.kind().is_error());
        Self { loc: pos, token }
    }

    fn kind(&self) -> SyntaxKind {
        self.token.kind()
    }

    fn len(&self) -> usize {
        self.token.len()
    }

    fn text(&self) -> &str {
        self.token.text()
    }

    fn is_empty(&self) -> bool {
        self.token.is_empty()
    }

    /// Whether the two leaf nodes are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.kind() == other.kind() && self.text() == other.text()
    }
}

impl std::fmt::Debug for LeafNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.kind(), self.text())
    }
}

/// An inner node in the untyped syntax tree.
#[derive(Clone, Eq, PartialEq, Hash)]
struct InnerNode {
    /// What kind of node this is (each kind would have its own struct in a
    /// strongly typed AST).
    kind: SyntaxKind,
    /// The node's span.
    loc: Location,
    /// Whether this node or any of its children are erroneous.
    erroneous: bool,
    /// This node's children, losslessly make up this node.
    children: Vec<SyntaxNode>,
}

impl InnerNode {
    /// Create a new inner node with the given kind and children.
    #[track_caller]
    fn new(kind: SyntaxKind, children: Vec<SyntaxNode>, loc: Location) -> Self {
        debug_assert!(!kind.is_error());

        let mut erroneous = false;

        for child in &children {
            erroneous |= child.erroneous();
        }

        Self {
            kind,
            loc,
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
    pos: Location,
}

impl ErrorNode {
    /// Create new error node.
    fn new(error: SyntaxError, text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            error,
            pos: Location::detached(),
        }
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
    pub loc: Location,
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
            loc: Location::default(),
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
                    output.push_str(&format!(
                        "{padding}{:?}: {:?} {}\n",
                        leaf.kind(),
                        leaf.text(),
                        leaf.loc
                    ));
                }
                Repr::Inner(inner) => {
                    output.push_str(&format!("{padding}{:?} {:?}\n", inner.kind, inner.loc));
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

// TODO: add is_empty() to this
trait LocationTrait {
    fn len(&self) -> usize;
    fn offset(&self) -> usize;
    fn span(&self) -> Position;
    #[cfg(feature = "tower_lsp")]
    fn position(&self) -> tower_lsp::lsp_types::Position;
    #[cfg(feature = "tower_lsp")]
    fn range(&self) -> tower_lsp::lsp_types::Range;
}

impl LocationTrait for LeafNode {
    /// The byte length of the node in the source text.
    fn len(&self) -> usize {
        self.token.len()
    }

    fn offset(&self) -> usize {
        self.loc.offsets.0
    }

    fn span(&self) -> Position {
        position!(self.loc.offsets.0, self.loc.offsets.0 + self.len())
    }

    #[cfg(feature = "tower_lsp")]
    fn position(&self) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position::new(
            self.loc.start.start() as u32,
            self.loc.end.end() as u32,
        )
    }

    #[cfg(feature = "tower_lsp")]
    fn range(&self) -> tower_lsp::lsp_types::Range {
        let line = self.loc.start.start() as u32;
        let character = (self.loc.end.end() + self.len()) as u32;
        tower_lsp::lsp_types::Range {
            start: self.position(),
            end: tower_lsp::lsp_types::Position { line, character },
        }
    }
}

impl LocationTrait for ErrorNode {
    fn offset(&self) -> usize {
        self.pos.offsets.0
    }

    fn span(&self) -> Position {
        position!(self.pos.offsets.0, self.pos.offsets.0 + self.len())
    }

    /// The char length of the node in the source text.
    fn len(&self) -> usize {
        self.text.len()
    }

    #[cfg(feature = "tower_lsp")]
    fn position(&self) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position::new(
            self.pos.start.start() as u32,
            self.pos.end.end() as u32,
        )
    }

    #[cfg(feature = "tower_lsp")]
    fn range(&self) -> tower_lsp::lsp_types::Range {
        let line = self.pos.start.start() as u32;
        let character = (self.pos.end.end() + self.len()) as u32;
        tower_lsp::lsp_types::Range {
            start: self.position(),
            end: tower_lsp::lsp_types::Position { line, character },
        }
    }
}

impl LocationTrait for InnerNode {
    fn offset(&self) -> usize {
        self.loc.offsets.0
    }

    fn span(&self) -> Position {
        let start = self.loc.offsets.0;
        let mut end = start;
        for i in &self.children {
            end += i.text().chars().count();
        }
        position!(start, end)
    }

    /// The char length of the node in the source text.
    fn len(&self) -> usize {
        let start = self.loc.offsets.0;
        let mut end = start;
        for i in &self.children {
            end += i.text().chars().count();
        }
        end
    }

    #[cfg(feature = "tower_lsp")]
    fn position(&self) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position::new(
            self.loc.start.start() as u32,
            self.loc.end.end() as u32,
        )
    }

    #[cfg(feature = "tower_lsp")]
    fn range(&self) -> tower_lsp::lsp_types::Range {
        let line = self.loc.start.start() as u32;
        let character = (self.loc.end.end() + self.len()) as u32;
        tower_lsp::lsp_types::Range {
            start: self.position(),
            end: tower_lsp::lsp_types::Position { line, character },
        }
    }
}
