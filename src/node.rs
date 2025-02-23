use std::fmt::{self, Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use ecow::{EcoString, EcoVec, eco_vec};

use crate::kind::SyntaxKind;
use crate::span::Span;

/// A node in the untyped syntax tree.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct SyntaxNode(Repr);

/// The three internal representations.
#[derive(Clone, Eq, PartialEq, Hash)]
enum Repr {
    /// A leaf node.
    Leaf(LeafNode),
    /// A reference-counted inner node.
    Inner(Arc<InnerNode>),
    /// An error node.
    Error(Arc<ErrorNode>),
}

impl SyntaxNode {
    /// Create a new leaf node.
    pub fn leaf(kind: SyntaxKind, text: impl Into<EcoString>, span: Span) -> Self {
        Self(Repr::Leaf(LeafNode::new(kind, text, span)))
    }

    /// Create a new inner node with children.
    pub fn inner(kind: SyntaxKind, children: Vec<SyntaxNode>, span: Span) -> Self {
        Self(Repr::Inner(Arc::new(InnerNode::new(kind, children, span))))
    }

    /// Create a new error node.
    pub fn error(error: SyntaxError, text: impl Into<EcoString>) -> Self {
        Self(Repr::Error(Arc::new(ErrorNode::new(error, text))))
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
    pub fn text(&self) -> &EcoString {
        static EMPTY: EcoString = EcoString::new();
        match &self.0 {
            Repr::Leaf(leaf) => &leaf.text,
            Repr::Inner(_) => &EMPTY,
            Repr::Error(node) => &node.text,
        }
    }

    /// Extract the text from the node.
    ///
    /// Builds the string if this is an inner node.
    pub fn into_text(self) -> EcoString {
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

    /// Add a user-presentable hint if this is an error node.
    pub fn hint(&mut self, hint: impl Into<EcoString>) {
        if let Repr::Error(node) = &mut self.0 {
            Arc::make_mut(node).hint(hint);
        }
    }

    /// Whether the two syntax nodes are the same apart from spans.
    pub fn spanless_eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (Repr::Leaf(a), Repr::Leaf(b)) => a.spanless_eq(b),
            (Repr::Inner(a), Repr::Inner(b)) => a.spanless_eq(b),
            (Repr::Error(a), Repr::Error(b)) => a.spanless_eq(b),
            _ => false,
        }
    }
}

impl SyntaxNode {}

impl Debug for SyntaxNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.fmt(f),
            Repr::Inner(inner) => inner.fmt(f),
            Repr::Error(node) => node.fmt(f),
        }
    }
}

impl Default for SyntaxNode {
    fn default() -> Self {
        Self::leaf(SyntaxKind::Eof, EcoString::new(), Span::default())
    }
}

/// A leaf node in the untyped syntax tree.
#[derive(Clone, Eq, PartialEq, Hash)]
struct LeafNode {
    /// What kind of node this is (each kind would have its own struct in a
    /// strongly typed AST).
    kind: SyntaxKind,
    /// The source text of the node.
    text: EcoString,
    /// The node's span.
    span: Span,
}

impl LeafNode {
    /// Create a new leaf node.
    #[track_caller]
    fn new(kind: SyntaxKind, text: impl Into<EcoString>, span: Span) -> Self {
        Self {
            kind,
            text: text.into(),
            span,
        }
    }
    /// Whether the two leaf nodes are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.text == other.text
    }
}

impl Debug for LeafNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
    /// This node's children, losslessly make up this node.
    children: Vec<SyntaxNode>,
}

impl InnerNode {
    /// Create a new inner node with the given kind and children.
    #[track_caller]
    fn new(kind: SyntaxKind, children: Vec<SyntaxNode>, span: Span) -> Self {
        Self {
            kind,
            span,
            children,
        }
    }

    /// Whether the two inner nodes are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.kind == other.kind
            && self.children.len() == other.children.len()
            && self
                .children
                .iter()
                .zip(&other.children)
                .all(|(a, b)| a.spanless_eq(b))
    }
}

impl Debug for InnerNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
    text: EcoString,
    /// The syntax error.
    error: SyntaxError,
}

impl ErrorNode {
    /// Create new error node.
    fn new(error: SyntaxError, text: impl Into<EcoString>) -> Self {
        Self {
            text: text.into(),
            error,
        }
    }

    /// Add a user-presentable hint to this error node.
    fn hint(&mut self, hint: impl Into<EcoString>) {
        self.error.hints.push(hint.into());
    }

    /// Whether the two leaf nodes are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.text == other.text && self.error.spanless_eq(&other.error)
    }
}

impl Debug for ErrorNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Error: {:?} ({})", self.text, self.error.message)
    }
}

/// A syntactical error.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SyntaxError {
    /// The node's span.
    pub span: Span,
    /// The error message.
    pub message: EcoString,
    /// Additional hints to the user, indicating how this error could be avoided
    /// or worked around.
    pub hints: EcoVec<EcoString>,
}

impl SyntaxError {
    /// Create a new detached syntax error.
    pub fn new(message: impl Into<EcoString>) -> Self {
        Self {
            span: Span::default(),
            message: message.into(),
            hints: eco_vec![],
        }
    }

    /// Whether the two errors are the same apart from spans.
    fn spanless_eq(&self, other: &Self) -> bool {
        self.message == other.message && self.hints == other.hints
    }
}

/// A syntax node in a context.
///
/// Knows its exact offset in the file and provides access to its
/// children, parent and siblings.
///
/// **Note that all sibling and leaf accessors skip over trivia!**
#[derive(Clone)]
pub struct LinkedNode<'a> {
    node: &'a SyntaxNode,
    parent: Option<Rc<Self>>,
    index: usize,
    offset: usize,
}

impl<'a> LinkedNode<'a> {
    /// Start a new traversal at a root node.
    pub fn new(root: &'a SyntaxNode) -> Self {
        Self {
            node: root,
            parent: None,
            index: 0,
            offset: 0,
        }
    }

    /// Get the contained syntax node.
    pub fn get(&self) -> &'a SyntaxNode {
        self.node
    }

    /// The index of this node in its parent's children list.
    pub fn index(&self) -> usize {
        self.index
    }

    /// The absolute byte offset of this node in the source file.
    pub fn offset(&self) -> usize {
        self.offset
    }
}

/// Access to parents and siblings.
impl LinkedNode<'_> {
    /// Get this node's parent.
    pub fn parent(&self) -> Option<&Self> {
        self.parent.as_deref()
    }

    /// Get the kind of this node's parent.
    pub fn parent_kind(&self) -> Option<SyntaxKind> {
        Some(self.parent()?.node.kind())
    }
}

/// Indicates whether the cursor is before the related byte index, or after.
#[derive(Debug, Clone)]
pub enum Side {
    Before,
    After,
}

impl Deref for LinkedNode<'_> {
    type Target = SyntaxNode;

    /// Dereference to a syntax node. Note that this shortens the lifetime, so
    /// you may need to use [`get()`](Self::get) instead in some situations.
    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl Debug for LinkedNode<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.node.fmt(f)
    }
}
