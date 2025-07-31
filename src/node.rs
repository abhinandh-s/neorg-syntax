use std::ops::Deref;
use std::sync::Arc;

use crate::{Location, Span, SyntaxKind, Token, token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNode(Repr);

#[macro_export]
macro_rules! node {
    ($token:expr, $pos:expr) => {
        LeafNode::new($token, $pos) as SyntaxNode
    };
}

impl From<InnerNode> for SyntaxNode {
    fn from(value: InnerNode) -> Self {
        SyntaxNode(Repr::Inner(Arc::new(value)))
    }
}

impl From<LeafNode> for SyntaxNode {
    fn from(val: LeafNode) -> Self {
        SyntaxNode(Repr::Leaf(val))
    }
}

impl From<ErrorNode> for SyntaxNode {
    fn from(value: ErrorNode) -> Self {
        SyntaxNode(Repr::Error(Arc::new(value)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Repr {
    Leaf(LeafNode),
    Inner(Arc<InnerNode>),
    Error(Arc<ErrorNode>),
}

pub fn get_errors(node: SyntaxNode) -> Vec<SyntaxNode> {
    let mut vec = Vec::new();
    get_error(node, &mut vec);
    vec
}

#[cfg(feature = "tower-lsp")]
pub fn get_diagnostics(node: SyntaxNode) -> Vec<tower_lsp::lsp_types::Diagnostic> {
    let mut vec = Vec::new();
    get_diagnostic(node, &mut vec);
    vec
}

#[cfg(feature = "tower-lsp")]
pub(crate) fn get_diagnostic(node: SyntaxNode, result: &mut Vec<tower_lsp::lsp_types::Diagnostic>) {
    match node.0 {
        Repr::Leaf(_) => {}
        Repr::Inner(inner_node) => {
            for i in &inner_node.children {
                get_diagnostic(i.clone(), result);
            }
        }
        Repr::Error(error_node) => {
            use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};

            let diagnostic = Diagnostic {
                range: error_node.range(),
                message: error_node.message().to_owned(),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("neorg-analyzer".to_owned()),
                ..Default::default()
            };
            if !error_node.error.hints.is_empty() {
                for msg in &error_node.error.hints {
                    let diagnostic = Diagnostic {
                        range: error_node.range(),
                        message: msg.to_owned(),
                        severity: Some(DiagnosticSeverity::HINT),
                        source: Some("neorg-analyzer".to_owned()),
                        ..Default::default()
                    };
                    result.push(diagnostic)
                }
            }
            result.push(diagnostic)
        }
    }
}

pub fn get_kinds(kind: SyntaxKind, node: SyntaxNode) -> Vec<SyntaxNode> {
    let mut vec = Vec::new();
    get_by_kind(kind, node, &mut vec);
    vec
}

fn get_by_kind(kind: SyntaxKind, node: SyntaxNode, result: &mut Vec<SyntaxNode>) {
    match node.0 {
        Repr::Leaf(leaf) => {
            if leaf.kind() == kind {
                result.push(leaf.into());
            }
        }
        Repr::Inner(inner_node) => {
            if inner_node.kind() == kind {
                let syn: SyntaxNode = inner_node.deref().to_owned().into();
                result.push(syn);
            }
            for i in &inner_node.children {
                get_by_kind(kind, i.clone(), result);
            }
        }
        Repr::Error(error_node) => {
            if kind == SyntaxKind::Error {
                let syn: SyntaxNode = error_node.deref().to_owned().into();
                result.push(syn)
            }
        }
    }
}

fn get_error(node: SyntaxNode, result: &mut Vec<SyntaxNode>) {
    match node.0 {
        Repr::Leaf(_) => {}
        Repr::Inner(inner_node) => {
            for i in &inner_node.children {
                get_error(i.clone(), result);
            }
        }
        Repr::Error(error_node) => {
            let syn: SyntaxNode = error_node.deref().to_owned().into();
            result.push(syn)
        }
    }
}

#[allow(dead_code)]
impl SyntaxNode {
    /// Create a new leaf node.
    pub(crate) fn leaf(tok: Token, pos: Location) -> Self {
        LeafNode::new(tok, pos).into()
    }

    /// Create a new inner node with children.
    pub(crate) fn inner(kind: SyntaxKind, children: Vec<SyntaxNode>, pos: Location) -> Self {
        InnerNode::new(kind, children, pos).into()
    }

    /// Create a new error node.
    pub fn error(error: SyntaxError, text: impl Into<String>) -> Self {
        ErrorNode::new(error, text).into()
    }

    /// Create a dummy node of the given kind.
    ///
    /// Panics if `kind` is `SyntaxKind::Error`.
    #[track_caller]
    pub fn placeholder(kind: SyntaxKind) -> impl Into<Self> {
        if matches!(kind, SyntaxKind::Error) {
            panic!("cannot create error placeholder");
        }
        LeafNode::new(token!(SyntaxKind::TombStone, "", 0), Location::detached())
    }

    /// The type of the node.
    pub fn kind(&self) -> SyntaxKind {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.kind(),
            Repr::Inner(inner) => inner.kind(),
            Repr::Error(_) => SyntaxKind::Error,
        }
    }

    /// The span of the node.
    pub(super) fn loc(&self) -> Location {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.loc,
            Repr::Inner(inner) => inner.loc,
            Repr::Error(node) => node.error.loc,
        }
    }
    /// The span of the node.
    pub fn offset(&self) -> usize {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.offset(),
            Repr::Inner(inner) => inner.offset(),
            Repr::Error(node) => node.offset(),
        }
    }
    #[cfg(feature = "tower-lsp")]
    pub fn range(&self) -> tower_lsp::lsp_types::Range {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.range(),
            Repr::Inner(inner) => inner.range(),
            Repr::Error(node) => node.range(),
        }
    }

    #[cfg(feature = "tower-lsp")]
    pub fn start_position(&self) -> tower_lsp::lsp_types::Position {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.start_position(),
            Repr::Inner(inner) => inner.start_position(),
            Repr::Error(node) => node.start_position(),
        }
    }
    #[cfg(feature = "tower-lsp")]
    pub fn end_position(&self) -> tower_lsp::lsp_types::Position {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.end_position(),
            Repr::Inner(inner) => inner.end_position(),
            Repr::Error(node) => node.end_position(),
        }
    }
    /// The span of the node.
    pub fn len_utf16(&self) -> usize {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.len_utf16(),
            Repr::Inner(inner) => inner.len_utf16(),
            Repr::Error(node) => node.len_utf16(),
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
            let loc = self.loc();
            let text = std::mem::take(self).into_text();
            *self = SyntaxNode::error(SyntaxError::new(message, loc), text);
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

impl LocationTrait for LeafNode {
    /// utf16 len of the text in token
    fn len_utf16(&self) -> usize {
        self.token.len()
    }

    fn offset(&self) -> usize {
        self.loc.offsets()
    }

    #[cfg(feature = "tower-lsp")]
    fn start_position(&self) -> tower_lsp::lsp_types::Position {
        let line = self.line();
        let character = self.col();
        tower_lsp::lsp_types::Position::new(line, character)
    }

    #[cfg(feature = "tower-lsp")]
    fn end_position(&self) -> tower_lsp::lsp_types::Position {
        let line = self.line();
        let character = self.col();
        tower_lsp::lsp_types::Position::new(line, character + (self.len_utf16() as u32))
    }
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

    fn text(&self) -> &str {
        self.token.text()
    }

    fn line(&self) -> u32 {
        self.loc.line()
    }

    fn col(&self) -> u32 {
        self.loc.character()
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

#[allow(dead_code)]
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
    pub fn line(&self) -> u32 {
        self.children.first().map_or(0, |f| f.loc().line())
    }

    pub fn col(&self) -> u32 {
        self.children.first().map_or(0, |f| f.loc().character())
    }

    fn kind(&self) -> SyntaxKind {
        self.kind
    }
}

impl LocationTrait for InnerNode {
    /// utf16 len of the text in token
    fn len_utf16(&self) -> usize {
        let start = self.children.first().map_or(0, |f| f.offset()) as u32;
        let end = self.children.last().map_or(0, |f| f.offset()) as u32;
        end.saturating_sub(start) as usize
    }

    fn offset(&self) -> usize {
        self.loc.offsets()
    }

    #[cfg(feature = "tower-lsp")]
    fn start_position(&self) -> tower_lsp::lsp_types::Position {
        let line = self.line();
        let character = self.col();
        tower_lsp::lsp_types::Position::new(line, character)
    }

    #[cfg(feature = "tower-lsp")]
    fn end_position(&self) -> tower_lsp::lsp_types::Position {
        let line = self.children.last().map_or(0, |f| f.loc().line());
        let col = self.children.last().map_or(0, |f| f.loc().character());
        let character = col;
        tower_lsp::lsp_types::Position::new(line, character)
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

    /// Add a user-presentable hint to this error node.
    fn hint(&mut self, hint: impl Into<String>) {
        self.error.hints.push(hint.into());
    }

    pub fn line(&self) -> u32 {
        self.error.loc.line()
    }

    pub fn character(&self) -> u32 {
        self.error.loc.character()
    }

    pub fn message(&self) -> &str {
        self.error.message()
    }
}

impl LocationTrait for ErrorNode {
    /// utf16 len of the text in token
    fn len_utf16(&self) -> usize {
        let mut len = 0;
        self.text.chars().for_each(|f| len += f.len_utf16());
        len
    }

    fn offset(&self) -> usize {
        self.error.loc.offsets()
    }

    #[cfg(feature = "tower-lsp")]
    fn start_position(&self) -> tower_lsp::lsp_types::Position {
        let line = self.error.loc.line();
        let character = self.error.loc.character();
        tower_lsp::lsp_types::Position::new(line, character)
    }

    #[cfg(feature = "tower-lsp")]
    fn end_position(&self) -> tower_lsp::lsp_types::Position {
        let line = self.line();
        let character = self.character();
        tower_lsp::lsp_types::Position::new(line, character + (self.len_utf16() as u32))
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
    pub(crate) loc: Location,
    /// The error message.
    pub(crate) message: String,
    /// Additional hints to the user, indicating how this error could be avoided
    /// or worked around.
    pub(crate) hints: Vec<String>,
}

#[allow(dead_code)]
impl SyntaxError {
    /// Create a new detached syntax error.
    pub(crate) fn new(message: impl Into<String>, loc: Location) -> Self {
        Self {
            loc,
            message: message.into(),
            hints: vec![],
        }
    }

    pub(crate) fn message(&self) -> &str {
        &self.message
    }

    pub(crate) fn hints(&self) -> &[String] {
        &self.hints
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
                    output.push_str(&format!("{padding}{:?} {}\n", inner.kind, inner.loc));
                    for child in &inner.children {
                        fmt(child, indent + 1, output);
                    }
                }
                Repr::Error(err) => {
                    output.push_str(&format!(
                        "{padding}Error: {:?} ({}) {}\n",
                        err.text, err.error.message, err.error.loc
                    ));
                }
            }
        }

        let mut out = String::new();
        fmt(self, 0, &mut out);
        out
    }
}

// req methods
//
// - len
// - offset
// - start_position
// - end_position
//
// # auto impl
//
// - span
// - range
pub trait LocationTrait {
    fn len_utf16(&self) -> usize;

    fn offset(&self) -> usize;

    fn span(&self) -> Span {
        Span {
            start: self.offset(),
            end: self.offset() + self.len_utf16(),
        }
    }

    #[cfg(feature = "tower-lsp")]
    fn start_position(&self) -> tower_lsp::lsp_types::Position;

    #[cfg(feature = "tower-lsp")]
    fn end_position(&self) -> tower_lsp::lsp_types::Position;

    #[cfg(feature = "tower-lsp")]
    fn range(&self) -> tower_lsp::lsp_types::Range {
        tower_lsp::lsp_types::Range {
            start: self.start_position(),
            end: self.end_position(),
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    #[cfg(feature = "tower-lsp")]
    fn range() {
        use tower_lsp::lsp_types::Position;

        use crate::{Parser, grammer::document};

        let source = "* this is a heading\nThis";
        let mut p = Parser::new(source);
        let res = document(&mut p);
        assert_eq!(0, res.loc().line());
        assert_eq!(1, res.loc().character());
        assert_eq!(0, res.loc().offsets());
        assert_eq!(Position::new(0, 1), res.start_position());
        assert_eq!(Position::new(1, 4), res.end_position());
        assert_eq!(
            tower_lsp::lsp_types::Range::new(
                tower_lsp::lsp_types::Position::new(0, 1),
                tower_lsp::lsp_types::Position::new(1, 4)
            ),
            res.range()
        )
    }
}
