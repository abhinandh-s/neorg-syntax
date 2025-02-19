use std::sync::Arc;

use crate::*;

#[derive(Debug, Clone, Copy)]
pub struct Marker(pub usize);

pub struct Parser {
    pub source: Vec<Token>,
    pub cursor: usize,
    pub nodes: InnerNode, // top-level node
}

impl Parser {
    pub fn new(source: Vec<Token>) -> Self {
        Self {
            source,
            cursor: 0,
            nodes: InnerNode::new(SyntaxKind::Document),
        }
    }

    pub fn next(&self, kind: SyntaxKind) -> bool {
        self.source.get(self.cursor + 1).map(|t| t.kind()) == Some(kind)
    }

    pub fn at(&self, kind: SyntaxKind) -> bool {
        self.source.get(self.cursor).map(|t| t.kind()) == Some(kind)
    }

    pub fn is_eof(&self) -> bool {
        self.cursor >= self.source.len()
    }

    pub fn eat(&mut self) {
        let token = self.source[self.cursor].clone();
        self.nodes
            .children
            .push(SyntaxElement::Leaf(LeafNode { token }));
        self.cursor += 1;
    }

    pub fn eat_if(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.eat();
            true
        } else {
            false
        }
    }

    pub fn eat_if_next(&mut self, kind: SyntaxKind) -> bool {
        if self.next(kind) {
            self.eat();
            true
        } else {
            false
        }
    }
    pub fn assert(&mut self, kind: SyntaxKind) {
        assert!(self.at(kind), "Expected {:?}", kind);
        self.eat();
    }

    pub fn start(&self) -> Marker {
        Marker(self.nodes.children.len())
    }

    pub fn wrap(&mut self, m: Marker, kind: SyntaxKind) {
        let drained = self.nodes.children.drain(m.0..).collect();
        let node = InnerNode {
            kind,
            erroneous: false,
            children: drained,
        };
        self.nodes
            .children
            .push(SyntaxElement::Inner(Arc::new(node)));
    }

    pub fn expect(&mut self, open: Marker, kind: SyntaxKind) {
        let token = self.source[self.cursor].clone();
        dbg!(token.kind());
        if token.kind() == kind {
            // consumed closing delimiter
        } else {
            // Error recovery: insert error node
            let error = ErrorNode {
                kind: SyntaxKind::Error,
                text: format!("expected {}", kind),
                hint: "".to_string(),
                span: {
                    let this = &token;
                    this.offset()
                }, // error
            };
            self.nodes
                .children
                .insert(open.0, SyntaxElement::Error(Arc::new(error)));
        }
    }
    pub fn expect_closing_delimiter(&mut self, open: Marker, kind: SyntaxKind) {
        if self.eat_if(kind) {
            // consumed closing delimiter
        } else {
            // Error recovery: insert error node
            let error = ErrorNode {
                kind: SyntaxKind::Error,
                text: format!("expected {}", kind),
                hint: "".to_string(),
                span: {
                    let this = &self.source[self.cursor - 1];
                    this.offset() - 1
                }, // error
            };
            self.nodes
                .children
                .insert(open.0, SyntaxElement::Error(Arc::new(error)));
        }
    }
}
impl Parser {
    pub fn debug_tree(&self) {
        println!("{}", self.nodes.pretty_string());
    }
}


impl Parser {
    /// Checks if the current token at `cursor` is a valid *opening delimiter*.
    /// Rule: Must not be followed by whitespace (e.g., `/ text` is invalid).
    pub fn is_valid_open_delimiter(&self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        match self.source.get(self.cursor + 1) {
            Some(t) => !matches!(t.kind(), SyntaxKind::WhiteSpace),
            None => false,
        }
    }

    /// Checks if the current token at `cursor` is a valid *closing delimiter*.
    /// Rule: Must not be preceded by whitespace (e.g., `text /` is invalid).
    pub fn is_valid_close_delimiter(&self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        if self.cursor == 0 {
            return false;
        }
        match self.source.get(self.cursor - 1) {
            Some(t) => !matches!(t.kind(), SyntaxKind::WhiteSpace),
            None => false,
        }
    }
}


/// ```ignore
/// / this is not italics/
/// /this is not italics /
/// /this is italics/
/// / this is not italics /
/// ```
pub fn parse_emph(p: &mut Parser) {
    let m = p.start();
    let kind = K!('/');

    if !p.is_valid_open_delimiter(kind) {
        // Just treat it as a normal slash
        p.eat();
        return;
    }

    p.assert(kind); // consume '/'
    let inner_start = p.start();

    // Parse until we hit a valid closing '/'
    while !p.is_eof() {
        if p.at(kind) && p.is_valid_close_delimiter(kind) {
            break;
        }
        p.eat();
    }
    p.wrap(inner_start, SyntaxKind::TextChunk);

    let had_closing = p.eat_if(kind);
    if !had_closing {
        // Error recovery: maybe insert error
        p.expect_closing_delimiter(m, kind);
    }

    p.wrap(m, SyntaxKind::Emph);
}

pub fn parse_heading(p: &mut Parser) {
    let m = p.start();
    p.assert(K!('*'));
    p.eat();
    p.eat_if(SyntaxKind::WhiteSpace);
    parse_text_chunk(p);
    p.wrap(m, SyntaxKind::Heading);
}

pub fn parse_text_chunk(p: &mut Parser) {
    let m = p.start();
    p.expect(m, SyntaxKind::Text);
    // p.assert(SyntaxKind::Text);

    while p.at(SyntaxKind::Text) || p.at(SyntaxKind::WhiteSpace) {
        p.eat();
    }
    p.wrap(m, SyntaxKind::TextChunk);
}

fn parse_paragraph(p: &mut Parser) {
    let m = p.start();

    // Paragraph is a sequence of inline elements like text, emphasis, etc.
    while !p.is_eof() {
        match p.source.get(p.cursor).map(|t| t.kind()) {
            Some(SyntaxKind::NewLine) => {
                // look ahead: if next is also newline, end paragraph
                if let Some(next) = p.source.get(p.cursor + 1) {
                    if next.kind() == SyntaxKind::NewLine {
                        break;
                    }
                }
                p.eat(); // single newline gets eaten inside paragraph
            }
            Some(SyntaxKind::Text) => parse_text_chunk(p),
            Some(SyntaxKind::Slash) => parse_emph(p),
            Some(SyntaxKind::WhiteSpace) => p.eat(),
            Some(SyntaxKind::Asterisk) => break, // block-level: heading
            Some(_) => {
                // unknown inside paragraph
                p.eat();
            }
            None => break,
        }
    }

    p.wrap(m, SyntaxKind::Paragraph);
}

pub fn parse_any(p: &mut Parser) {
    match p.source.get(p.cursor).map(|t| t.kind()) {
        Some(SyntaxKind::Asterisk) => parse_heading(p),
        Some(SyntaxKind::Slash) => parse_emph(p),
        Some(SyntaxKind::Text | SyntaxKind::WhiteSpace | SyntaxKind::NewLine) => parse_paragraph(p),
        Some(kind) => {
            eprintln!("Unhandled kind: {:?}", kind);
            p.eat();
        }
        None => {}
    }
}

/// Document
///     Paragraph
///         ParagraphSegments
///             TextChunk
///             Other Inline elements
pub fn parse_doc(p: &mut Parser) {
    while !p.is_eof() {
        parse_any(p);
    }
}

pub fn parse_paragraph_segment(_p: &mut Parser) {}
pub fn parse_bold(_p: &mut Parser) {}
pub fn parse_strikethrogh(_p: &mut Parser) {}
