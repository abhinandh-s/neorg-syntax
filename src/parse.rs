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
                    let this = &self.source[self.cursor];
                    this.offset()
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
/// /this is italics/  
/// TODO: / this is not italics/
/// TODO: /this is not italics /
/// TODO: / this is not italics /
/// ```
pub fn parse_emph(p: &mut Parser) {
    let m = p.start();
    p.assert(K!('/')); // consume '/'
    // this really wont work. we need parse inline or text
    let tok = p.source[p.cursor].clone().kind();
    if tok.is_inline_expr() {
        parse_inline(p);
    } else {
        parse_text_chunk(p);
    }

    p.eat_if(K!('/'));
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

pub fn parse_verbatnium_text_chunk(p: &mut Parser) {
    let m = p.start();
    p.expect(m, SyntaxKind::Word);
    while p.at(SyntaxKind::Word) || p.at(SyntaxKind::WhiteSpace) {
        p.eat();
    }
    p.wrap(m, SyntaxKind::TextChunk);
}

/// `TextChunk` cant have whitespace at beg and end
pub fn parse_text_chunk(p: &mut Parser) {
    let m = p.start();
    p.expect(m, SyntaxKind::Word);
    while p.at(SyntaxKind::Word) || p.at(SyntaxKind::WhiteSpace) {
        p.eat();
    }
    // if the parser ate any whitespace.
    // we push it to this vector and pop from the TextChunk children.
    // after parsing the text TextChunk
    // we push those collected whitespaces as LeafNode
    while let Some(SyntaxElement::Leaf(e)) = p.nodes.children.last() {
        if e.token.kind() == SyntaxKind::WhiteSpace {
                let error = ErrorNode {
                kind: SyntaxKind::Error,
                text: "unexpected WhiteSpace".to_string(),
                hint: "expected `text`. must not end with whitespace".to_string(),
                span: {
                    let this = &p.source[p.cursor - 1];
                    this.offset()
                }, // error
            };
            p.nodes.children.push(SyntaxElement::Error(Arc::new(error)));
        } else {
            break;
        }
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
            Some(SyntaxKind::Word) => parse_text_chunk(p),
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
        Some(SyntaxKind::Word | SyntaxKind::WhiteSpace | SyntaxKind::NewLine) => parse_paragraph(p),
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
    // no wrapping here
    while !p.is_eof() {
        parse_any(p);
    }
}

/// # Inline elements:
///
/// - `bold`:
/// - `italics`:
/// - `spoilers`:
/// - `strikethrogh`:
pub fn parse_inline(p: &mut Parser) {
    // match on current token in vec of tokens.
    match p.source.get(p.cursor).map(|t| t.kind()) {
        Some(SyntaxKind::Asterisk) => parse_heading(p),
        Some(SyntaxKind::Slash) => parse_emph(p),
        Some(kind) => {
            eprintln!("Unhandled kind: {:?}", kind);
        }
        None => {}
    }
}
pub fn parse_paragraph_segment(_p: &mut Parser) {}
pub fn parse_bold(_p: &mut Parser) {}
pub fn parse_strikethrogh(_p: &mut Parser) {}
