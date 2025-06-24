// -- TODO: Any line may be preceded by a variable amount of whitespace, should be ignored.

use std::sync::Arc;

use crate::*;

#[derive(Debug, Clone, Copy)]
pub struct Marker(pub usize);

pub struct Parser {
    source: Vec<Token>,
    cursor: usize,
    /// Whether the parser has the expected set of open/close delimiters. This
    /// only ever transitions from `true` to `false`.
    balanced: bool,
    nodes_two: Vec<SyntaxNode>,
    nodes: InnerNode, // top-level node
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let mut lexer = Lexer::new(source);
        Self {
            source: lexer.lex().to_vec(),
            cursor: 0,
            balanced: false,
            nodes_two: Vec::new(),
            nodes: InnerNode::new(SyntaxKind::Document),
        }
    }

    pub fn current(&self) -> SyntaxKind {
        self.source[self.cursor].kind()
    }
    pub fn next(&self, kind: SyntaxKind) -> bool {
        self.source.get(self.cursor + 1).map(|t| t.kind()) == Some(kind)
    }

    pub fn at(&self, kind: SyntaxKind) -> bool {
        self.source.get(self.cursor).map(|t| t.kind()) == Some(kind)
    }

    /// Whether the current token is contained in a [`SyntaxSet`].
    pub fn at_set(&self, set: SyntaxSet) -> bool {
        set.contains(self.current())
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

    pub fn eat_while(&mut self, pred: impl Fn(SyntaxKind) -> bool) {
        while let Some(t) = self.source.get(self.cursor) {
            if pred(t.kind()) {
                self.eat();
            } else {
                break;
            }
        }
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

    #[track_caller]
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
        tracing::debug!(?m, ?kind, "wrap node");
        self.nodes
            .children
            .push(SyntaxElement::Inner(Arc::new(node)));
    }

    pub fn expect(&mut self, open: Marker, kind: SyntaxKind) {
        let token = self.source[self.cursor].clone();
        if token.kind() == kind {
            // consumed closing delimiter
            self.eat();
        } else {
            // Error recovery: insert error node
            let error = ErrorNode {
                kind: SyntaxKind::Error,
                text: format!("Syntax Error: expected `{}` found `{}`", kind, token.kind()),
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
    pub fn expect_closing_delimiter(&mut self, kind: SyntaxKind) {
        let token = self.source[self.cursor].clone();
        if token.kind() == kind {
            // consumed closing delimiter
            self.eat();
        } else {
            // Error recovery: insert error node
            let error = ErrorNode {
                kind: SyntaxKind::Error,
                text: format!("Syntax Error: unclosed delimiter `{}`", kind.text()),
                hint: "".to_string(),
                span: {
                    let this = &token;
                    this.offset()
                }, // error
            };
            self.nodes
                .children
                .push(SyntaxElement::Error(Arc::new(error)));
        }
    }

    pub fn balanced(&self) -> bool {
        self.balanced
    }

    pub fn set_balanced(&mut self, balanced: bool) {
        self.balanced = balanced;
    }
}
impl Parser {
    pub fn debug_tree(&self) {
        println!("{}", self.nodes.pretty_string());
    }
}

/// # cases
///
/// all these cases are covered.
///
/// ```ignore
/// /this is italics/  
/// / this is not italics/
/// /this is not italics /
/// / this is not italics /
/// ```
#[tracing::instrument(skip_all)]
pub fn parse_italics(p: &mut Parser) {
    let m = p.start();
    p.assert(K!('/')); // assert & consume '/'
    let tok = p.source[p.cursor].clone().kind();
    if tok.is_inline_expr() {
        parse_inline(p);
    } else {
        parse_text_chunk(p);
    }

    p.expect_closing_delimiter(K!('/'));
    p.wrap(m, SyntaxKind::Emph);
}

#[tracing::instrument(skip_all)]
pub fn parse_pipe(p: &mut Parser) {
    let m = p.start();
    p.assert(K!('|')); // assert & consume '/'
    parse_verbatim_text_chunk(p);

    p.expect_closing_delimiter(K!('|'));
    p.wrap(m, SyntaxKind::Emph);
}

/// # Parameters
///
/// - `function name`:
/// - `delimiter kind`:
/// - `wrapper kind`:
///
/// # Usage
///
/// ```ignore
/// parse_inline!(parse_spoiler, K!('!'), SyntaxKind::Spoiler);
/// parse_inline!(parse_null_modifiler, K!('%'), SyntaxKind::Comment);
/// parse_inline!(parse_bold, K!('*'), SyntaxKind::Bold);
/// ```
/// This is an exact replica of `parse_italics` function
///
/// Don't forget to add `SyntaxKind` to `is_inline_expr' method of `SyntaxKind`
#[macro_export]
macro_rules! parse_inline {
    ($fn_name:ident, $kind:expr, $wrapper:expr) => {
        #[tracing::instrument(skip_all)]
        pub fn $fn_name(p: &mut Parser) {
            let m = p.start();
            p.assert($kind); // assert & consume current token
            let tok = p.source[p.cursor].clone().kind();
            if tok.is_inline_expr() {
                parse_inline(p);
            } else {
                parse_text_chunk(p);
            }
            p.expect_closing_delimiter($kind);
            p.wrap(m, $wrapper);
        }
    };
}

parse_inline!(parse_spoiler, K!('!'), SyntaxKind::Spoiler);
parse_inline!(parse_null_modifiler, K!('%'), SyntaxKind::Comment);
parse_inline!(parse_bold, K!('*'), SyntaxKind::Bold);
// -- TODO: make it verbatim
parse_inline!(parse_maths, K!('$'), SyntaxKind::Maths);
// -- TODO: check wheter this is single word or not.
parse_inline!(parse_subscript, K!('^'), SyntaxKind::Subscript);
parse_inline!(parse_superscript, K!(','), SyntaxKind::Superscript);
parse_inline!(parse_strike_through, K!('-'), SyntaxKind::StrikeThrough);

#[tracing::instrument(skip_all)]
pub fn parse_heading(p: &mut Parser) {
    let m = p.start();
    p.assert(K!('*'));
    p.eat_while(|k| matches!(k, SyntaxKind::Asterisk));
    p.eat_if(SyntaxKind::WhiteSpace);
    parse_text_chunk(p);
    p.wrap(m, SyntaxKind::Heading);
}

#[tracing::instrument(skip_all)]
pub fn parse_paragraph_segment(p: &mut Parser) {
    let m = p.start();
    while !p.at(SyntaxKind::LineEnding) {
        p.eat();
    }
    p.wrap(m, SyntaxKind::ParaSegment);
}

#[tracing::instrument(skip_all)]
pub fn parse_verbatim_text_chunk(p: &mut Parser) {
    let m = p.start();
    while p.at(SyntaxKind::Pipe) || p.at(SyntaxKind::Eof) {
        p.eat();
    }
    p.wrap(m, SyntaxKind::TextChunk);
}

/// `TextChunk` cant have whitespace at beg and end
#[tracing::instrument(skip_all)]
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
            p.nodes
                .children
                .push(SyntaxElement::Error(Arc::new(error)));
        } else {
            break;
        }
    }
    p.wrap(m, SyntaxKind::TextChunk);
}

#[tracing::instrument(skip_all)]
fn parse_paragraph(p: &mut Parser) {
    let m = p.start();

    // Paragraph is a sequence of inline elements like text, emphasis, etc.
    while !p.is_eof() {
        match p.source.get(p.cursor).map(|t| t.kind()) {
            Some(SyntaxKind::LineEnding) => {
                // look ahead: if next is also newline, end paragraph
                if let Some(next) = p.source.get(p.cursor + 1) {
                    if next.kind() == SyntaxKind::LineEnding {
                        break;
                    }
                }
                p.eat(); // single newline gets eaten inside paragraph
            }
            Some(SyntaxKind::Word) => parse_text_chunk(p),
            Some(SyntaxKind::Slash) => parse_italics(p),
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

#[tracing::instrument(skip_all)]
pub fn parse_any(p: &mut Parser) {
    match p.source.get(p.cursor).map(|t| t.kind()) {
        Some(SyntaxKind::Percent) => parse_null_modifiler(p),
        Some(SyntaxKind::Exclamation) => parse_spoiler(p),
        Some(SyntaxKind::Asterisk) => parse_bold(p),
        Some(SyntaxKind::Slash) => parse_italics(p),
        Some(SyntaxKind::Dollar) => parse_maths(p),
        Some(SyntaxKind::Subscript) => parse_subscript(p),
        Some(SyntaxKind::Superscript) => parse_superscript(p),
        Some(SyntaxKind::Hyphen) => parse_strike_through(p),
        Some(SyntaxKind::Word | SyntaxKind::WhiteSpace) => parse_paragraph(p),
        // -- FIX: we should not get LineEnding
        Some(SyntaxKind::LineEnding) => parse_paragraph_segment(p),
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
#[tracing::instrument(skip_all)]
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
#[tracing::instrument(skip_all)]
pub(crate) fn parse_inline(p: &mut Parser) {
    // match on current token in vec of tokens.
    match p.source.get(p.cursor).map(|t| t.kind()) {
        Some(SyntaxKind::Percent) => parse_null_modifiler(p),
        Some(SyntaxKind::Exclamation) => parse_spoiler(p),
        Some(SyntaxKind::Asterisk) => parse_bold(p),
        Some(SyntaxKind::Slash) => parse_italics(p),
        Some(kind) => {
            eprintln!("Unhandled kind: {:?}", kind);
        }
        None => {}
    }
}

#[macro_export]
macro_rules! assert_tree {
    ($input:expr, $expect:literal) => {{
        let mut p = Parser::new($input);
        parse_doc(&mut p);
        let actual = p.nodes.pretty_string();
        expect_test::expect![$expect].assert_eq(&actual);
    }};
}

#[macro_export]
macro_rules! insta_tree {
    ($fn_name:ident, $text:literal) => {
        #[test]
        fn $fn_name() {
            let key = "INSTA_UPDATE";
            unsafe {
                std::env::set_var(key, "no");
            }
            let mut p = Parser::new($text);
            parse_doc(&mut p);
            let actual = p.nodes.pretty_string();
            insta::assert_snapshot!(actual)
        }
    };
}

insta_tree!(empty_file, "");
insta_tree!(italics_001, "/this is italics with no errors/");

#[test]
fn basic_emph() {
    assert_tree!(
        "/this/",
        r#"
DOCUMENT
│   EMPH
│   │   SLASH@0 "/"
│   │   TEXT_CHUNK
│   │   │   WORD@1 "this"
│   │   SLASH@5 "/"
│   EOF@6 "\0"
"#
    );
}
#[test]
fn basic_heading() {
    assert_tree!(
        "* this is a heading",
        r#"
DOCUMENT
│   BOLD
│   │   ASTERISK@0 "*"
│   │   TEXT_CHUNK
│   │   │   ERROR@1 > Syntax Error: expected `WORD` found `WHITE_SPACE` > 
│   │   │   WHITE_SPACE@1 " "
│   │   │   WORD@2 "this"
│   │   │   WHITE_SPACE@6 " "
│   │   │   WORD@7 "is"
│   │   │   WHITE_SPACE@9 " "
│   │   │   WORD@10 "a"
│   │   │   WHITE_SPACE@11 " "
│   │   │   WORD@12 "heading"
│   │   ERROR@19 > Syntax Error: unclosed delimiter `*` > 
│   EOF@19 "\0"
"#
    );
}
#[test]
fn basic_nesting() {
    assert_tree!(
        "!%/*this is a nested inline text*/%!",
        r#"
DOCUMENT
│   SPOILER
│   │   EXCLAMATION@0 "!"
│   │   COMMENT
│   │   │   PERCENT@1 "%"
│   │   │   EMPH
│   │   │   │   SLASH@2 "/"
│   │   │   │   BOLD
│   │   │   │   │   ASTERISK@3 "*"
│   │   │   │   │   TEXT_CHUNK
│   │   │   │   │   │   WORD@4 "this"
│   │   │   │   │   │   WHITE_SPACE@8 " "
│   │   │   │   │   │   WORD@9 "is"
│   │   │   │   │   │   WHITE_SPACE@11 " "
│   │   │   │   │   │   WORD@12 "a"
│   │   │   │   │   │   WHITE_SPACE@13 " "
│   │   │   │   │   │   WORD@14 "nested"
│   │   │   │   │   │   WHITE_SPACE@20 " "
│   │   │   │   │   │   WORD@21 "inline"
│   │   │   │   │   │   WHITE_SPACE@27 " "
│   │   │   │   │   │   WORD@28 "text"
│   │   │   │   │   ASTERISK@32 "*"
│   │   │   │   SLASH@33 "/"
│   │   │   PERCENT@34 "%"
│   │   EXCLAMATION@35 "!"
│   EOF@36 "\0"
"#
    );
}
