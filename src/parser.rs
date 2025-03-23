#![allow(dead_code)]

use ecow::EcoString;

use crate::kind::SyntaxKind;
use crate::lexer::Token;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parser {
    start: usize,
    current: usize,
    tokens: Vec<Token>,
    nodes: Vec<SyntaxNode>,
}

impl Parser {
    pub fn parse(&mut self) -> Vec<SyntaxNode> {
        // Use a while let loop instead of checking self.current directly
        while let Some(token) = self.peek() {
            if token.kind == SyntaxKind::Eof {
                break;
            }
            let node = self.scan();
            self.nodes.push(node);
        }
        std::mem::take(&mut self.nodes)
    }

    fn scan(&mut self) -> SyntaxNode {
        match self.eat().unwrap().kind {
            SyntaxKind::Slash => self.italics(),
            _ => self.italics(),
        }
    }
    // " / this is not italics text /"
    fn italics(&mut self) -> SyntaxNode {
        let cur = self.peek().unwrap();
        let _text = String::new();

        self.expect(T::Slash).then(|| {
            self.eat()
                .map(|tok| {
                    println!("{}", tok);
                })
                .unwrap();
            // let s = self.eat_many(SyntaxKind::Text).unwrap();
        });

        println!("peaked: {}", cur);
        println!("eated: {}", self.eat().unwrap());
        println!("peaked: {}", self.peek().unwrap());
        SyntaxNode {
            kind: SyntaxKind::Text,
            text: "this".into(),
            span: Span::default(),
            errorneous: false,
        }
    }
}

type T = SyntaxKind;

trait BoolExt {
    fn else_try<F, T>(self, f: F) -> Option<T>
    where
        F: FnOnce() -> T;
}

impl BoolExt for bool {
    fn else_try<F, T>(self, f: F) -> Option<T>
    where
        F: FnOnce() -> T,
    {
        if !self { Some(f()) } else { None }
    }
}

impl Parser {
    // returns true if current Token kind and given SyntaxKind matches
    fn expect(&self, kind: SyntaxKind) -> bool {
        self.peek().unwrap().kind == kind
    }

    // returns true if current index matches `Vec<Token>` len
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    // peek at the current `Token` without consuming it.
    #[inline]
    fn peek(&self) -> Option<Token> {
        if self.is_at_end() {
            None
        } else {
            self.tokens[self.current..].first().cloned()
        }
    }

    // eat current Token and gives the next `Token`. It also updates current index
    #[inline]
    fn eat(&mut self) -> Option<Token> {
        if self.is_at_end() {
            None
        } else {
            self.current += 1;
            let s = self.current <= self.tokens.len();
            assert!(s);
            Some(self.tokens[self.current].clone())
        }
    }

    // eats sub seqeunt `Tokens` which matches the pattern `SyntaxKind` and gives the next `Token`.
    #[inline]
    // -- TEST: rewrite this
    fn eat_while(&mut self, con: bool) -> Option<Token> {
        while self.peek().is_some() {
            if !con {
                break;
            }
            self.current += 1;
        }
        self.eat()
    }
    // eats sub seqeunt `Tokens` which matches the pattern `SyntaxKind` and gives the next `Token`.
    #[inline]
    fn eat_many(&mut self, pat: SyntaxKind) -> Option<Token> {
        while let Some(tok) = self.peek() {
            if tok.kind != pat {
                break;
            }
            self.current += 1;
        }
        self.eat()
    }
}

impl Iterator for Parser {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.is_at_end() {
            None
        } else {
            // .cloned() cuz the type is &Token but we need to return owned Token
            // can we eleminate it
            self.tokens[self.current..].iter().next().cloned()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNode {
    kind: SyntaxKind,
    text: EcoString,
    span: Span,
    errorneous: bool,
}

impl SyntaxNode {
    fn new(kind: SyntaxKind, text: EcoString, span: Span) -> Self {
        Self {
            kind,
            text,
            span,
            errorneous: false,
        }
    }
}

impl std::fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NODE:=> {}: {:?} {}", self.kind, self.text, self.span)
    }
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            current: 1,
            tokens,
            nodes: Vec::new(),
            start: 0,
        }
    }
}
//    pub fn scan(&mut self) -> Repr {
//        self.start = self.current;
//
//        let text = EcoString::new();
//
//        match self.advance() {
//            Some(i) => {
//                let start = i.span.start;
//                let mut parse_delimeted_expr =
//                |pat: SyntaxKind,
//                kind: SyntaxKind,
//                mut text: EcoString,
//                error: Option<String>,
//                hint: Option<String>| {
//                    if let Some(next_token) = self.peek() {
//                        if next_token.kind == SyntaxKind::Text {
//                            text.push_str(&next_token.text);
//                            self.advance();
//                            if next_token.text.ends_with(' ') {
//                                // if the text ends with WhiteSpace its not vaild
//                                if let Some(closing_token) = self.peek() {
//                                    if closing_token.kind == pat {
//                                        self.advance();
//                                        return Repr::ErrorNode(ErrorNode {
//                                            kind: SyntaxKind::Error,
//                                            text,
//                                            error: Some("Trailing WhiteSpace".to_owned()),
//                                            hint,
//                                            span: Span::new(start, i.span.end),
//                                        });
//                                    }
//                                }
//                            }
//                            if let Some(closing_token) = self.peek() {
//                                if closing_token.kind == pat {
//                                    self.advance();
//                                    return Repr::SyntaxNode(SyntaxNode::new(
//                                        kind,
//                                        text.clone(),
//                                        Span::new(start, closing_token.span.end),
//                                    ));
//                                }
//                            }
//                        }
//                    }
//
//                    Repr::ErrorNode(ErrorNode {
//                        kind: SyntaxKind::Error,
//                        text,
//                        error,
//                        hint,
//                        span: i.span,
//                    })
//                };
//                match i.kind {
//                    SyntaxKind::Slash => parse_delimeted_expr(
//                        SyntaxKind::Slash,
//                        SyntaxKind::Italics,
//                        text,
//                        Some("Incomplete italic text".to_string()),
//                        Some("Text must be wrapped in slash pairs like /text/".to_string()),
//                    ),
//                    _ => Repr::SyntaxNode(SyntaxNode::new(i.kind, i.text.into(), i.span)),
//                }
//            }
//            _ => Repr::SyntaxNode(SyntaxNode {
//                kind: SyntaxKind::Eof,
//                text: "".into(),
//                span: Span::default(),
//            }),
//        }
//    }
//}
//

//#[cfg(test)]
//mod test {
//    use crate::*;
//
//    use self::kind::SyntaxKind;
//    use self::span::Span;
//
//    #[test]
//    fn parse_italics() {
//        let mut italics = Vec::new();
//        let mut errors = Vec::new();
//        let source = include_str!("../examples/tests/italics.norg");
//        let lexed = lexer::Lexer::new(source.into()).lex();
//        let mut nodes = parser::Parser::new(lexed.clone());
//        nodes.parse().iter().for_each(|node| {
//            if node.kind() == SyntaxKind::Italics {
//                assert_eq!(node.span(), Span::new(52, 69));
//                italics.push(node.text());
//            } else if node.kind() == SyntaxKind::Error {
//                errors.push(node.text());
//            }
//        });
//        assert_eq!(italics.len(), 1);
//        assert_eq!(errors.len(), 5);
//    }
//}
