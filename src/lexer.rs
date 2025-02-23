use std::fmt::Display;

use ropey::Rope;

use crate::kind::SyntaxKind;
use crate::span::Span;
use crate::NeoChar as _;

pub struct Lexer {
    source: Rope,
    start: usize,   // start index (char offset) of current lexeme
    current: usize, // current index (char offset)
}

impl Lexer {
    pub fn new(source: Rope) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len_bytes()
    }

    // peek at the current charecter without consuming it.
    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            self.source
                .get_slice(self.current..)
                .unwrap()
                .chars()
                .next()
        }
    }

    // eat current char and return next char, updating current
    fn advance(&mut self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            self.source
                .get_slice(self.current..)
                .unwrap()
                .chars()
                .next()
                .map(|c| {
                    let char = c;
                    self.current += char.len_utf8();
                    char
                })
        }
    }

    fn make_token(&self, token_kind: SyntaxKind) -> Token {
        Token {
            kind: token_kind,
            text: self
                .source
                .get_slice(self.start..self.current)
                .unwrap()
                .to_string(),
            span: Span::new(self.start, self.current),
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.scan();
            tokens.push(token.clone());
            if token.kind == SyntaxKind::Eof {
                break;
            }
        }
        tokens
    }

    pub fn scan(&mut self) -> Token {
        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(SyntaxKind::Eof);
        }

        self.advance()
            .map(|i| match i {
                '*' => self.make_token(SyntaxKind::Astrisk),
                '@' => self.make_token(SyntaxKind::At),
                '-' => self.make_token(SyntaxKind::Hyphen),
                '~' => self.make_token(SyntaxKind::Tilda),
                '/' => self.make_token(SyntaxKind::Slash),
                '_' => self.make_token(SyntaxKind::Underscore),
                '\n' => self.make_token(SyntaxKind::NewLine),
                ' ' | '\t' => {
                    // Consume contiguous spaces and tabs.
                    while let Some(ch) = self.peek() {
                        if ch == ' ' || ch == '\t' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.make_token(SyntaxKind::WhiteSpace)
                }
                _ => {
                    while let Some(char) = self.peek() {
                        if char.is_special_char() {
                            break;
                        }
                        self.advance();
                    }
                    self.make_token(SyntaxKind::Text)
                }
            })
            .unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Token {
    pub kind: SyntaxKind,
    pub text: String,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?} {}", self.kind, self.text, self.span)
    }
}
