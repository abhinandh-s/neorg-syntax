use derive_screaming_snake_case::Display;

#[allow(non_camel_case_types)]
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    Pound,
    At,
    Slash,
    Asterisk,
    Caret,
    Dollar,
    Percent,
    Exclamation,
    Comma,
    Pipe,
    Underscore,
    Backtick,
    WhiteSpace,

    NewLine,
    CarriageReturn,
    Tab,

    Text,

    KW_Document,
    KW_Meta,
    KW_End,

    LSquare,
    RSquare,
    LParen,
    RParen,
    LCurly,
    RCurly,

    GreaterThan,
    LessThan,
    Error,
    Italics,
    Heading,
    Bold,
    HeadingMarker,
    Eof,
    Semicolon,
    Colon,
    Equal,
    StringLiteral,
    Type,
    Ident,
    Document,
    Dot,
    Emph,
    TextChunk,
    Paragraph,
    __LAST,
}

impl SyntaxKind {
    pub fn is_inline_expr(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Asterisk | SyntaxKind::Slash | SyntaxKind::Underscore
        )
    }
}
