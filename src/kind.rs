use derive_screaming_snake_case::Display;

#[derive(Debug, Display, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    Slash,
    Astrisk,
    Underscore,
    WhiteSpace,
    NewLine,
    Text,

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
    __LAST,
    Semicolon,
    Colon,
    Equal,
    StringLiteral,
    Type,
    Whitespace,
    Ident,
    Document,
}

impl SyntaxKind {
    pub fn is_inline_expr(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Astrisk | SyntaxKind::Slash | SyntaxKind::Underscore
        )
    }
}
