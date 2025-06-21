use derive_screaming_snake_case::Display;

#[allow(non_camel_case_types)]
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    Pound, // `#`
    At, // `@`
    Slash, // `/`
    Asterisk, // `*`
    Caret, // `^`
    Dollar, // `$`
    Percent, // `%`
    Exclamation, // `!`
    Comma, // `,`
    Pipe, // `|`
    Underscore, // `_`
    Backtick, // `\``

    NewLine, // `\n`
    CarriageReturn, // `\r`
    Tab, // `\t`

    Word, // a word
    WhiteSpace, // ` `

    KwDocument, // `@document`
    KwMeta, // `@document.meta`
    KwEnd, // `@end`
    KwCode, // `@code`

    LSquare,  // `[` 
    RSquare, // `]`
    LParen, // `(`
    RParen, // `)`
    LCurly, // `{`
    RCurly, // `}`

    GreaterThan, // `>`
    LessThan, // `<`
    Error, // `=`
    Italics, // `/this is Italics/`
    Heading, // `** this is heading`
    Bold,
    HeadingMarker,
    Eof,
    Semicolon,
    Colon,
    Equal,
    StringLiteral,
    Type,
    Ident,
    Document, // the root node type
    Dot,
    Emph,
    Paragraph,
    TextChunk, // better call Paragraph segments
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
