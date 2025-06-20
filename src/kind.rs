use derive_screaming_snake_case::Display;

use crate::kind_to_char;

#[allow(non_camel_case_types)]
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    Comment,
    Spoiler,
    Pound,       // `#`
    At,          // `@`
    Slash,       // `/`
    Asterisk,    // `*`
    Caret,       // `^`
    Subscript,   // `^subscript^`
    Dollar,      // `$`
    Percent,     // `%`
    Exclamation, // `!`
    Comma,       // `,`
    Superscript, // `,superscript,`
    Pipe,        // `|`
    Underscore,  // `_`
    Backtick,    // `\``

    Hyphen,        // `-`
    StrikeThrough, // `-this is strike through-`

    NewLine,        // `\n`
    CarriageReturn, // `\r`
    Tab,            // `\t`

    Word,       // a word
    WhiteSpace, // ` `

    KwDocument, // `@document`
    KwMeta,     // `@document.meta`
    KwEnd,      // `@end`
    KwCode,     // `@code`

    LSquare, // `[`
    RSquare, // `]`
    LParen,  // `(`
    RParen,  // `)`
    LCurly,  // `{`
    RCurly,  // `}`

    GreaterThan, // `>`
    LessThan,    // `<`
    Error,       // `=`
    Italics,     // `/this is Italics/`
    Heading,     // `** this is heading`
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
    Paragraph,   // group of Paragraph Segments
    ParaSegment, // Paragraph Segments
    TextChunk,   // contains `Word`s and `WhiteSpace` only
    Verbatim,    // raw text
    Maths,       // $ contents inside this $
    __LAST,
}

impl SyntaxKind {
    pub const fn text(&self) -> char {
        kind_to_char(*self)
    }
    pub const fn is_inline_expr(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Asterisk // Bold 
                | SyntaxKind::Slash // Italics
                | SyntaxKind::Underscore // underline
                | SyntaxKind::Exclamation // Spoiler
                | SyntaxKind::Percent // Comment
                | SyntaxKind::Pipe // Verbatim
                | SyntaxKind::Dollar // Maths
                | SyntaxKind::Comma // Superscript
                | SyntaxKind::Caret // Subscript 
                | SyntaxKind::Hyphen // StrikeThrough
        )
    }
}
