use derive_screaming_snake_case::Display;

use crate::{PuncKind, kind_to_char};

#[allow(non_camel_case_types)]
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    TombStone = 0,
    Eof = 1,
    Error = 2,      // `=`
    LineEnding = 3, // `\n`, `\r`, `\u{000C}`
    ParaBreak = 4,
    Tab = 5, // `\t`
    Word,    // a word
    HorizontalRule,
    EscapedChar,   // neorg specific chars can be escaped
    Comment,       // %this is inline comment%, there is also an another way
    Spoiler,       // !this is a spoiler!
    Pound,         // `#`
    At,            // `@`
    Slash,         // `/`
    ForwardSlash,  // `\`
    QuestionMark,  // `?`
    Asterisk,      // `*`
    Plus,          // `+`
    Caret,         // `^`
    Subscript,     // `^subscript^`
    Dollar,        // `$`
    Percent,       // `%`
    Exclamation,   // `!`
    Comma,         // `,`
    Ampersand,     // `&`
    DoubleQoute,   // '"'
    SingleQoute,   // `'`
    Tilda,         // `~`
    Superscript,   // `,superscript,`
    Pipe,          // `|`
    Underscore,    // `_`
    Backtick,      // `\``
    Hyphen,        // `-`
    StrikeThrough, // `-this is strike through-`
    WhiteSpace,    // ` `
    KwDocument,    // `@document`
    KwMeta,        // `@document.meta`
    KwEnd,         // `@end`
    KwCode,        // `@code`
    LSquare,       // `[`
    RSquare,       // `]`
    LParen,        // `(`
    RParen,        // `)`
    LCurly,        // `{`
    RCurly,        // `}`
    GreaterThan,   // `>`
    LessThan,      // `<`
    Quote,         // > this is a quote
    Italics,       // `/this is Italics/`
    Heading,       // `** this is heading`
    Bold,
    HeadingMarker,
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
    UnderLine,
    InlineCode,
    NullModifier,
    InlineMath,
    Variable,
    LinkLocation,
    LinkDescription,
    Link,
    __LAST,
}

impl SyntaxKind {
    /// Whether this is an error.
    pub fn is_error(self) -> bool {
        self == Self::Error
    }

    #[track_caller]
    pub fn text(&self) -> String {
        if self.is_punctuation() {
            kind_to_char(*self).to_string()
        } else {
            format!("{:?}", *self)
        }
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

    pub const fn as_attached_modifers(&self) -> Option<Self> {
        match self {
            SyntaxKind::Asterisk => Some(SyntaxKind::Bold),
            SyntaxKind::Slash => Some(SyntaxKind::Italics),
            SyntaxKind::Underscore => Some(SyntaxKind::UnderLine),
            SyntaxKind::Exclamation => Some(SyntaxKind::Spoiler),
            SyntaxKind::Caret => Some(SyntaxKind::Superscript),
            SyntaxKind::Comma => Some(SyntaxKind::Comma),
            SyntaxKind::Backtick => Some(SyntaxKind::InlineCode),
            SyntaxKind::Percent => Some(SyntaxKind::NullModifier),
            SyntaxKind::Dollar => Some(SyntaxKind::Maths),
            SyntaxKind::Ampersand => Some(SyntaxKind::Variable),
            SyntaxKind::Hyphen => Some(SyntaxKind::StrikeThrough),
            SyntaxKind::Pipe => Some(SyntaxKind::Verbatim),
            _ => None,
        }
    }

    pub fn as_attached_modifers_unchecked(&self) -> Self {
        match self {
            SyntaxKind::Asterisk => SyntaxKind::Bold,
            SyntaxKind::Slash => SyntaxKind::Italics,
            SyntaxKind::Underscore => SyntaxKind::UnderLine,
            SyntaxKind::Exclamation => SyntaxKind::Spoiler,
            SyntaxKind::Caret => SyntaxKind::Superscript,
            SyntaxKind::Comma => SyntaxKind::Comma,
            SyntaxKind::Backtick => SyntaxKind::InlineCode,
            SyntaxKind::Percent => SyntaxKind::NullModifier,
            SyntaxKind::Dollar => SyntaxKind::Maths,
            SyntaxKind::Ampersand => SyntaxKind::Variable,
            SyntaxKind::Hyphen => SyntaxKind::StrikeThrough,
            SyntaxKind::Pipe => SyntaxKind::Verbatim,
            _ => panic!("can't cast {} to attached modifers", self),
        }
    }
}
