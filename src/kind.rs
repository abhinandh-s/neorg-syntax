use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    /// '*'
    Star,
    /// '/'
    Slash,
    /// spaces and tabs (preserved)
    WhiteSpace,
    /// '\n'
    NewLine,
    /// any other text
    Text,
    /// represents end of file, added by lexer
    Eof,
    /// '*'
    Astrisk,
    /// '_' - a horizontal line and underline
    Underscore,

    /// `$` - Definition
    Dollar,

    // lists
    /// '~' ordered list
    Tilda,
    /// '-' unodered list
    Hyphen,

    LParen,
    RParen,
    LCurly,
    RCurly,

    /// `>` blockquote
    GreaterThan,
    LessThan,
    /// `=` - This modifier uses the `=` character and immediately closes all nesting levels.
    Equal,
    /// `|` pipe symbol can mean example, tables
    Pipe,
    /// '@' at symbol
    At,
    /// Error Node
    Error,
    /// root node of the document
    Root,
    /// `/` Italics
    Italics,
    Heading,
    ListItem,
    Bold,
    Strikethrough,
    UnderLined,
    IndentWhiteSpace,
    BoldMarker,
    HeadingMarker,

    // The following characters are reserved for the TODO status extension:
    /// -- `| |`: undone (a literal space)
    /// -- `x`: done
    Done,
    /// -- `?`: needs further input/clarification
    QuestionMark,
    /// -- `!`: urgent
    /// -- `+`: recurring (with an optional {**** timestamp extension}[timestamp])
    /// -- `-`: in-progress/pending
    /// -- `=`: on hold
    /// -- `_`: put down/cancelled
    Plus,
}

type T = SyntaxKind;

impl SyntaxKind {
    pub fn is_inline_expr(&self) -> bool {
        matches!(
            self,
            T::Astrisk | T::Slash | T::Underscore | T::Star | T::Hyphen | T::Tilda
        )
    }
}

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SyntaxKind::Star => "STAR",
                SyntaxKind::Slash => "SLASH",
                SyntaxKind::WhiteSpace => "WHITESPACE",
                SyntaxKind::NewLine => "NEWLINE",
                SyntaxKind::Text => "TEXT",
                SyntaxKind::Eof => "EOF",
                SyntaxKind::Astrisk => "ASTRISK",
                SyntaxKind::Underscore => "UNDERSCORE",
                SyntaxKind::Tilda => "TILDA",
                SyntaxKind::Hyphen => "HYPHEN",
                SyntaxKind::At => "AT",
                SyntaxKind::Error => "ERROR",
                SyntaxKind::Root => "ROOT",
                SyntaxKind::Italics => "ITALIC",
                SyntaxKind::Heading => "HEADING",
                SyntaxKind::ListItem => "LISTITEM",
                SyntaxKind::Bold => "BOLD",
                SyntaxKind::Strikethrough => "STRIKETHROUGH",
                SyntaxKind::UnderLined => "UNDERLINED",
                SyntaxKind::IndentWhiteSpace => "INDENTWHITESPACE",
                SyntaxKind::BoldMarker => "BOLDMARKER",
                SyntaxKind::HeadingMarker => "HEADINGMARKER",
                SyntaxKind::Dollar => "DOLLAR",
                SyntaxKind::LParen => "LPAREN",
                SyntaxKind::RParen => "RPAREN",
                SyntaxKind::LCurly => "LCURLY",
                SyntaxKind::RCurly => "RCURLY",
                SyntaxKind::GreaterThan => "GREATERTHAN",
                SyntaxKind::Equal => "EQUAL",
                SyntaxKind::Done => "DONE",
                SyntaxKind::QuestionMark => "QUESTIONMARK",
                SyntaxKind::Plus => "PLUS",
                SyntaxKind::LessThan => "LESSTHAN",
                SyntaxKind::Pipe => "PIPE",
            }
        )
    }
}
