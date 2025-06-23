#![deny(rust_2018_idioms, missing_docs)]

use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    str::CharIndices,
    sync::Arc,
};

use crate::SyntaxKind;

/// helper private trait to keep trait only accessable for `char`
///
/// see [`NeorgChar`] trait for example
mod char {
    pub(super) trait Sealed {}
    impl Sealed for char {}
}

/// wrapper around `Arc<TokenData>`
pub(crate) type Token = Arc<TokenData>;

/// A macro to easily create `TokenData` wrapped in `Arc`
///
/// # Parameters
///
/// - `kind`: `SyntaxKind`
/// - `text`: `String`
/// - `offset`: usize
///
/// Expands to:
/// ```ignore
/// Token::New(TokenData {
///   kind: SyntaxKind::WhiteSpace,
///   text: text.into(),
///   offset: offset,
/// })
/// ```
#[macro_export]
macro_rules! token {
    ($kind:expr, $text:expr, $offset:expr) => {
        std::sync::Arc::new(TokenData {
            kind: $kind,
            text: $text.into(),
            offset: $offset,
        })
    };
}

/// A lexical analysis function type used in the tokenizer/lexer stage.
///
/// `LexFn` represents the type of functions that accept a mutable reference to a
/// `Peekable<CharIndices>` iterator and return an optional `Token`.
///
/// # Parameters
///
/// - `chars`: A mutable reference to a `Peekable<CharIndices>`. This iterator yields `(usize, char)`
///   pairs representing the index and the character from the input string. The `Peekable` wrapper
///   allows lookahead behavior, which is essential for multi-character token recognition.
///
/// # Returns
///
/// - `Some(Token)`: If the function successfully lexes a valid token from the input.
///
/// # Example
///
/// ```ignore
/// fn lex_whitespace(chars: &mut Peekable<CharIndices>) -> Option<Token> {
///     let &(start_idx, ch) = chars.peek()?;
///     if ch.is_whitespace() {
///         let mut end = start_idx;
///         while let Some(&(_, c)) = chars.peek() {
///             if c.is_whitespace() {
///                 end += c.len_utf8();
///                 chars.next();
///             } else {
///                 break;
///             }
///         }
///         let text = " ".repeat((end - start_idx) / 1); // simplified
///         Some(token!(SyntaxKind::Whitespace, text, start_idx))
///     } else {
///         None
///     }
/// }
/// ```
///
/// This become handy when we put it in `HashMap`.
/// Any function with this Signature can be tied together.
type LexFn = fn(&mut Peekable<CharIndices<'_>>) -> Option<Token>;

/// This macro generates a function with the given name that checks if the current character
/// in the input stream matches a specified character literal. If it matches, the function
/// consumes the character and returns a token of the provided kind.
///
/// # Parameters
///
/// - `$fn_name`: The name of the generated function.
/// - `$char`: The character literal (`'('`, `'='`, `'*'`, etc.) to match against the current input.
/// - `$kind`: The `SyntaxKind` variant (or equivalent enum/constant) to assign to the resulting token.
///
/// # Generated Function Signature
///
/// ```ignore
/// fn $fn_name(chars: &mut Peekable<CharIndices<'_>>) -> Option<Token>
/// ```
///
/// # Behavior
///
/// - If the next character in the stream matches `$char`, it consumes the character and
///   returns a `Token` of kind `$kind`, with the correct `text` and `offset`.
/// - If the character does not match, it returns `None`.
///
/// # Example
///
/// ```ignore
/// define_lex_fn!(lex_bang, '!', SyntaxKind::Bang);
///
/// // This generates:
/// // fn lex_bang(chars: &mut Peekable<CharIndices<'_>>) -> Option<Token> {
/// //     let (start, ch) = *chars.peek()?;
/// //     if ch == '!' {
/// //         chars.next();
/// //         Some(token!(SyntaxKind::Bang, '!', start))
/// //     } else {
/// //         None
/// //     }
/// // }
/// ```
macro_rules! define_lex_fn {
    ($fn_name:ident, $char:literal, $kind:expr) => {
        fn $fn_name(chars: &mut std::iter::Peekable<std::str::CharIndices<'_>>) -> Option<Token> {
            let (start, ch) = *chars.peek()?;
            if ch == $char {
                chars.next();
                Some(token!($kind, $char, start))
            } else {
                None
            }
        }
    };
}

/// Defines multiple single-character punctuation lexer functions and related utilities.
///
/// This macro batch-generates:
/// - Individual lexer functions using [`define_lex_fn!`]
/// - A `NeorgPunct` trait with an `is_punctuation` method for filtering characters
/// - A `punctuation_tokenizers()` function that maps punctuation characters to their lexer functions
/// - A `char_to_kind()` utility that returns the [`SyntaxKind`] for a given punctuation character
/// - A unit test to ensure all declared punctuation characters are recognized by the trait
///
/// # Parameters
///
/// A list of tuples in the format:
/// ```ignore
/// (punctuation_char, lexer_fn_name, SyntaxKind_variant)
/// ```
///
/// - `punctuation_char`: A single-character literal (e.g., `'='`, `'>'`, `'*'`)
/// - `lexer_fn_name`: The function name to be generated (e.g., `lex_equals`)
/// - `SyntaxKind_variant`: The corresponding [`SyntaxKind`] enum variant
///
/// # Example
///
/// ```ignore
/// define_punct_lexers! {
///     ('=', lex_equals, SyntaxKind::Equals),
///     ('*', lex_star,   SyntaxKind::Star),
///     ('!', lex_bang,   SyntaxKind::Bang),
/// }
/// ```
///
/// This will generate:
///
/// - `fn lex_equals(...) -> Option<Token>`
/// - `fn lex_star(...) -> Option<Token>`
/// - `fn lex_bang(...) -> Option<Token>`
/// - Trait `NeorgPunct` with method `is_punctuation`
/// - Function `punctuation_tokenizers()` returning a `HashMap<char, LexFn>`
/// - Function `char_to_kind(char) -> SyntaxKind`
/// - Unit test `test_punct_trait()`
///
/// # Generated Items
///
/// ### Lexer Functions
/// Each punctuation character gets a simple matcher function using [`define_lex_fn!`] that checks
/// the next character in the stream, consumes it if it matches, and returns a [`Token`].
///
/// ### Trait: `NeorgPunct`
/// Adds an `is_punctuation()` method to the `char` type, returning `true` if the character
/// is among the defined punctuation characters.
///
/// ### `punctuation_tokenizers()`
/// A dispatch table to get the appropriate lexer function for a punctuation character.
///
/// ### `char_to_kind(char)`
/// Converts a punctuation character to its `SyntaxKind`. Panics on unsupported input.
///
/// ### `test_punct_trait()`
/// Verifies that all registered punctuation characters return `true` for `is_punctuation()`.
///
/// # Requirements
///
/// - The macro assumes [`SyntaxKind`] and [`Token`] types are in scope.
/// - It depends on the [`define_lex_fn!`] macro being available.
/// - This macro should be invoked at module level, not inside a function.
///
macro_rules! define_punct_lexers {
    // $kind is path not expr, cuz it wont allow kind_to_char method
    ($(($char:literal, $fn_name:ident, $kind:path)),* $(,)?) => {
        $(
            define_lex_fn!($fn_name, $char, $kind);
        )*

        trait NeorgPunct: char::Sealed {
            fn is_punctuation(&self) -> bool;
        }

        impl NeorgPunct for char {
            /// # Punctuation
            ///
            /// A {** characters}[character] is considered *punctuation* if it is any of the following:
            ///
            /// - A standard ASCII punctuation character: `|!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~|`
            /// - Anything in the general Unicode categories `Pc`, `Pd`, `Pe`, `Pf`, `Pi`, `Po` or `Ps`.
            ///
            /// -- NOTE: i have not included all general Unicode categories.
            ///          These 7 categories contains almost 850 chars.
            ///          Since, `Neorg` doesn't uses all of it. i don't see any point
            ///          puting it all here.
            fn is_punctuation(&self) -> bool {
                matches!(self, $( $char )|*)
            }
        }

        use std::sync::LazyLock;
        use std::collections::HashMap;

        static PUNCTUATION_TOKENIZERS: LazyLock<HashMap<char, LexFn>> = LazyLock::new(|| {
             HashMap::from([
                 $(
                     ($char, $fn_name as LexFn),
                 )*
             ])

        });

        fn punctuation_tokenizers() -> &'static HashMap<char, LexFn> {
            &PUNCTUATION_TOKENIZERS
        }

        ///  returns `SyntaxKind` of corresponding `char`
        ///
        ///  > is indented for `K!` macro
        ///
        /// # Parameters
        ///
        ///  - SyntaxKind
        ///
        /// # Panic
        ///
        /// panics if given `char` is not a punctuation char
        pub(super) const fn char_to_kind(c: char) -> SyntaxKind {
            match c {
                $(
                    $char => $kind,
                )*
                _ => panic!("not a PUNCTUATION char"),
            }
        }

        ///  returns `char` of corresponding `SyntaxKind`
        ///
        /// # Parameters
        ///
        ///  - SyntaxKind
        ///
        /// # Panic
        ///
        /// panics if given `SyntaxKind` is not a punctuation Kind
        pub(crate) const fn kind_to_char(kind: SyntaxKind) -> char {
            match kind {
                $(
                    $kind => $char,
                )*
                _ => panic!("not a PUNCTUATION kind"),
            }
        }

        #[test]
        fn test_punct_trait() {
            let char = [$(
                ($char),
            )*];
            for c in char {
                assert!(c.is_punctuation());
            }
        }

    };
}

/// A shorthand macro for mapping a character literal to its corresponding `SyntaxKind`.
///
/// This is a wrapper around [`char_to_kind()`], and the macro works like the `T!` in rust-analyzer
///
/// # Example
///
/// ```ignore
/// assert_eq!(K!('='), SyntaxKind::Equals);
/// assert_eq!(K!('*'), SyntaxKind::Star);
/// ```
///
/// # Requirements
///
/// - The macro expects that the character passed as `$ch` is one of the defined punctuation characters.
/// - If an unsupported character is passed, it will panic at runtime via [`char_to_kind()`].
///
/// # Use Case
///
/// Primarily used in tests, hand-written token streams, or pattern matchers where
/// shorthand access to a `SyntaxKind` via a character is beneficial.
///
/// # See Also
///
/// - [`char_to_kind()`] — the function used internally to perform the mapping.
/// - [`SyntaxKind`] — the enum of token types.
///
/// # Notes
///
/// - `$ch` should be a valid single-character literal (e.g., `'='`, `'>'`, `'!'`)
/// - This macro is only as safe as the underlying `char_to_kind()` function.
///
/// ---
#[macro_export]
macro_rules! K {
    ($ch:tt) => {
        $crate::char_to_kind($ch)
    };
}

/// represents a single token emitted by `Lexer`
#[derive(Clone, PartialEq, Eq)]
pub struct TokenData {
    text: String,
    kind: SyntaxKind,
    offset: usize,
}

/// set and get methods for TokenData
impl TokenData {
    /// Creates a new [`TokenData`].
    pub fn new(text: String, kind: SyntaxKind, offset: usize) -> Self {
        Self { text, kind, offset }
    }

    /// Returns a reference to the text of this [`TokenData`].
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Returns the kind of this [`TokenData`].
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Returns the offset of this [`TokenData`].
    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl Display for TokenData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}", self.kind, self.offset)
    }
}

impl Debug for TokenData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{} {:?}", self.kind, self.offset, self.text)
    }
}

/// The `Lexer`
///
/// # Fields
///
/// - `source`: the input for lexing `&str`
/// - `tokens`: stores the lexed [`Token`]
#[derive(Debug)]
pub(crate) struct Lexer<'a> {
    source: &'a str,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    /// Creates a new [`Lexer`].
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
        }
    }

    /// Returns a reference to the lex of this [`Lexer`].
    pub fn lex(&mut self) -> &Vec<Token> {
        let mut chars = self.source.char_indices().peekable();

        let eof_offset = self.source.chars().count();

        while let Some(&(_, char)) = chars.peek() {
            if let Some(&lex_fn) = punctuation_tokenizers().get(&char) {
                if let Some(tok) = lex_fn(&mut chars) {
                    self.tokens.push(tok);
                    continue;
                }
            }

            if let Some(tok) = lex_line_ending(&mut chars) {
                self.tokens.push(tok);
                continue;
            }

            // TODO: write test
            if let Some(tok) = lex_escaped_char(&mut chars) {
                self.tokens.push(tok);
                continue;
            }

            if let Some(tok) = lex_white_space(&mut chars) {
                self.tokens.push(tok);
                continue;
            }

            if let Some(tok) = lex_text(&mut chars) {
                self.tokens.push(tok);
                continue;
            }

            chars.next();
        }

        self.tokens.push(token!(SyntaxKind::Eof, '\0', eof_offset));
        &self.tokens
    }
}

trait NeorgChar: char::Sealed {
    fn is_neorg_char(&self) -> bool;
}

impl NeorgChar for char {
    fn is_neorg_char(&self) -> bool {
        self.is_punctuation() || self.is_zs_whitespace() || self.is_line_ending()
    }
}

define_punct_lexers![
    ('&', lex_ampersand, SyntaxKind::Ampersand),
    ('?', lex_question_mark, SyntaxKind::QuestionMark),
    ('~', lex_tilda, SyntaxKind::Tilda),
    ('"', lex_double_qoute, SyntaxKind::DoubleQoute),
    ('<', lex_less_than, SyntaxKind::Equal),
    ('=', lex_equal, SyntaxKind::LessThan),
    ('>', lex_greater_than, SyntaxKind::GreaterThan),
    ('*', lex_asterisk, SyntaxKind::Asterisk),
    ('+', lex_plus, SyntaxKind::Plus),
    (':', lex_colon, SyntaxKind::Colon),
    (';', lex_semicolon, SyntaxKind::Semicolon),
    ('@', lex_at, SyntaxKind::At),
    ('.', lex_dot, SyntaxKind::Dot),
    ('\t', lex_tab, SyntaxKind::Tab), // Tabs are not expanded to spaces
    ('#', lex_pound, SyntaxKind::Pound),
    ('[', lex_l_brace, SyntaxKind::LSquare),
    (']', lex_r_brace, SyntaxKind::RSquare),
    ('{', lex_l_curly, SyntaxKind::LCurly),
    ('}', lex_r_curly, SyntaxKind::RCurly),
    ('(', lex_l_paren, SyntaxKind::LParen),
    (')', lex_r_paren, SyntaxKind::RParen),
    ('_', lex_underscore, SyntaxKind::Underscore),
    ('-', lex_hyphen, SyntaxKind::Hyphen),
    ('/', lex_slash, SyntaxKind::Slash),
    ('^', lex_caret, SyntaxKind::Caret),
    (',', lex_comma, SyntaxKind::Comma),
    ('\'', lex_single_qoute, SyntaxKind::SingleQoute),
    ('`', lex_backtick, SyntaxKind::Backtick),
    ('%', lex_percent, SyntaxKind::Percent),
    ('|', lex_pipe, SyntaxKind::Pipe),
    ('$', lex_dollar, SyntaxKind::Dollar),
    ('!', lex_exclamation, SyntaxKind::Exclamation),
];

/// # Escaping
///
/// A single [`character`] can be escaped if it is immediately preceded by a backslash,
/// `|\|` (`U+005C`).
///
/// Escaping renders the next character verbatim. Any [`character`] may be escaped
/// apart from `characters` within free-form and ranged verbatim segments (see free-form
/// attached modifiers and verbatim ranged tags).
/// -- TODO: should we check `verbatim` at lexing stage
fn lex_escaped_char(chars: &mut Peekable<CharIndices<'_>>) -> Option<Token> {
    let (offset, char) = *chars.peek()?;
    if char != '\\' {
        return None;
    }
    chars.next(); // char is '\' so eat it

    if let Some((_, char)) = chars.peek() {
        if char.is_punctuation() {
            return Some(token!(
                SyntaxKind::EscapedChar,
                format!("\\{}", &char),
                offset
            ));
        }
    }

    // if there is no punctuation char after `\` then lex it as `ForwardSlash`
    Some(token!(SyntaxKind::ForwardSlash, '\\', offset))
}

/// # Words
///  
///  The Norg format is designed to be parsed on a word-by-word basis from left-to-right through the
///  entire document _in a single pass_. This is possible because the language is [free-form], meaning
///  that whitespace has no semantic meaning, and because the markup follows strict rules which are
///  outlined in the later sections of this document.
///
///  A `word` is considered to be any combination of `regular characters`.
fn lex_text(chars: &mut Peekable<CharIndices<'_>>) -> Option<Token> {
    let (offset, _) = *chars.peek()?;
    // -- NOTE: we might need lex_verbatim function

    // no assestion and return None as we want everything to fall here

    let mut text = String::new();

    while let Some((_, char)) = chars.peek() {
        if char.is_neorg_char() {
            break;
        }
        text.push(*char);
        chars.next();
    }

    Some(token!(SyntaxKind::Word, text, offset))
}

/// helper trait can only be implemented on char
#[allow(clippy::wrong_self_convention)]
trait NorgChar: char::Sealed {
    fn is_zs_whitespace(self) -> bool;
    fn is_line_ending(self) -> bool;
}

#[rustfmt::skip]
impl NorgChar for char {
    /// # Unicode Characters in the 'Separator, Space' Category
    ///
    /// <https://www.fileformat.info/info/unicode/category/Zs/list.htm>
    #[inline(always)]
    fn is_zs_whitespace(self) -> bool {
        matches!(
            self,
            '\u{0020}' // SPACE
          | '\u{00A0}' // NO-BREAK SPACE
          | '\u{1680}' // OGHAM SPACE MARK
          | '\u{2000}' // EN QUAD
          | '\u{2001}' // EM QUAD
          | '\u{2002}' // EN SPACE
          | '\u{2003}' // EM SPACE
          | '\u{2004}' // THREE-PER-EM SPACE
          | '\u{2005}' // FOUR-PER-EM SPACE
          | '\u{2006}' // SIX-PER-EM SPACE
          | '\u{2007}' // FIGURE SPACE
          | '\u{2008}' // PUNCTUATION SPACE
          | '\u{2009}' // THIN SPACE
          | '\u{200A}' // HAIR SPACE
          | '\u{202F}' // NARROW NO-BREAK SPACE
          | '\u{205F}' // MEDIUM MATHEMATICAL SPACE
          | '\u{3000}' // IDEOGRAPHIC SPACE
        )
    }

    /// # Line Endings
    /// 
    /// Line endings in Norg serve as a termination character.
    /// They are used e.g. to terminate `paragraph segments`, `paragraphs`
    /// and other elements like the endings of `range-able detached modifiers`.
    /// They are not considered `whitespace`.
    ///
    /// The following chars are considered line endings:
    /// 
    /// - A line feed `U+000A` (\n)
    /// - A form feed `U+000C`
    /// - A carriage return `U+000D` (\r)
    ///
    /// The following line ending combinations are permitted:
    /// 
    /// - A single line feed
    /// - A single carriage return
    /// - A carriage return immediately followed by a line feed
    #[inline(always)]
    fn is_line_ending(self) -> bool {
        matches!(self, '\u{000A}' | '\u{000C}' | '\u{000D}')
    }
}

/// The following chars are considered line endings:
///
/// - `U+000A` — Line Feed (\n)
/// - `U+000C` — Form Feed
/// - `U+000D` — Carriage Return (\r)
///
/// The following line ending combinations are permitted:
///
/// - A single line feed (\n)
/// - A single carriage return (\r)
/// - A CRLF combo — carriage return immediately followed by line feed (\r\n)
///
/// This mirrors real-world newline conventions:
///
/// | OS               | Line Ending |
/// |------------------|-------------|
/// | Unix/Linux/macOS | \n          |
/// | Windows          | \r\n        |
/// | Classic Mac      | \r          |
///
fn lex_line_ending(chars: &mut Peekable<CharIndices<'_>>) -> Option<Token> {
    let (offset, char) = *chars.peek()?;

    if !char.is_line_ending() {
        return None;
    }

    chars.next();

    if char == '\r' {
        if let Some((_, next_char)) = chars.peek() {
            if *next_char == '\n' {
                chars.next();
                return Some(token!(SyntaxKind::LineEnding, "\r\n", offset));
            }
        }
    }

    Some(token!(SyntaxKind::LineEnding, char, offset))
}

#[test]
fn test_lex_line_endings() {

    fn check(input: &str, expected: &str) {
        let mut chars = input.char_indices().peekable();
        let token = lex_line_ending(&mut chars).expect("Expected line ending");
        assert_eq!(token.text(), expected);
    }

    // U+000A (Line Feed)
    check("\nrest", "\n");

    // U+000C (Form Feed)
    check("\u{000C}next", "\u{000C}");

    // U+000D (Carriage Return)
    check("\rnext", "\r");

    // U+000D U+000A (CRLF)
    check("\r\nnext", "\r\n");

    // Invalid: No line ending
    let mut it = "hello".char_indices().peekable();
    assert!(lex_line_ending(&mut it).is_none());

    // Only \r followed by EOF — should not panic
    let mut it = "\r".char_indices().peekable();
    let tok = lex_line_ending(&mut it).unwrap();
    assert_eq!(tok.text(), "\r");
}


/// # Whitespace
///
/// A [`character`] is considered *whitespace* if it constitutes any code point in the
/// [Unicode Zs general category](https://www.fileformat.info/info/unicode/category/Zs/list.htm).
///
/// Any combination of the above is also considered whitespace.
///
/// Tabs are not expanded to spaces and since whitespace has no semantic meaning there is no need
/// to define a default tab stop. However, if a parser must (for implementation reasons) define a
/// tab stop, we suggest setting it to 4 spaces.
///
/// Any line may be preceded by a variable amount of whitespace, which should be ignored. Upon
/// entering the beginning of a new line, it is recommended for parsers to continue consuming (and
/// discarding) consecutive whitespace characters exhaustively.
///
/// The "start of a line" is considered to be /after/ this initial whitespace has been parsed.
/// Keep this in mind when reading the rest of the document.
/// -- NOTE: we are not consuming and discarding whitespace's at the beginning of lines, even
///          though the spec says
///          cuz, it mess up the spans / offset (and the concept of CST)
///          maybe a warning should be provided while parsing, with low severity
fn lex_white_space(chars: &mut Peekable<CharIndices<'_>>) -> Option<Token> {
    let (offset, _) = *chars.peek()?;
    if chars
        .peek()
        .copied()
        .map(|(_, char)| char.is_zs_whitespace() && char != '\n')
        != Some(true)
    {
        return None;
    }
    let mut text = String::new();
    while let Some((_, char)) = chars.peek() {
        if char.is_zs_whitespace() && *char != '\n' {
            text.push(*char);
            chars.next();
        } else {
            break;
        }
    }
    Some(token!(SyntaxKind::WhiteSpace, text, offset))
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::*;

    #[test]
    fn test_ws() {
        let input = "     ";
        let re = lex_white_space(&mut input.char_indices().peekable());
        if let Some(tok) = re {
            assert_eq!(tok.kind, SyntaxKind::WhiteSpace);
            assert_eq!(tok.text.len(), 5);
            assert_eq!(tok.text, "     ");
        }
    }
    #[test]
    fn fuzz_unknown_characters_are_error() {
        let input = "🙂⚠️©®€🤯⌘";
        let mut binding = Lexer::new(input);
        let tokens = binding.lex();
        for tok in tokens {
            if tok.kind() == SyntaxKind::Eof {
                break;
            }
            assert_eq!(tok.kind(), SyntaxKind::Word);
        }
    }

    #[test]
    fn token_macro_creates_token() {
        let tok = token!(SyntaxKind::Equal, "=", 10);
        assert_eq!(tok.kind(), SyntaxKind::Equal);
        assert_eq!(tok.text(), "=");
        assert_eq!(tok.offset(), 10);
    }

    #[test]
    fn test_neorg_char_trait() {
        assert!('='.is_neorg_char());
        assert!('\u{2003}'.is_neorg_char());
        assert!(!'a'.is_neorg_char());
    }

    #[test]
    fn lexer_correctly_lexes_text_and_ws() {
        let mut lexer = Lexer::new("hello  world");
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].kind(), SyntaxKind::Word);
        assert_eq!(tokens[1].kind(), SyntaxKind::WhiteSpace);
        assert_eq!(tokens[2].kind(), SyntaxKind::Word);
        assert_eq!(tokens[3].kind(), SyntaxKind::Eof);
    }

    #[test]
    fn lexer_handles_unknown_characters_as_text() {
        let mut lexer = Lexer::new("\u{001F}");
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind(), SyntaxKind::Word);
        assert_eq!(tokens[1].kind(), SyntaxKind::Eof);
    }
}
