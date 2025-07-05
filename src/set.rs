// Acknowledgement:
// Based on rust-analyzer's `TokenSet`.
// https://github.com/rust-lang/rust-analyzer/blob/master/crates/parser/src/token_set.rs

use crate::SyntaxKind;

/// A set of syntax kinds.
#[derive(Default, Copy, Clone)]
pub struct SyntaxSet(u128);

impl SyntaxSet {
    /// Create a new set from a slice of kinds.
    pub const fn new() -> Self {
        Self(0)
    }

    /// Insert a syntax kind into the set.
    ///
    /// You can only add kinds with discriminator < 128.
    pub const fn add(self, kind: SyntaxKind) -> Self {
        assert!((kind as u8) < BITS);
        Self(self.0 | bit(kind))
    }

    /// Combine two syntax sets.
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Whether the set contains the given syntax kind.
    pub const fn contains(&self, kind: SyntaxKind) -> bool {
        (kind as u8) < BITS && (self.0 & bit(kind)) != 0
    }
}

const BITS: u8 = 128;

const fn bit(kind: SyntaxKind) -> u128 {
    1 << (kind as usize)
}

/// Generate a compile-time constant `SyntaxSet` of the given kinds.
#[macro_export]
macro_rules! syntax_set {
    ($($kind:ident),* $(,)?) => {{
        const SET: $crate::SyntaxSet = $crate::set::SyntaxSet::new()
            $(.add($crate::SyntaxKind::$kind))*;
        SET
    }}
}

/// Syntax kinds that can start a `|!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~|`.
/// Dot is excluded from PUNCTUATION's set
pub const PUNCTUATIONS: SyntaxSet = syntax_set!(
    Exclamation,
    DoubleQoute,
    Pound,
    Dollar,
    Percent,
    Ampersand,
    SingleQoute,
    LParen,
    RParen,
    Asterisk,
    Plus,
    Comma,
    Hyphen,
    Slash,
    Colon,
    Semicolon,
    LessThan,
    Equal,
    GreaterThan,
    QuestionMark,
    At,
    LSquare,
    ForwardSlash,
    RSquare,
    Caret,
    Underscore,
    Backtick,
    LCurly,
    Pipe,
    RCurly,
    Tilda
);

/// - *bold*
/// - /italic/
/// - _underline_
/// - -strike-through-
/// - !spoiler!
/// - `%null modifier%`
/// - TODO: ^superscript^ (cannot be nested into `subscript`)
/// - TODO: ,subscript, (cannot be nested into `superscript`)
/// - TODO: `inline code` (disables any nested markup - verbatim)
/// - TODO: $f(x) = y$ (verbatim)
/// - &variable& (verbatim)
/// 
/// -- NOTE: no `Pipe` here 
pub const ATTACHED_MODIFIERS: SyntaxSet = syntax_set!(
    Asterisk,
    Slash,
    Underscore,
    Hyphen,
    Exclamation,
    Caret,
    Comma,
    Backtick,
    Percent,
    Dollar,
    Ampersand,
);
