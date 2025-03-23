use std::fmt::{Debug, Display};

use ropey::str_utils::{byte_to_char_idx, byte_to_line_idx, line_to_char_idx};
use tower_lsp::lsp_types::{Position, Range};

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Span {
    /// start byte offset of the Token
    pub start: usize,
    /// end byte offset of the Token
    pub end: usize,
}

pub trait SpanOps {
    fn span(&self) -> Span;
}

impl SpanOps for str {
    fn span(&self) -> Span {
        Span {
            start: 0,
            end: self.bytes().len(),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ span: {}..{} ]", self.start, self.end)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// -- FIX: erratic
    pub fn to_char_offset(&self, text: &str) -> Span {
        let s = byte_to_char_idx(text, self.start);
        let e = byte_to_char_idx(text, self.end);
        text[s..e].span()
    }

    /// returns (beginning of char_offset, char len)
    pub fn char_position(&self, text: &str) -> (usize, usize) {
        let s = byte_to_char_idx(text, self.start);
        let e = byte_to_char_idx(text, self.end);
        (s + 1, e.saturating_sub(s))
    }

    /// Position in a text document expressed as zero-based line and character offset.
    /// A position is between two characters like an 'insert' cursor in a editor.
    ///
    ///     // Line position in a document (zero-based).
    ///     // Character offset on a line in a document (zero-based). The meaning of this
    ///     // offset is determined by the negotiated `PositionEncodingKind`.
    ///
    ///     // If the character value is greater than the line length it defaults back
    ///     // to the line length.
    ///
    /// zero based indexing
    /// take a &str and try to convert the given span to lsp range
    #[doc(hidden)]
    #[doc = r" This is a doc comment."]
    pub fn into_zero_based_lsp_range(self, text: &str) -> Result<Range, anyhow::Error> {
        // Determine 0-indexed start and end lines.
        let line_start = byte_to_line_idx(text, self.start) as u32;
        let line_end = byte_to_line_idx(text, self.end) as u32;

        // Determine 0-indexed character offsets.
        let char_start_offset = byte_to_char_idx(text, self.start) as u32;
        let char_end_offset = byte_to_char_idx(text, self.end) as u32;

        // Compute the first char index for the start and end lines.
        let line_start_first_idx = line_to_char_idx(text, line_start as usize) as u32;
        let line_end_first_idx = line_to_char_idx(text, line_end as usize) as u32;

        let start_diff = char_start_offset.saturating_sub(line_start_first_idx);
        let end_diff = char_end_offset.saturating_sub(line_end_first_idx);

        Ok(Range {
            start: Position::new(line_start, start_diff),
            end: Position::new(line_end, end_diff),
        })
    }
}

impl Default for Span {
    fn default() -> Self {
        Self { start: 1, end: 1 }
    }
}
