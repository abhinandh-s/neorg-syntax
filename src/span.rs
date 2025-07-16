#![allow(dead_code)]

//! # Span
//!
//! SyntaxNode will get offset, Span, Range(LSP)
//!
//! [`.offset()`]
//! [`.span()`]
//! [`.range()`]

#[derive(PartialEq, Eq, Clone, Copy, Hash, Default, Debug)]
pub(crate) struct Location {
    offsets: usize,
    /// start character offset of the SyntaxNode
    line: u32,
    /// end character offset of the SyntaxNode
    character: u32,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ offsets: {}, position: ( line: {}, col: {} ) ] ]",
            self.offsets, self.line, self.character,
        )
    }
}

#[macro_export]
macro_rules! location {
    () => {
        Location::detached()
    };
    ($offset:expr, $line:expr, $character:expr) => {
        Location::new($offset, $line, $character)
    };
}

#[allow(dead_code)]
impl Location {
    pub fn new(offsets: usize, line: u32, col: u32) -> Self {
        Self {
            offsets,
            line,
            character: col,
        }
    }

    pub fn offsets(&self) -> usize {
        self.offsets
    }

    pub fn line(&self) -> u32 {
        self.line
    }
    pub fn character(&self) -> u32 {
        self.character
    }

    pub const fn detached() -> Location {
        Location {
            offsets: 0,
            line: 0,
            character: 0,
        }
    }
    pub(crate) fn bump_offset(&mut self, count: usize) {
        self.offsets += count;
    }
    pub(crate) fn bump_line(&mut self, count: u32) {
        self.line += count;
        self.set_col(0);
    }
    pub(crate) fn bump_col(&mut self, count: u32) {
        self.character += count;
    }

    pub(crate) fn set_line(&mut self, line: u32) {
        self.line = line;
    }

    pub(crate) fn set_col(&mut self, col: u32) {
        self.character = col;
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct Span {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl Span {
    pub(crate) fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub(crate) const fn detached() -> Span {
        Span { start: 0, end: 0 }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::detached()
    }
}

#[macro_export]
macro_rules! span {
    () => {
        Span::detached()
    };
    ($start:expr, $end:expr) => {
        Span {
            start: $start,
            end: $end,
        }
    };
}

#[cfg(feature = "tower-lsp")]
pub mod lsp {

    use ropey::Rope;
    use tower_lsp::lsp_types::Position;

    pub fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
        let line = rope.try_char_to_line(offset).ok()?;
        let first_char_of_line = rope.try_line_to_char(line).ok()?;
        let column = offset - first_char_of_line;
        Some(Position::new(line as u32, column as u32))
    }

    pub fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
        let line_char_offset = rope.try_line_to_char(position.line as usize).ok()?;
        let slice = rope.slice(0..line_char_offset + position.character as usize);
        Some(slice.len_bytes())
    }
}
