//! # Span
//!
//! span is the devel
//!
//! should i just give offset 
//! or should i just give span [start_offset, end_offset]
//! or should i give LSP like Range [ start: {line, col}, end: {line, col} ]
//! 
//! Token will have just offset
//! SyntaxNode will get offset, Span, Range(LSP)

use std::fmt::Display;

#[derive(PartialEq, Eq, Clone, Copy, Hash, Default, Debug)]
pub struct Location {
    pub offsets: (usize, usize),
    pub start: Position,
    pub end: Position,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ offsets: {}..{}, start ( line: {}, col: {} ) ], end ( line: {}, col: {} ) ]",
            self.offsets.0,
            self.offsets.1,
            self.start.line,
            self.start.col,
            self.end.line,
            self.end.col
        )
    }
}

#[macro_export]
macro_rules! location {
    ($offset:expr, $line:expr, $col:expr) => {
        Location::new($offset, $line, $col)
    };
}

impl Location {
    pub fn new(offsets: (usize, usize), start: Position, end: Position) -> Self {
        Self {
            offsets,
            start,
            end,
        }
    }

    pub const fn detached() -> Location {
        Location {
            offsets: (0, 0),
            start: crate::position!(),
            end: crate::position!(),
        }
    }
}

#[cfg(feature = "tower_lsp")]
impl From<Location> for tower_lsp::lsp_types::Range {
    fn from(val: Location) -> Self {
        tower_lsp::lsp_types::Range {
            start: tower_lsp::lsp_types::Position {
                line: val.start.line as u32,
                character: val.start.col as u32,
            },
            end: tower_lsp::lsp_types::Position {
                line: val.end.line as u32,
                character: val.start.line as u32,
            },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Position {
    /// start character offset of the SyntaxNode
    pub line: usize,
    /// end character offset of the SyntaxNode
    pub col: usize,
}

#[macro_export]
macro_rules! position {
    () => {
        Position::detached()
    };
    ($start:expr, $end:expr) => {
        Position {
            line: $start,
            col: $end,
        }
    };
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ span: {}..{} ]", self.line, self.col)
    }
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ span: {}..{} ]", self.line, self.col)
    }
}

impl Position {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            line: start,
            col: end,
        }
    }

    pub fn start(&self) -> usize {
        self.line
    }

    pub fn end(&self) -> usize {
        self.col
    }
    pub const fn detached() -> Position {
        Self { line: 1, col: 1 }
    }
}

impl Default for Position {
    fn default() -> Self {
        Position::detached()
    }
}

#[cfg(feature = "tower_lsp")]
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
