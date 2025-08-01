#![allow(dead_code, unused)]

use crate::{SyntaxKind, SyntaxNode};

#[cfg(feature = "tower-lsp")]
pub(crate) use tower_lsp::lsp_types as lsp;

#[cfg(all(not(feature = "tower-lsp"), feature = "lsp-types"))]
pub(crate) use lsp_types as lsp;

pub struct Highlight {
    source: Vec<SyntaxNode>,
    //  line: u32,
    //  char: u32,
    result: lsp::SemanticTokens,
}

impl Highlight {
    pub fn new(source: SyntaxNode) -> Self {
        Self {
            source: source.flatten(),
            result: lsp::SemanticTokens {
                result_id: None,
                data: Vec::new(),
            },
        }
    }

    fn push(&mut self, tok: lsp::SemanticToken) {
        self.result.data.push(tok);
    }

    pub fn get(&mut self) -> &lsp::SemanticTokens {
        let mut length = 0;
        let mut token_type = 0;
        let mut delta_line = 0;
        let mut delta_start = 0;
        for node in self.source.clone() {
            let ln_start = node.start_position().line;
            let ch_start = node.start_position().character;

            // delta_line = ln_start - delta_line;
            // delta_start = ch_start - delta_line;
            length = node.len_utf16() as u32;
            let lat = node.children().last().map_or(0, |f| f.len_utf16());
            match node.kind() {
                SyntaxKind::Quote => token_type = 8,
                SyntaxKind::Heading => token_type = 9,
                _ => (),
            }
            // everything is u32
            self.push(lsp::SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type,                // quote
                token_modifiers_bitset: 0, // ???
            });
        }
        &self.result
    }
}

macro_rules! semtype {
    ($tag:literal) => {
        lsp::SemanticTokenType::new($tag)
    };
}

pub const LEGEND_TYPE: &[lsp::SemanticTokenType] = &[
    lsp::SemanticTokenType::FUNCTION,
    lsp::SemanticTokenType::VARIABLE,
    lsp::SemanticTokenType::STRING,
    lsp::SemanticTokenType::COMMENT,
    lsp::SemanticTokenType::NUMBER,
    lsp::SemanticTokenType::KEYWORD,
    lsp::SemanticTokenType::OPERATOR,
    lsp::SemanticTokenType::PARAMETER,
    lsp::SemanticTokenType::new("neorg.quote"),
    lsp::SemanticTokenType::new("neorg.heading"),
    lsp::SemanticTokenType::new("neorg.heading"),
];
