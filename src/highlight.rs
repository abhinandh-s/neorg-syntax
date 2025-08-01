use crate::{SyntaxKind, SyntaxNode};

#[cfg(feature = "tower-lsp")]
pub(crate) use tower_lsp::lsp_types as lsp;

#[cfg(all(not(feature = "tower-lsp"), feature = "lsp-types"))]
pub(crate) use lsp_types as lsp;

pub struct Highlight {
    source: Vec<SyntaxNode>,
    length: u32,
    delta_line: u32,
    delta_start: u32,
    token_type: u32,
    result: lsp::SemanticTokens,
}

impl Highlight {
    pub fn new(source: SyntaxNode) -> Self {
        Self {
            source: source.flatten(false),
            length: 0,
            delta_line: 0,
            delta_start: 0,
            token_type: 0,
            result: lsp::SemanticTokens {
                result_id: None,
                data: Vec::new(),
            },
        }
    }

    // covert lsp Range to delta
    // Range {
    //  start: Position {
    //      line: u32,
    //      character: u32,
    //  },
    //  end: Position {
    //      line: u32,
    //      character: u32,
    //  }
    // }

    fn push(&mut self, n: u32, node: SyntaxNode) {
        self.set_length(self.length().saturating_add(node.len_utf16() as u32));
        let line_start = node.start_position().line.saturating_sub(1);
        println!("{line_start}");
        let length = node.len_utf16() as u32;
        self.set_delta_line(line_start.saturating_sub(self.delta_line()));
        self.result.data.push(lsp::SemanticToken {
            delta_line: self.delta_line(),
            delta_start: self.delta_start(),
            length,
            token_type: n,
            token_modifiers_bitset: 0,
        });
    }
    pub fn get_nodes(&self) -> Vec<SyntaxNode> {
        let mut result = Vec::new();
        for node in self.source.clone() {
            match node.kind() {
                SyntaxKind::Heading => result.push(node),
                SyntaxKind::NullModifier => result.push(node),
                _ => (),
            }
        }
        result
    }
    pub fn get(&mut self) -> &lsp::SemanticTokens {
        for node in self.get_nodes() {
            match node.kind() {
                SyntaxKind::Heading => self.push(9, node),
                SyntaxKind::NullModifier => self.push(3, node),
                _ => (),
            }
            self.reset_token_type();
        }
        &self.result
    }

    pub fn set_delta_line(&mut self, delta_line: u32) {
        self.delta_line = delta_line;
    }

    pub fn set_delta_start(&mut self, delta_start: u32) {
        self.delta_start = delta_start;
    }

    pub fn delta_line(&self) -> u32 {
        self.delta_line
    }

    pub fn delta_start(&self) -> u32 {
        self.delta_start
    }

    pub fn length(&self) -> u32 {
        self.length
    }

    pub fn set_length(&mut self, length: u32) {
        self.length = length;
    }

    pub fn set_token_type(&mut self, token_type: u32) {
        self.token_type = token_type;
    }

    pub fn reset_token_type(&mut self) {
        self.token_type = 0;
    }

    pub fn token_type(&self) -> u32 {
        self.token_type
    }
}

#[macro_export]
macro_rules! semtype {
    ($tag:literal) => {
        lsp::SemanticTokenType::new($tag)
    };
}

pub const LEGEND_TYPE: &[lsp::SemanticTokenType] = &[
    lsp::SemanticTokenType::FUNCTION,             // 0
    lsp::SemanticTokenType::VARIABLE,             // 1
    lsp::SemanticTokenType::STRING,               // 2
    lsp::SemanticTokenType::COMMENT,              // 3
    lsp::SemanticTokenType::NUMBER,               // 5
    lsp::SemanticTokenType::KEYWORD,              // 5
    lsp::SemanticTokenType::OPERATOR,             // 6
    lsp::SemanticTokenType::PARAMETER,            // 7
    lsp::SemanticTokenType::new("neorg.quote"),   // 8
    lsp::SemanticTokenType::new("neorg.heading"), // 9
    lsp::SemanticTokenType::new("neorg.italics"), // 10
    lsp::SemanticTokenType::new("neorg.bold"),    // 11
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semantic_token_encoding_and_delta_edit() {
        let source = "this is a test for delta calculation";
        let mut p = crate::Parser::new(source);

        let cst = crate::document(&mut p);

        let mut res = Highlight::new(cst);
        let _s = res.get();
    }
}
