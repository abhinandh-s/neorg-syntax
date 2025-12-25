#[cfg(feature = "tower-lsp")]
pub(crate) use tower_lsp::lsp_types as lsp;

#[cfg(all(not(feature = "tower-lsp"), feature = "lsp-types"))]
pub(crate) use lsp_types as lsp;

#[macro_export]
macro_rules! semtype {
    ($tag:literal) => {
        lsp::SemanticTokenType::new($tag)
    };
}

pub const LEGEND_TYPE: &[lsp::SemanticTokenType] = &[
    lsp::SemanticTokenType::new("neorg.text"),   // 0
    lsp::SemanticTokenType::VARIABLE,             // 1
    lsp::SemanticTokenType::STRING,               // 2
    lsp::SemanticTokenType::COMMENT,              // 3
    lsp::SemanticTokenType::NUMBER,               // 4
    lsp::SemanticTokenType::KEYWORD,              // 5
    lsp::SemanticTokenType::OPERATOR,             // 6
    lsp::SemanticTokenType::PARAMETER,            // 7
    lsp::SemanticTokenType::new("neorg.quote"),   // 8
    lsp::SemanticTokenType::new("neorg.heading"), // 9
    lsp::SemanticTokenType::new("neorg.italics"), // 10
    lsp::SemanticTokenType::new("neorg.bold"),    // 11
    lsp::SemanticTokenType::FUNCTION,             // 12
];
