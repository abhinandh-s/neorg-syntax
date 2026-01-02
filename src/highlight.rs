//! # SemanticTokenTypes
//!
//! this module exports only one thing
//!
//! const LEGEND_TYPE

#[macro_export]
macro_rules! SemanticTokenType {
    ($tag:literal) => {
        tower_lsp::lsp_types::SemanticTokenType::new($tag)
    }
}

macro_rules! SemanticTokenTypes {
    ($($tag:literal),* $(,)?) => {
        pub const LEGEND_TYPE: &[tower_lsp::lsp_types::SemanticTokenType] = &[
            $(
                tower_lsp::lsp_types::SemanticTokenType::new($tag),
            )*
        ];
    }
}

SemanticTokenTypes! {
    "neorg.text",    // 00
    "neorg.heading", // 01
    "neorg.quote",   // 02
    "neorg.italics", // 04
    "neorg.bold",    // 04
}
