//! # SemanticTokenTypes
//!
//! this module exports only one thing
//!
//! const LEGEND_TYPE

#[macro_export]
macro_rules! SemanticTokenType {
    ($tag:literal) => {
        tower_lsp::lsp_types::SemanticTokenType::new($tag)
    };
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
    "neorg.text",
    "neorg.heading",
    "neorg.quote",
    "neorg.bold",
    "neorg.italic",
    "neorg.underline",
    "neorg.strikethrough",
    "neorg.spoiler",
    "neorg.superscript",
    "neorg.subscript",
    "neorg.inlinecode",
    "neorg.nullmodifier",
    "neorg.inlinemath",
    "neorg.variable",
}
