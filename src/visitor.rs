use std::sync::Arc;

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

use crate::*;

pub fn collect_emph_nodes(node: &InnerNode, output: &mut Vec<Arc<InnerNode>>) {
    if node.kind == SyntaxKind::Emph {
        output.push(Arc::new(node.clone()));
    }

    for child in &node.children {
        match child {
            SyntaxElement::Inner(inner) => {
                collect_emph_nodes(inner, output);
            }
            _ => {} // ignore leaves and errors
        }
    }
}

pub fn collect_error_nodes(node: &InnerNode, output: &mut Vec<Arc<ErrorNode>>) {
    for child in &node.children {
        match child {
            SyntaxElement::Error(err) => {
                output.push(err.clone());
            }
            SyntaxElement::Inner(inner) => {
                collect_error_nodes(inner, output); // recurse
            }
            SyntaxElement::Leaf(_) => {
                // Do nothing
            }
        }
    }
}

/// Convert byte offset to LSP `Position`
pub fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut col = 0;
    let mut current_offset = 0;

    for c in text.chars() {
        if current_offset >= offset {
            break;
        }

        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }

        current_offset += c.len_utf8();
    }

    Position {
        line: line as u32,
        character: col as u32,
    }
}

/// Collect LSP ranges from all ErrorNodes recursively in the syntax tree.
pub fn collect_error_ranges(node: &InnerNode, source_text: &str, output: &mut Vec<Diagnostic>) {
    for child in &node.children {
        match child {
            SyntaxElement::Error(err) => {
                let start = offset_to_position(source_text, err.span);
                let end = offset_to_position(source_text, err.span + 1); // or smarter
                let diagnostic = tower_lsp::lsp_types::Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("here".into()),
                    message: "this".into(),
                    related_information: None,
                    ..Default::default()
                };
                output.push(diagnostic);
            }
            SyntaxElement::Inner(inner) => {
                collect_error_ranges(inner, source_text, output);
            }
            SyntaxElement::Leaf(_) => {}
        }
    }
}

pub fn extract_text_from_emph(emph: &InnerNode) -> String {
    emph.children
        .iter()
        .filter_map(|el| {
            if let SyntaxElement::Leaf(LeafNode { token }) = el {
                if token.kind() == SyntaxKind::Word {
                    Some(token.text().to_string())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}
