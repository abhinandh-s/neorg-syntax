use std::sync::Arc;

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


pub fn extract_text_from_emph(emph: &InnerNode) -> String {
    emph.children.iter().filter_map(|el| {
        if let SyntaxElement::Leaf(LeafNode { token }) = el {
            if token.kind() == SyntaxKind::Text {
                Some(token.text().to_string())
            } else {
                None
            }
        } else {
            None
        }
    }).collect::<Vec<_>>().join(" ")
}
