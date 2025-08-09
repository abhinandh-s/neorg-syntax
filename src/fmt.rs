use crate::{get_kinds, SyntaxKind, SyntaxNode};

pub fn fmt_list(node: &mut crate::SyntaxNode) {
    let n = get_kinds(crate::SyntaxKind::UnOrderedList, node);
    for mut i in n {
        println!(">> In Ord List");
        for child in i.children_mut() {
            if child.kind() == SyntaxKind::WhiteSpace {
                println!(">> Is WhiteSpace");
                child.convert_text("###");
            }
        }
    }
}
