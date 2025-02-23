use crate::span::Span;

#[derive(Debug, PartialEq)]
pub enum Node {
    Document(Vec<Node>),
    Heading {
        level: u32,
        content: Vec<Node>,
    },
    UnorderedList {
        level: u32,
        items: Vec<Node>,
    },
    OrderedList {
        level: u32,
        items: Vec<Node>,
    },
    ListItem(Vec<Node>),
    CodeBlock {
        language: Option<String>,
        content: String,
    },
    Text(String),
    Bold(Vec<Node>),
    Italic(Vec<Node>),
    Underline(Vec<Node>),
    Strikethrough(Vec<Node>),
    LineBreak,
    Error(String, Span),
}

impl Node {
    pub fn add_child(&mut self, node: Node) {
        match self {
            Node::Document(children) => children.push(node),
            Node::Heading { content, .. } => content.push(node),
            Node::Bold(content) => content.push(node),
            Node::Italic(content) => content.push(node),
            Node::Underline(content) => content.push(node),
            Node::Strikethrough(content) => content.push(node),
            Node::ListItem(content) => content.push(node),
            _ => {}
        }
    }
}
