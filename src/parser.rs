#![allow(unused)]

use std::fmt::{self, Debug, Display, Formatter};
use std::ops::{Deref, Range};
use std::rc::Rc;
use std::sync::Arc;

use ecow::{EcoString, EcoVec, eco_format, eco_vec};

use crate::kind::SyntaxKind;
use crate::node::*;
use crate::span::Span;

use crate::lexer::{Lexer, Token};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(source: impl Into<String>) -> Self {
        let mut lexer = Lexer::new(source.into().into());
        let tokens = lexer.lex();
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> SyntaxNode {
        self.parse_document()
    }

    fn parse_document(&mut self) -> SyntaxNode {
        let mut children = Vec::new();

        while !self.is_at_end() {
            match self.peek().kind {
                SyntaxKind::NewLine => {
                    children.push(self.make_leaf_node());
                }
                SyntaxKind::WhiteSpace => {
                    children.push(self.make_leaf_node());
                }
                SyntaxKind::Astrisk => {
                    children.push(self.parse_heading());
                }
                SyntaxKind::Hyphen => {
                    children.push(self.parse_list_item());
                }
                SyntaxKind::At => {
                    children.push(self.parse_attribute());
                }
                _ => {
                    children.push(self.parse_paragraph());
                }
            }
        }

        let len = self.tokens.len();

        SyntaxNode::inner(SyntaxKind::Document, children, Span::new(self.tokens[0].span.start, self.tokens[len - 1].span.end ))
    }

    fn parse_heading(&mut self) -> SyntaxNode {
        let mut text = EcoString::new();
        let start_span  = self.peek().span.start;
        let mut children = Vec::new();
        // Consume the asterisk
        children.push(self.make_leaf_node());

        // Parse the heading content
        while !self.is_at_end() && self.peek().kind != SyntaxKind::NewLine {
            text.push_str(&self.peek().text);
            match self.peek().kind {
                SyntaxKind::WhiteSpace => {
                    children.push(self.make_leaf_node());
                }
                SyntaxKind::Text => {
                    children.push(self.make_leaf_node());
                }
                unexpected => {
                    let error = SyntaxError::new(eco_format!(
                        "Unexpected token in heading: {:?}",
                        unexpected
                    ));
                    children.push(SyntaxNode::error(error, self.peek().text.clone()));
                    self.advance();
                }
            }
        }

        // Consume the newline if present
        if !self.is_at_end() && self.peek().kind == SyntaxKind::NewLine {
            children.push(self.make_leaf_node());
        }
        // println!("{:?}", text);
        SyntaxNode::inner(SyntaxKind::Heading, children, Span::new(start_span, self.previous().span.end))
    }

    fn parse_list_item(&mut self) -> SyntaxNode {
        let mut text = EcoString::new();
        let mut children = Vec::new();

        // Consume the hyphen
        children.push(self.make_leaf_node());

        // Parse the list item content
        while !self.is_at_end() && self.peek().kind != SyntaxKind::NewLine {
            text.push_str(&self.peek().text);

            match self.peek().kind {
                SyntaxKind::WhiteSpace => {
                    children.push(self.make_leaf_node());
                }
                SyntaxKind::Text => {
                    children.push(self.make_leaf_node());
                }
                unexpected => {
                    let error = SyntaxError::new(eco_format!(
                        "Unexpected token in list item: {:?}",
                        unexpected
                    ));
                    children.push(SyntaxNode::error(error, self.peek().text.clone()));
                    self.advance();
                }
            }
        }

        // Consume the newline if present
        if !self.is_at_end() && self.peek().kind == SyntaxKind::NewLine {
            children.push(self.make_leaf_node());
        }
        // println!("{:?}", text);

        SyntaxNode::inner(SyntaxKind::ListItem, children, Span::default())
    }

    fn parse_attribute(&mut self) -> SyntaxNode {
        let mut children = Vec::new();

        // Consume the @
        children.push(self.make_leaf_node());

        // Parse the attribute content
        while !self.is_at_end() && self.peek().kind != SyntaxKind::NewLine {
            match self.peek().kind {
                SyntaxKind::WhiteSpace => {
                    children.push(self.make_leaf_node());
                }
                SyntaxKind::Text => {
                    children.push(self.make_leaf_node());
                }
                unexpected => {
                    let error = SyntaxError::new(eco_format!(
                        "Unexpected token in attribute: {:?}",
                        unexpected
                    ));
                    children.push(SyntaxNode::error(error, self.peek().text.clone()));
                    self.advance();
                }
            }
        }

        // Consume the newline if present
        if !self.is_at_end() && self.peek().kind == SyntaxKind::NewLine {
            children.push(self.make_leaf_node());
        }

        SyntaxNode::inner(SyntaxKind::Attribute, children, Span::default())
    }

    fn parse_paragraph(&mut self) -> SyntaxNode {
        let mut children = Vec::new();

        while !self.is_at_end() && self.peek().kind != SyntaxKind::NewLine {
            match self.peek().kind {
                SyntaxKind::Text | SyntaxKind::WhiteSpace => {
                    children.push(self.make_leaf_node());
                }
                SyntaxKind::Underscore | SyntaxKind::Astrisk | SyntaxKind::Tilda => {
                    children.push(self.parse_inline_markup());
                }
                unexpected => {
                    let error = SyntaxError::new(eco_format!(
                        "Unexpected token in paragraph: {:?}",
                        unexpected
                    ));
                    children.push(SyntaxNode::error(error, self.peek().text.clone()));
                    self.advance();
                }
            }
        }

        // Consume the newline if present
        if !self.is_at_end() && self.peek().kind == SyntaxKind::NewLine {
            children.push(self.make_leaf_node());
        }

        SyntaxNode::inner(SyntaxKind::Paragraph, children, Span::default())
    }

    fn parse_inline_markup(&mut self) -> SyntaxNode {
        let mut children = Vec::new();
        let marker = self.peek().kind;

        // Consume the opening marker
        children.push(self.make_leaf_node());

        while !self.is_at_end() {
            match self.peek().kind {
                kind if kind == marker => {
                    children.push(self.make_leaf_node());
                    break;
                }
                SyntaxKind::Text | SyntaxKind::WhiteSpace => {
                    children.push(self.make_leaf_node());
                }
                SyntaxKind::NewLine => {
                    let error = SyntaxError::new("Unclosed inline markup");
                    children.push(SyntaxNode::error(error, self.peek().text.clone()));
                    break;
                }
                _ => {
                    children.push(self.make_leaf_node());
                }
            }
        }

        let kind = match marker {
            SyntaxKind::Underscore => SyntaxKind::Italic,
            SyntaxKind::Astrisk => SyntaxKind::Bold,
            SyntaxKind::Tilda => SyntaxKind::Strikethrough,
            _ => unreachable!(),
        };

        SyntaxNode::inner(kind, children, Span::default())
    }

    fn make_leaf_node(&mut self) -> SyntaxNode {
        let token = self.advance();
        SyntaxNode::leaf(token.kind, token.text, token.span)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().kind == SyntaxKind::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current.min(self.tokens.len() - 1)]
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens[self.current - 1].clone()
    }

     fn previous(&mut self) -> Token {
        if !self.is_at_end() {
            self.current -= 1;
        }
        self.tokens[self.current - 1].clone()
    }
}
