use super::stream::TokenStream;
use crate::ast::NodeId;
use std::cell::RefCell;

pub struct ParserState<'src, 'a> {
    pub tokens: &'a TokenStream<'src>,
    id_gen: RefCell<IdGenerator>,
}

impl<'src, 'a> ParserState<'src, 'a> {
    pub fn new(tokens: &'a TokenStream<'src>) -> Self {
        Self {
            tokens,
            id_gen: RefCell::new(IdGenerator::new()),
        }
    }
}

impl ParserState<'_, '_> {
    pub fn next_id(&self) -> NodeId {
        self.id_gen.borrow_mut().next().unwrap()
    }
}

/// Implements the [`Iterator`] trait to generate a new ID.
pub struct IdGenerator {
    current: NodeId,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self { current: 0 }
    }
}

impl Iterator for IdGenerator {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.current += 1;
        Some(self.current)
    }
}
