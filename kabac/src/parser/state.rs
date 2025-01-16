use super::stream::TokenStream;
use crate::ast::SymbolId;
use std::cell::RefCell;

pub struct ParserState<'a> {
    pub tokens: &'a TokenStream,
    id_gen: RefCell<SymbolIdGenerator>,
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a TokenStream) -> Self {
        Self {
            tokens,
            id_gen: RefCell::new(SymbolIdGenerator::new()),
        }
    }
}

impl ParserState<'_> {
    pub fn next_id(&self) -> SymbolId {
        self.id_gen.borrow_mut().next().unwrap()
    }
}

/// Implements the [`Iterator`] trait to generate a new symbol ID.
pub struct SymbolIdGenerator {
    current: SymbolId,
}

impl SymbolIdGenerator {
    pub fn new() -> Self {
        Self { current: 0 }
    }
}

impl Iterator for SymbolIdGenerator {
    type Item = SymbolId;

    fn next(&mut self) -> Option<Self::Item> {
        self.current += 1;
        Some(self.current)
    }
}
