use super::stream::TokenStream;
use crate::ast::SymbolId;
use std::cell::RefCell;

pub struct ParserState<'a> {
    pub tokens: &'a TokenStream,
    symbol_id_gen: RefCell<IdGenerator>,
    scope_id_gen: RefCell<IdGenerator>,
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a TokenStream) -> Self {
        Self {
            tokens,
            symbol_id_gen: RefCell::new(IdGenerator::new()),
            scope_id_gen: RefCell::new(IdGenerator::new()),
        }
    }
}

impl ParserState<'_> {
    pub fn next_symbol_id(&self) -> SymbolId {
        self.symbol_id_gen.borrow_mut().next().unwrap()
    }

    pub fn next_scope_id(&self) -> SymbolId {
        self.scope_id_gen.borrow_mut().next().unwrap()
    }
}

/// Implements the [`Iterator`] trait to generate a new ID.
pub struct IdGenerator {
    current: SymbolId,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self { current: 0 }
    }
}

impl Iterator for IdGenerator {
    type Item = SymbolId;

    fn next(&mut self) -> Option<Self::Item> {
        self.current += 1;
        Some(self.current)
    }
}
