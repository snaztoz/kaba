use crate::{ast::NodeId, semantic::types::Type};
use std::{cell::RefCell, collections::HashMap};

pub type SymbolTableData = HashMap<NodeId, SymbolEntry>;

pub struct SymbolTable {
    table: RefCell<SymbolTableData>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: RefCell::new(HashMap::new()),
        }
    }

    pub fn take(self) -> SymbolTableData {
        self.table.take()
    }

    pub fn get_t(&self, id: NodeId) -> Option<SymbolType> {
        self.table
            .borrow()
            .get(&id)
            .map(|symbol_entry| symbol_entry.t.clone())
    }

    pub fn add_entity(&self, id: NodeId, sym: &str, t: Type) {
        self.table.borrow_mut().insert(
            id,
            SymbolEntry {
                name: sym.to_string(),
                t: SymbolType::Entity(t),
            },
        );
    }
}

#[derive(Clone, Debug)]
pub struct SymbolEntry {
    pub name: String,
    pub t: SymbolType,
}

#[derive(Clone, Debug)]
pub enum SymbolType {
    TypeDefinition(Type),
    Entity(Type),
}

impl SymbolType {
    pub fn unwrap_entity(self) -> Type {
        if let Self::Entity(ent) = self {
            ent
        } else {
            unreachable!()
        }
    }
}
