use crate::{
    ast::{ScopeId, SymbolId},
    semantic::types::Type,
};
use std::{cell::RefCell, collections::HashMap};

pub type ScopeTableData = HashMap<ScopeId, ScopeEntry>;

pub const GLOBAL_SCOPE_ID: ScopeId = 1;

#[derive(Debug)]
pub struct ScopeTable {
    table: RefCell<ScopeTableData>,
}

impl ScopeTable {
    pub fn new() -> Self {
        let table = HashMap::from([(GLOBAL_SCOPE_ID, ScopeEntry::new_global())]);

        Self {
            table: RefCell::new(table),
        }
    }

    pub fn get(&self, id: ScopeId) -> ScopeEntry {
        self.table.borrow().get(&id).unwrap().clone()
    }

    pub fn add(&self, id: ScopeId, variant: ScopeVariant, parent_id: ScopeId) {
        let mut table = self.table.borrow_mut();

        // Insert the new child data
        table.insert(id, ScopeEntry::new(variant, parent_id));

        // Update parent scope data
        table.get_mut(&parent_id).unwrap().children_id.push(id);
    }

    pub fn has_sym(&self, id: ScopeId, sym: &str) -> bool {
        let scope = self.get(id);
        scope.symbols.contains_key(sym)
    }

    pub fn add_sym(&self, id: SymbolId, sym: &str, scope_id: ScopeId) {
        let mut table = self.table.borrow_mut();
        let scope = table.get_mut(&scope_id).unwrap();
        scope.symbols.insert(String::from(sym), id);
    }
}

#[derive(Clone, Debug)]
pub struct ScopeEntry {
    pub variant: ScopeVariant,
    pub symbols: HashMap<String, SymbolId>,
    pub parent_id: Option<ScopeId>,
    children_id: Vec<ScopeId>,
}

impl ScopeEntry {
    fn new(variant: ScopeVariant, parent_id: ScopeId) -> Self {
        Self {
            variant,
            symbols: HashMap::new(),
            parent_id: Some(parent_id),
            children_id: vec![],
        }
    }

    fn new_global() -> Self {
        Self {
            variant: ScopeVariant::Global,
            symbols: HashMap::new(),
            parent_id: None,
            children_id: vec![],
        }
    }
}

#[derive(Clone, Debug)]
pub enum ScopeVariant {
    Global,
    Function { return_t: Type },
    Conditional,
    Loop,
}

impl ScopeVariant {
    pub const fn is_function(&self) -> bool {
        matches!(self, Self::Function { .. })
    }

    pub const fn is_loop(&self) -> bool {
        matches!(self, ScopeVariant::Loop)
    }

    pub fn unwrap_function_return_t(&self) -> Type {
        match self {
            Self::Function { return_t } => return_t.clone(),
            _ => unreachable!(),
        }
    }
}
