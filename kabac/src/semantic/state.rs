use super::{
    error::{Error, Result},
    types::Type,
};
use crate::ast::{ScopeId, SymbolId};
use std::{cell::RefCell, collections::HashMap};

pub const GLOBAL_SCOPE_ID: ScopeId = 1;

pub type SymbolTableData = HashMap<SymbolId, SymbolType>;
type ScopeTableData = HashMap<ScopeId, ScopeEntry>;

pub struct AnalyzerState {
    symbols: SymbolTable,
    scopes: ScopeTable,
    current_scope_id: RefCell<ScopeId>,
}

impl AnalyzerState {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            scopes: ScopeTable::new(),
            current_scope_id: RefCell::new(GLOBAL_SCOPE_ID),
        }
    }

    pub fn take_symbols(self) -> SymbolTableData {
        self.symbols.take()
    }

    /// Get the type associated with a `sym` in the current active scope or its
    /// ancestors.
    pub fn get_sym_t(&self, sym: &str) -> Option<SymbolType> {
        let mut scope_id = *self.current_scope_id.borrow();

        loop {
            let entry = self.scopes.get(scope_id).unwrap();

            if let Some(sym_id) = entry.symbols.get(sym) {
                return self.symbols.get(*sym_id);
            }

            // Traverse upward
            if let Some(id) = entry.parent_id {
                scope_id = id;
            } else {
                return None;
            }
        }
    }

    /// Check if a type exists in the current active scope or its ancestors.
    pub fn has_t(&self, t: &Type) -> bool {
        match t {
            t if t.is_basic_t() => true,

            Type::Callable { params_t, return_t } => {
                params_t.iter().all(|t| self.has_t(t)) && self.has_t(return_t)
            }

            Type::Array { elem_t } => self.has_t(elem_t),

            Type::Symbol(name) => {
                matches!(self.get_sym_t(name), Some(SymbolType::_TypeDefinition(_)))
            }

            _ => unreachable!(),
        }
    }

    /// Get the return type of nearest function ancestor.
    ///
    /// Returns `None` if no function can be found.
    pub fn nearest_return_t(&self) -> Option<Type> {
        let mut scope_id = *self.current_scope_id.borrow();

        loop {
            let entry = self.scopes.get(scope_id).unwrap();

            if let ScopeVariant::Function { return_t } = &entry.variant {
                return Some(return_t.clone());
            }

            // Traverse upward
            if let Some(id) = entry.parent_id {
                scope_id = id;
            } else {
                return None;
            }
        }
    }

    /// Check if the current active scope is a loop or contained within a loop
    /// scope ancestor.
    pub fn is_inside_loop(&self) -> bool {
        let mut scope_id = *self.current_scope_id.borrow();

        loop {
            let entry = self.scopes.get(scope_id).unwrap();

            if let ScopeVariant::Loop = &entry.variant {
                return true;
            }

            // Traverse upward
            if let Some(id) = entry.parent_id {
                scope_id = id;
            } else {
                return false;
            }
        }
    }

    /// Save symbol and its associated type to current active scope.
    ///
    /// If symbol is already exist in the scope, it will run the `err` closure.
    pub fn save_entity_or_else<F>(&self, id: SymbolId, sym: &str, t: Type, err: F) -> Result<()>
    where
        F: FnOnce() -> Error,
    {
        if Type::is_basic_t_str(sym) {
            todo!("handle builtin type as symbol error");
        };

        //
        // Save to scope table
        //

        let scope_id = *self.current_scope_id.borrow();

        if self.scopes.has_sym(scope_id, sym) {
            return Err(err());
        }

        self.scopes.add_sym(id, sym, scope_id);

        //
        // Save to symbol table
        //

        self.symbols.add(id, SymbolType::Entity(t));

        Ok(())
    }

    /// Execute `action` within a scope of `variant` variant.
    ///
    /// It automatically handles the creating and exiting of the new scope.
    pub fn with_scope<U, F>(&self, id: ScopeId, variant: ScopeVariant, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        let parent_id = *self.current_scope_id.borrow();

        self.scopes.add(id, variant, parent_id);

        // Entering child scope
        *self.current_scope_id.borrow_mut() = id;

        let result = action();

        // Exiting child scope
        *self.current_scope_id.borrow_mut() = parent_id;

        result
    }
}

pub struct SymbolTable {
    table: RefCell<SymbolTableData>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            table: RefCell::new(HashMap::new()),
        }
    }

    fn take(self) -> SymbolTableData {
        self.table.take()
    }

    fn get(&self, id: SymbolId) -> Option<SymbolType> {
        self.table.borrow().get(&id).cloned()
    }

    fn add(&self, id: SymbolId, t: SymbolType) {
        self.table.borrow_mut().insert(id, t);
    }
}

#[derive(Clone, Debug)]
pub enum SymbolType {
    _TypeDefinition(Type),
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

#[derive(Debug)]
struct ScopeTable {
    table: RefCell<ScopeTableData>,
}

impl ScopeTable {
    fn new() -> Self {
        let mut table = HashMap::new();

        table.insert(
            GLOBAL_SCOPE_ID,
            ScopeEntry {
                variant: ScopeVariant::Global,
                symbols: HashMap::new(),
                parent_id: None,
                children_id: vec![],
            },
        );

        Self {
            table: RefCell::new(table),
        }
    }

    fn get(&self, id: ScopeId) -> Option<ScopeEntry> {
        self.table.borrow().get(&id).cloned()
    }

    fn add(&self, id: ScopeId, variant: ScopeVariant, parent_id: ScopeId) {
        let mut table = self.table.borrow_mut();

        // Insert the new child data
        table.insert(
            id,
            ScopeEntry {
                variant,
                symbols: HashMap::new(),
                parent_id: Some(parent_id),
                children_id: vec![],
            },
        );

        // Update parent scope data
        table.get_mut(&parent_id).unwrap().children_id.push(id);
    }

    fn has_sym(&self, id: ScopeId, sym: &str) -> bool {
        let scope = self.get(id).unwrap();
        scope.symbols.contains_key(sym)
    }

    fn add_sym(&self, id: SymbolId, sym: &str, scope_id: ScopeId) {
        let mut table = self.table.borrow_mut();
        let scope = table.get_mut(&scope_id).unwrap();
        scope.symbols.insert(String::from(sym), id);
    }
}

#[derive(Clone, Debug)]
struct ScopeEntry {
    variant: ScopeVariant,
    symbols: HashMap<String, SymbolId>,
    parent_id: Option<ScopeId>,
    children_id: Vec<ScopeId>,
}

#[derive(Clone, Debug)]
pub enum ScopeVariant {
    Global,
    Function { return_t: Type },
    Conditional,
    Loop,
}
