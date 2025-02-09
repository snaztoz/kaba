use super::types::{Type, BASIC_T};
use crate::ast::{ScopeId, SymbolId};
use scope::{ScopeTable, ScopeVariant, GLOBAL_SCOPE_ID};
use std::cell::RefCell;
use symbol::{SymbolTable, SymbolTableData, SymbolType};

mod scope;
pub mod symbol;

pub struct AnalyzerState {
    symbols: SymbolTable,
    scopes: ScopeTable,
    _current_scope_id: RefCell<ScopeId>,
}

impl AnalyzerState {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            scopes: ScopeTable::new(),
            _current_scope_id: RefCell::new(GLOBAL_SCOPE_ID),
        }
    }

    pub fn take_symbols(self) -> SymbolTableData {
        self.symbols.take()
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
                matches!(self.get_sym_t(name), Some(SymbolType::TypeDefinition(_)))
            }

            _ => unreachable!(),
        }
    }

    /// Get the type associated with a `sym` in the current active scope or its
    /// ancestors.
    pub fn get_sym_t(&self, sym: &str) -> Option<SymbolType> {
        let mut scope_id = self.current_scope_id();

        loop {
            let scope_entry = self.scopes.get(scope_id);

            if let Some(symbol_id) = scope_entry.symbols.get(sym) {
                return self.symbols.get_t(*symbol_id);
            }

            // Traverse upward
            if let Some(id) = scope_entry.parent_id {
                scope_id = id;
            } else {
                return None;
            }
        }
    }

    /// Get the return type of nearest function ancestor.
    ///
    /// Returns `None` if no function can be found.
    pub fn nearest_return_t(&self) -> Option<Type> {
        let mut scope_id = self.current_scope_id();

        loop {
            let scope_entry = self.scopes.get(scope_id);

            if scope_entry.variant.is_function() {
                return Some(scope_entry.variant.unwrap_function_return_t());
            }

            // Traverse upward
            if let Some(id) = scope_entry.parent_id {
                scope_id = id;
            } else {
                return None;
            }
        }
    }

    /// Check if the current active scope is a loop or contained within a loop
    /// scope ancestor.
    pub fn is_inside_loop(&self) -> bool {
        let mut scope_id = self.current_scope_id();

        loop {
            let scope_entry = self.scopes.get(scope_id);

            if scope_entry.variant.is_loop() {
                return true;
            }

            // Traverse upward
            if let Some(id) = scope_entry.parent_id {
                scope_id = id;
            } else {
                return false;
            }
        }
    }

    /// Check if the given `sym` can be saved. A symbol can't be saved if:
    ///
    ///  1. It is a builtin (reserved) name,
    ///  2. or it's already exist in the current scope.
    pub fn can_save_sym(&self, sym: &str) -> bool {
        !BASIC_T.contains(&sym) && !self.scopes.has_sym(self.current_scope_id(), sym)
    }

    /// Save symbol and its associated type to current active scope.
    pub fn save_entity(&self, sym_id: SymbolId, sym: &str, t: Type) {
        // Save to scope table
        self.scopes.add_sym(sym_id, sym, self.current_scope_id());

        // Save to symbol table
        self.symbols.add_entity(sym_id, sym, t);
    }

    /// Execute `action` within a new function scope.
    pub fn with_function_scope<U, F>(&self, scope_id: ScopeId, return_t: Type, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        self.with_scope(scope_id, ScopeVariant::Function { return_t }, action)
    }

    /// Execute `action` within a new conditional scope.
    pub fn with_conditional_scope<U, F>(&self, scope_id: ScopeId, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        self.with_scope(scope_id, ScopeVariant::Conditional, action)
    }

    /// Execute `action` within a new loop scope.
    pub fn with_loop_scope<U, F>(&self, scope_id: ScopeId, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        self.with_scope(scope_id, ScopeVariant::Loop, action)
    }

    /// Execute `action` within a scope of `variant` variant.
    ///
    /// It automatically handles the creating and exiting of the new scope.
    fn with_scope<U, F>(&self, scope_id: ScopeId, scope_variant: ScopeVariant, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        let current_id = self.current_scope_id();

        self.scopes.add(scope_id, scope_variant, current_id);

        self.enter_scope(scope_id);
        let result = action();
        self.enter_scope(current_id);

        result
    }

    fn current_scope_id(&self) -> ScopeId {
        *self._current_scope_id.borrow()
    }

    fn enter_scope(&self, scope_id: ScopeId) {
        *self._current_scope_id.borrow_mut() = scope_id;
    }
}
