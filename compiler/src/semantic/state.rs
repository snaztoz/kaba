use super::{
    error::{Error, Result},
    types::Type,
};
use scope::{Scope, ScopeVariant, WeakScopeRef};
use std::{cell::RefCell, rc::Rc};
use symtable::SymTable;

pub mod scope;
pub mod symtable;

/// Contains data that is shared across multiple analyzers.
pub struct SharedState {
    st: SymTable,

    // Current active scope
    current_scope: RefCell<WeakScopeRef>,
}

impl SharedState {
    /// Creates a new [`SharedState`] instance.
    pub fn new() -> Self {
        let st = SymTable::new();

        // Initialize current active scope to global
        let current_scope = Rc::downgrade(&st.root.as_ref().borrow().children[0]);

        Self {
            st,
            current_scope: RefCell::new(current_scope),
        }
    }

    pub fn take_st(self) -> SymTable {
        self.st
    }

    /// Get the type associated with a `sym` in the current active scope or its
    /// ancestors.
    pub fn get_sym_t(&self, sym: &str) -> Option<Type> {
        let current_scope = self.current_scope.borrow().upgrade().unwrap();
        SymTable::get_sym_t(sym, &current_scope)
    }

    /// Check if a type exists in the current active scope or its ancestors.
    pub fn has_t(&self, t: &Type) -> bool {
        if let Type::Callable { params_t, return_t } = t {
            return params_t.iter().all(|t| self.has_t(t)) && self.has_t(return_t);
        }

        if let Type::Array { elem_t } = t {
            return self.has_t(elem_t.as_ref().unwrap());
        }

        let current_scope = self.current_scope.borrow().upgrade().unwrap();
        SymTable::has_t(t, &current_scope)
    }

    /// Get the return type of nearest function ancestor.
    ///
    /// Returns `None` if no function can be found.
    pub fn nearest_return_t(&self) -> Option<Type> {
        let current_scope = self.current_scope.borrow().upgrade().unwrap();
        SymTable::nearest_return_t(&current_scope)
    }

    /// Check if the current active scope is a loop or contained within a loop
    /// scope ancestor.
    pub fn is_inside_loop(&self) -> bool {
        let current_scope = self.current_scope.borrow().upgrade().unwrap();
        SymTable::is_inside_loop(&current_scope)
    }

    /// Save symbol and its associated type to current active scope.
    ///
    /// If symbol is already exist in the scope, it will run the `err` closure.
    pub fn save_sym_or_else<F>(&self, sym: &str, t: Type, err: F) -> Result<()>
    where
        F: FnOnce() -> Error,
    {
        let current_scope = self.current_scope.borrow().upgrade().unwrap();
        let mut scope = current_scope.as_ref().borrow_mut();

        if scope.has_sym(sym) {
            return Err(err());
        }

        scope.save_sym(sym, t);

        Ok(())
    }

    /// Execute `action` within a scope of `variant` variant.
    ///
    /// It automatically handles the creating and exiting of the new scope.
    pub fn with_scope<U, F>(&self, variant: ScopeVariant, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        let parent = self.current_scope.borrow().upgrade().unwrap();
        let child = Scope::new(variant);

        SymTable::add_scope(&parent, child.clone());

        // Entering child scope
        *self.current_scope.borrow_mut() = Rc::downgrade(&child);

        let result = action();

        // Exiting child scope
        *self.current_scope.borrow_mut() = Rc::downgrade(&parent);

        result
    }
}
