// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use super::{
    error::{Error, Result},
    scope::{Scope, ScopeStack, ScopeType},
    types::Type,
};
use std::cell::RefCell;

#[derive(Default)]
pub struct Context {
    scope_stack: RefCell<ScopeStack>,
}

impl Context {
    pub fn get_symbol_type(&self, name: &str) -> Option<Type> {
        self.scope_stack
            .borrow()
            .stack
            .iter()
            .rev()
            .find_map(|scope| scope.symbols.get(name).cloned())
    }

    pub fn get_current_function_return_type(&self) -> Option<Type> {
        self.scope_stack
            .borrow()
            .stack
            .iter()
            .rev()
            .find_map(|s| match &s.scope_t {
                ScopeType::Function { return_t } => Some(return_t.clone()),
                _ => None,
            })
    }

    pub fn is_inside_loop(&self) -> bool {
        self.scope_stack
            .borrow()
            .stack
            .iter()
            .rev()
            .any(|s| s.scope_t == ScopeType::Loop)
    }

    pub fn save_symbol_or_else<F>(&self, name: &str, sym_t: Type, err: F) -> Result<()>
    where
        F: FnOnce() -> Error,
    {
        let stack = &mut self.scope_stack.borrow_mut().stack;
        let symbols = &mut stack.last_mut().unwrap().symbols;

        if symbols.contains_key(name) {
            return Err(err());
        }

        symbols.insert(String::from(name), sym_t);
        Ok(())
    }

    pub fn with_scope<U, F>(&self, scope: Scope, callback: F) -> U
    where
        F: FnOnce() -> U,
    {
        self.push_scope(scope);
        let result = callback();
        self.pop_scope();
        result
    }

    fn push_scope(&self, scope: Scope) {
        self.scope_stack.borrow_mut().stack.push(scope);
    }

    fn pop_scope(&self) {
        self.scope_stack.borrow_mut().stack.pop();
    }
}
