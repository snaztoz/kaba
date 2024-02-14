// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use super::{
    scope::{Scope, ScopeStack, ScopeType},
    types::Type,
};
use std::cell::RefCell;

#[derive(Default)]
pub struct Context {
    scope_stack: RefCell<ScopeStack>,
}

impl Context {
    pub fn has_symbol(&self, name: &str) -> bool {
        for scope in self.scope_stack.borrow().stack.iter().rev() {
            if scope.symbols.contains_key(name) {
                return true;
            }
        }
        false
    }

    pub fn current_scope_has_symbol(&self, name: &str) -> bool {
        self.scope_stack
            .borrow()
            .stack
            .last()
            .unwrap() // at least builtin and global scope is always exist
            .symbols
            .contains_key(name)
    }

    pub fn global_scope_has_symbol(&self, name: &str) -> bool {
        self.scope_stack
            .borrow()
            .stack
            .iter()
            .find(|scope| scope.scope_type == ScopeType::Global)
            .unwrap() // global scope is always exist
            .symbols
            .contains_key(name)
    }

    pub fn current_scope_is_inside_loop(&self) -> bool {
        self.scope_stack
            .borrow()
            .stack
            .iter()
            .rev()
            .any(|scope| scope.scope_type == ScopeType::Loop)
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

    pub fn get_current_function_return_type(&self) -> Option<Type> {
        self.scope_stack
            .borrow()
            .stack
            .iter()
            .rev()
            .find_map(|scope| match &scope.scope_type {
                ScopeType::Function { return_type } => Some(return_type.clone()),
                _ => None,
            })
    }

    pub fn get_symbol_type(&self, name: &str) -> Type {
        self.scope_stack
            .borrow()
            .stack
            .iter()
            .rev()
            .find_map(|scope| scope.symbols.get(name))
            .unwrap()
            .clone()
    }

    pub fn save_symbol_to_global_scope(&self, name: String, symbol_type: Type) {
        self.scope_stack
            .borrow_mut()
            .stack
            .iter_mut()
            .find(|scope| scope.scope_type == ScopeType::Global)
            .unwrap() // global scope is always exist
            .symbols
            .insert(name, symbol_type);
    }

    pub fn save_symbol_to_current_scope(&self, name: String, symbol_type: Type) {
        self.scope_stack
            .borrow_mut()
            .stack
            .last_mut()
            .unwrap()
            .symbols
            .insert(name, symbol_type);
    }
}
