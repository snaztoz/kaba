use super::{error::Result, typ::Type};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

pub struct ScopeStack {
    stack: RefCell<Vec<Scope>>,
}

impl ScopeStack {
    pub fn find_reversed<U, F>(&self, finder: F) -> Option<U>
    where
        F: FnMut(&Scope) -> Option<U>,
    {
        self.stack.borrow().iter().rev().find_map(finder)
    }

    pub fn any_reversed<F>(&self, cond: F) -> bool
    where
        F: FnMut(&Scope) -> bool,
    {
        self.stack.borrow().iter().rev().any(cond)
    }

    pub fn with_current_scope<F>(&self, action: F) -> Result<()>
    where
        F: FnOnce(&mut Scope) -> Result<()>,
    {
        let mut stack = self.stack.borrow_mut();
        let s = stack.last_mut().unwrap();
        action(s)
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
        self.stack.borrow_mut().push(scope);
    }

    fn pop_scope(&self) {
        self.stack.borrow_mut().pop();
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self {
            stack: RefCell::new(vec![Scope::new_builtin_scope(), Scope::new_global_scope()]),
        }
    }
}

pub struct Scope {
    pub symbols: HashMap<String, Type>,
    pub types: HashSet<Type>,
    pub scope_t: ScopeType,
}

impl Scope {
    pub fn new_builtin_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            types: HashSet::from([
                Type::new("Void"),
                Type::new("Int"),
                Type::new("Float"),
                Type::new("Bool"),
            ]),
            scope_t: ScopeType::Builtin,
        }
    }

    pub fn new_global_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            types: HashSet::new(),
            scope_t: ScopeType::Global,
        }
    }

    pub fn new_conditional_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            types: HashSet::new(),
            scope_t: ScopeType::Conditional,
        }
    }

    pub fn new_loop_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            types: HashSet::new(),
            scope_t: ScopeType::Loop,
        }
    }

    pub fn new_function_scope(return_t: Type) -> Self {
        Self {
            symbols: HashMap::new(),
            types: HashSet::new(),
            scope_t: ScopeType::Function { return_t },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ScopeType {
    Builtin,
    Global,
    Conditional,
    Loop,
    Function { return_t: Type },
}
