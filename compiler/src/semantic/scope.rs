use super::{
    error::{Error, Result},
    types::Type,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

/// ScopeStack (or often abbreviated as `ss`) represents the stack of scopes
/// found in the program.
pub struct ScopeStack {
    stack: RefCell<Vec<Scope>>,
}

impl ScopeStack {
    pub fn get_symbol_t(&self, name: &str) -> Option<Type> {
        self.find_reversed(|s| s.symbols.get(name).cloned())
    }

    pub fn has_t(&self, t: &Type) -> bool {
        match t {
            Type::Callable { params_t, return_t } => {
                params_t.iter().all(|t| self.has_t(t)) && self.has_t(return_t)
            }

            Type::Array { elem_t, .. } => {
                let elem_t = elem_t.as_ref().unwrap();
                self.has_t(elem_t)
            }

            t => self
                .find_reversed(|s| if s.types.contains(t) { Some(()) } else { None })
                .is_some(),
        }
    }

    pub fn current_function_return_t(&self) -> Option<Type> {
        self.find_reversed(|s| match &s.scope_t {
            ScopeType::Function { return_t } => Some(return_t.clone()),
            _ => None,
        })
    }

    pub fn is_inside_loop(&self) -> bool {
        self.any_reversed(|s| s.scope_t == ScopeType::Loop)
    }

    pub fn save_symbol_or_else<F>(&self, name: &str, sym_t: Type, err: F) -> Result<()>
    where
        F: FnOnce() -> Error,
    {
        self.with_current_scope(|s| {
            if s.symbols.contains_key(name) {
                return Err(err());
            }
            s.symbols.insert(String::from(name), sym_t);
            Ok(())
        })
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

    fn find_reversed<U, F>(&self, finder: F) -> Option<U>
    where
        F: FnMut(&Scope) -> Option<U>,
    {
        self.stack.borrow().iter().rev().find_map(finder)
    }

    fn any_reversed<F>(&self, cond: F) -> bool
    where
        F: FnMut(&Scope) -> bool,
    {
        self.stack.borrow().iter().rev().any(cond)
    }

    fn with_current_scope<F>(&self, action: F) -> Result<()>
    where
        F: FnOnce(&mut Scope) -> Result<()>,
    {
        let mut stack = self.stack.borrow_mut();
        let s = stack.last_mut().unwrap();
        action(s)
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
