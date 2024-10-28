use super::{
    error::{Error, Result},
    scope::{Scope, ScopeStack, ScopeType},
    types::Type,
};

#[derive(Default)]
pub struct Context {
    scopes: ScopeStack,
}

impl Context {
    pub fn get_symbol_type(&self, name: &str) -> Option<Type> {
        self.scopes.find_reversed(|s| s.symbols.get(name).cloned())
    }

    pub fn has_type(&self, t: &Type) -> bool {
        matches!(t, Type::Callable { .. })
            || self
                .scopes
                .find_reversed(|s| if s.types.contains(t) { Some(()) } else { None })
                .is_some()
    }

    pub fn current_function_return_type(&self) -> Option<Type> {
        self.scopes.find_reversed(|s| match &s.scope_t {
            ScopeType::Function { return_t } => Some(return_t.clone()),
            _ => None,
        })
    }

    pub fn is_inside_loop(&self) -> bool {
        self.scopes.any_reversed(|s| s.scope_t == ScopeType::Loop)
    }

    pub fn save_symbol_or_else<F>(&self, name: &str, sym_t: Type, err: F) -> Result<()>
    where
        F: FnOnce() -> Error,
    {
        self.scopes.with_current_scope(|s| {
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
        self.scopes.with_scope(scope, callback)
    }
}
