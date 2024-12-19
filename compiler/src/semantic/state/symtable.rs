use super::scope::{Scope, ScopeRef, ScopeVariant};
use crate::semantic::types::Type;
use std::rc::Rc;

/// SymTable implements the [symbol table](https://en.wikipedia.org/wiki/Symbol_table)
/// of a Kaba program.
///
/// It uses a tree data structure to store [`Scope`] alongside their associated
/// symbols and [`Type`].
///
/// It is constructed during the semantic analysist and will be accessed again
/// on later phase(s), such as during IR code generation.
#[derive(Debug)]
pub struct SymTable {
    pub root: ScopeRef,
}

impl SymTable {
    /// Create a new [`SymTable`] instance with some [`Scope`] already included.
    ///
    /// The newly created instance contains the `ScopeVariant::Builtin` and
    /// `ScopeVariant::Global` variants, with some builtin [`Type`] included
    /// in the `ScopeVariant::Builtin` (such as the `Type::Void` and
    /// `Type::Int`).
    pub fn new() -> Self {
        let builtin = Scope::new(ScopeVariant::Builtin);
        builtin.borrow_mut().add_t(Type::Void);
        builtin.borrow_mut().add_t(Type::Bool);
        builtin.borrow_mut().add_t(Type::SByte);
        builtin.borrow_mut().add_t(Type::Short);
        builtin.borrow_mut().add_t(Type::Int);
        builtin.borrow_mut().add_t(Type::Long);
        builtin.borrow_mut().add_t(Type::Float);

        let global = Scope::new(ScopeVariant::Global);

        SymTable::add_scope(&builtin, global);

        Self { root: builtin }
    }

    /// Attach a new scope to a parent scope as its child.
    pub fn add_scope(parent: &ScopeRef, child: ScopeRef) {
        child.borrow_mut().parent = Some(Rc::downgrade(parent));

        parent.borrow_mut().children.push(child);
    }

    /// Get the [`Type`] stored under `sym` key.
    ///
    /// It first will check inside the scope pointed by `start_from` reference,
    /// and will walk upward (traversing the ancestors) until the symbol is
    /// found.
    ///
    /// If it has reached the root scope (`ScopeVariant::Builtin`) but still
    /// unable to find the symbol, it will return `None` instead.
    pub fn get_sym_t(sym: &str, start_from: &ScopeRef) -> Option<Type> {
        let mut current = Some(Rc::clone(start_from));

        while let Some(scope) = current {
            if scope.as_ref().borrow().has_sym(sym) {
                return Some(scope.as_ref().borrow().get_sym_t(sym));
            }

            current = scope
                .as_ref()
                .borrow()
                .parent
                .as_ref()
                .and_then(|p| p.upgrade());
        }

        None
    }

    /// Check if the scope pointed by `start_from` reference (and its ancestors)
    /// contains type `t`.
    pub fn has_t(t: &Type, start_from: &ScopeRef) -> bool {
        let mut current = Some(Rc::clone(start_from));

        while let Some(scope) = current {
            if scope.as_ref().borrow().has_t(t) {
                return true;
            }

            current = scope
                .as_ref()
                .borrow()
                .parent
                .as_ref()
                .and_then(|p| p.upgrade());
        }

        false
    }

    /// Get the return type of nearest function scope (`ScopeVariant::Function`)
    /// ancestor.
    ///
    /// It will returns `None` if no function scope can be found until it has
    /// reached the root.
    pub fn nearest_return_t(start_from: &ScopeRef) -> Option<Type> {
        let mut current = Some(Rc::clone(start_from));

        while let Some(scope) = current {
            if scope.as_ref().borrow().is_function() {
                return Some(scope.as_ref().borrow().function_return_t());
            }

            current = scope
                .as_ref()
                .borrow()
                .parent
                .as_ref()
                .and_then(|p| p.upgrade());
        }

        None
    }

    /// Check if the scope referenced by `start_from` is contained within a
    /// loop.
    ///
    /// That is, check if there is a `ScopeVariant::Loop` in one of the scope's
    /// ancestors.
    pub fn is_inside_loop(start_from: &ScopeRef) -> bool {
        let mut current = Some(Rc::clone(start_from));

        while let Some(scope) = current {
            if scope.as_ref().borrow().is_loop() {
                return true;
            }

            current = scope
                .as_ref()
                .borrow()
                .parent
                .as_ref()
                .and_then(|p| p.upgrade());
        }

        false
    }
}
