use super::types::{Type, BASIC_T};
use crate::ast::NodeId;
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
};

pub type SymbolTable = HashMap<NodeId, SymbolEntry>;
type ScopeTable = HashMap<NodeId, ScopeEntry>;

pub struct AnalyzerState {
    symbol_table: RefCell<SymbolTable>,
    scope_table: RefCell<ScopeTable>,
    _current_scope_id: RefCell<NodeId>,
}

impl AnalyzerState {
    pub fn new(global_id: NodeId) -> Self {
        Self {
            symbol_table: RefCell::new(HashMap::new()),
            scope_table: RefCell::new(HashMap::from([(global_id, ScopeEntry::new_global())])),
            _current_scope_id: RefCell::new(global_id),
        }
    }

    pub fn take_symbol_table(self) -> SymbolTable {
        self.symbol_table.take()
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
                matches!(
                    self.get_sym_t(name).as_deref(),
                    Some(SymbolType::TypeDefinition(_))
                )
            }

            _ => unreachable!(),
        }
    }

    /// Get the type associated with a `sym` in the current active scope or its
    /// ancestors.
    pub fn get_sym_t(&self, sym_name: &str) -> Option<Ref<'_, SymbolType>> {
        let mut scope_id = self.current_scope_id();

        loop {
            let scope_entry = self.get_scope(scope_id);

            if let Some(symbol_id) = scope_entry.symbols.get(sym_name) {
                return Some(Ref::map(self.get_sym(*symbol_id), |s| &s.t));
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
            let scope_entry = self.get_scope(scope_id);

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
            let scope_entry = self.get_scope(scope_id);

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
    pub fn can_save_sym(&self, sym_name: &str) -> bool {
        if BASIC_T.contains(&sym_name) {
            return false;
        }

        let current_scope_id = self.current_scope_id();
        let current_scope = self.get_scope(current_scope_id);

        !current_scope.symbols.contains_key(sym_name)
    }

    /// Save symbol and its associated type to current active scope.
    pub fn save_entity(&self, sym_id: NodeId, sym_name: &str, t: Type) {
        self.save_entity_to(self.current_scope_id(), sym_id, sym_name, t);
    }

    /// Save entity to a specific scope without have to entering it first.
    pub fn save_entity_to(&self, scope_id: NodeId, sym_id: NodeId, sym_name: &str, t: Type) {
        // Save to scope table
        let mut scope = self.get_scope_mut(scope_id);
        scope.symbols.insert(String::from(sym_name), sym_id);

        // Save to symbol table
        self.symbol_table.borrow_mut().insert(
            sym_id,
            SymbolEntry {
                name: String::from(sym_name),
                t: SymbolType::Entity(t),
            },
        );
    }

    /// Execute `action` within a new function scope.
    pub fn with_function_scope<U, F>(&self, scope_id: NodeId, return_t: Type, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        self.with_scope(scope_id, ScopeVariant::Function { return_t }, action)
    }

    /// Execute `action` within a new conditional scope.
    pub fn with_conditional_scope<U, F>(&self, scope_id: NodeId, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        self.with_scope(scope_id, ScopeVariant::Conditional, action)
    }

    /// Execute `action` within a new loop scope.
    pub fn with_loop_scope<U, F>(&self, scope_id: NodeId, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        self.with_scope(scope_id, ScopeVariant::Loop, action)
    }

    /// Execute `action` within a scope of `variant` variant.
    ///
    /// It automatically handles the creating and exiting of the new scope.
    fn with_scope<U, F>(&self, scope_id: NodeId, scope_variant: ScopeVariant, action: F) -> U
    where
        F: FnOnce() -> U,
    {
        let current_id = self.current_scope_id();

        if !self.has_scope(scope_id) {
            self.create_scope(scope_id, scope_variant, current_id);
        }

        self.enter_scope(scope_id);
        let result = action();
        self.enter_scope(current_id);

        result
    }

    pub fn create_scope(&self, id: NodeId, variant: ScopeVariant, parent_id: NodeId) {
        let mut table = self.scope_table.borrow_mut();

        // Insert the new child data
        table.insert(id, ScopeEntry::new(variant, parent_id));

        // Update parent scope data
        table.get_mut(&parent_id).unwrap().children_id.push(id);
    }

    pub fn current_scope_id(&self) -> NodeId {
        *self._current_scope_id.borrow()
    }

    fn get_sym(&self, sym_id: NodeId) -> Ref<'_, SymbolEntry> {
        Ref::map(self.symbol_table.borrow(), |st| st.get(&sym_id).unwrap())
    }

    fn enter_scope(&self, scope_id: NodeId) {
        *self._current_scope_id.borrow_mut() = scope_id;
    }

    fn get_scope(&self, scope_id: NodeId) -> Ref<'_, ScopeEntry> {
        Ref::map(self.scope_table.borrow(), |st| st.get(&scope_id).unwrap())
    }

    fn get_scope_mut(&self, scope_id: NodeId) -> RefMut<'_, ScopeEntry> {
        RefMut::map(self.scope_table.borrow_mut(), |st| {
            st.get_mut(&scope_id).unwrap()
        })
    }

    fn has_scope(&self, scope_id: NodeId) -> bool {
        self.scope_table.borrow().contains_key(&scope_id)
    }
}

#[derive(Clone, Debug)]
pub struct SymbolEntry {
    pub name: String,
    pub t: SymbolType,
}

#[derive(Clone, Debug)]
pub enum SymbolType {
    TypeDefinition(Type),
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

#[derive(Clone, Debug)]
struct ScopeEntry {
    variant: ScopeVariant,
    symbols: HashMap<String, NodeId>,
    parent_id: Option<NodeId>,
    children_id: Vec<NodeId>,
}

impl ScopeEntry {
    fn new(variant: ScopeVariant, parent_id: NodeId) -> Self {
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
    const fn is_function(&self) -> bool {
        matches!(self, Self::Function { .. })
    }

    const fn is_loop(&self) -> bool {
        matches!(self, ScopeVariant::Loop)
    }

    fn unwrap_function_return_t(&self) -> Type {
        match self {
            Self::Function { return_t } => return_t.clone(),
            _ => unreachable!(),
        }
    }
}
