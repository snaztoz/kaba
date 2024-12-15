use crate::semantic::types::Type;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

pub type WeakScopeRef = Weak<RefCell<Scope>>;
pub type ScopeRef = Rc<RefCell<Scope>>;

/// Represents the scopes found in a Kaba program.
///
/// For example, an `each` loop will be represented by the `ScopeVariant::Loop`
/// variant.
///
/// A scope may contain type definitions, symbols, and also types associated
/// with the symbols. Using the previous example, if a variable called `x` is
/// declared within a loop scope, this variable will belongs to the scope, and
/// is reachable from the same or its children scopes only.
#[derive(Debug)]
pub struct Scope {
    variant: ScopeVariant,
    symbols: HashMap<String, Type>,
    types: HashSet<Type>,

    pub parent: Option<WeakScopeRef>,
    pub children: Vec<ScopeRef>,
}

impl Scope {
    /// Create a new [`Scope`] instance with `variant` variant.
    pub fn new(variant: ScopeVariant) -> ScopeRef {
        Rc::new(RefCell::new(Scope {
            variant,
            symbols: HashMap::new(),
            types: HashSet::new(),
            parent: None,
            children: vec![],
        }))
    }

    /// Save a symbol and its associated type inside the scope.
    pub fn save_sym(&mut self, sym: &str, t: Type) {
        self.symbols.insert(String::from(sym), t);
    }

    /// Check if the scope contains a symbol named `sym`.
    pub fn has_sym(&self, sym: &str) -> bool {
        self.symbols.contains_key(sym)
    }

    /// Get the type of a symbol inside the scope.
    pub fn get_sym_t(&self, sym: &str) -> Type {
        self.symbols.get(sym).cloned().unwrap()
    }

    /// Add a new type definition in the scope.
    pub fn add_t(&mut self, t: Type) {
        self.types.insert(t);
    }

    /// Check if the scope contains a type named `t`.
    pub fn has_t(&self, t: &Type) -> bool {
        self.types.contains(t)
    }

    /// Check if the scope variant is a `ScopeVariant::Function`.
    pub fn is_function(&self) -> bool {
        matches!(self.variant, ScopeVariant::Function { .. })
    }

    /// Check if the scope variant is a `ScopeVariant::Loop`.
    pub fn is_loop(&self) -> bool {
        self.variant == ScopeVariant::Loop
    }

    /// Retrieve the return type of the scope.
    ///
    /// It assumes that the scope has variant of `ScopeVariant::Function`.
    pub fn function_return_t(&self) -> Type {
        if let ScopeVariant::Function { return_t } = &self.variant {
            return_t.clone()
        } else {
            unreachable!()
        }
    }
}

/// Represents different variant of scopes.
#[derive(Debug, Eq, PartialEq)]
pub enum ScopeVariant {
    Builtin,
    Global,
    Function { return_t: Type },
    Conditional,
    Loop,
}
