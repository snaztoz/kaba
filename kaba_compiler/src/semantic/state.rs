use super::typ::{Type, BASIC_T};
use crate::ast::NodeId;
use std::{collections::HashMap, mem};

pub type SymbolTable = HashMap<NodeId, SymbolTableEntry>;
type ScopeTable = HashMap<NodeId, ScopeTableEntry>;

pub struct AnalyzerState {
    symbol_table: SymbolTable,
    scope_table: ScopeTable,

    // Maintain the current active scope by storing its ID.
    current_scope_id: NodeId,

    // The last returned type.
    //
    // Used to track the type of `return` statements to facilitate the type
    // checking with function's return type.
    returned_type: Box<Type>,
}

impl AnalyzerState {
    /// Create a new [`AnalyzerState`] instance.
    ///
    /// It takes the ID of [`crate::ast::AstNode`] to determines the global
    /// scope.
    pub fn new(program_id: NodeId) -> Self {
        Self {
            symbol_table: HashMap::new(),
            scope_table: HashMap::from([(program_id, ScopeTableEntry::new_global())]),
            current_scope_id: program_id,
            returned_type: Box::new(Type::Void),
        }
    }

    /// Consume `self` and return the instance of [`SymbolTable`].
    pub fn take_symbol_table(self) -> SymbolTable {
        self.symbol_table
    }

    /// Check if a given type `t` exists in the current active scope or its
    /// ancestors.
    pub fn has_t(&self, t: &Type) -> bool {
        match t {
            t if t.is_basic_t() => true,

            Type::Callable { params_t, return_t } => {
                params_t.iter().all(|t| self.has_t(t)) && self.has_t(return_t)
            }

            Type::Array { elem_t } => self.has_t(elem_t),

            Type::Symbol(name) => {
                matches!(
                    self.get_sym_variant(name),
                    Some(SymbolVariant::UndefinedType) | Some(SymbolVariant::Type(_))
                )
            }

            _ => unreachable!(),
        }
    }

    /// Get the variant associated with a `sym_name` in the current active
    /// scope or its ancestors.
    pub fn get_sym_variant(&self, sym_name: &str) -> Option<&SymbolVariant> {
        let mut scope_id = self.current_scope_id();

        loop {
            let scope_entry = self.get_scope(scope_id);

            if let Some(symbol_id) = scope_entry.symbols.get(sym_name) {
                return Some(&self.get_sym(*symbol_id).variant);
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

    /// Check if the given `sym_name` can be saved.
    ///
    /// A symbol is considered to can't be saved if:
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

    /// Save type declaration to current active scope without providing type.
    pub fn save_type_declaration(&mut self, sym_id: NodeId, sym_name: &str) {
        // Save to scope table
        let scope = self.get_scope_mut(self.current_scope_id());
        scope.symbols.insert(String::from(sym_name), sym_id);

        // Save to symbol table
        self.symbol_table.insert(
            sym_id,
            SymbolTableEntry {
                name: String::from(sym_name),
                variant: SymbolVariant::UndefinedType,
            },
        );
    }

    /// Set the definition of a type.
    pub fn set_type_definition(&mut self, sym_id: NodeId, t: Type) {
        self.symbol_table.get_mut(&sym_id).unwrap().variant = SymbolVariant::Type(t);
    }

    /// Save symbol and its associated type to current active scope.
    pub fn save_entity(&mut self, sym_id: NodeId, sym_name: &str, t: Type) {
        self.save_entity_to(self.current_scope_id(), sym_id, sym_name, t);
    }

    /// Save entity to a specific scope without have to entering it first.
    pub fn save_entity_to(&mut self, scope_id: NodeId, sym_id: NodeId, sym_name: &str, t: Type) {
        // Save to scope table
        let scope = self.get_scope_mut(scope_id);
        scope.symbols.insert(String::from(sym_name), sym_id);

        // Save to symbol table
        self.symbol_table.insert(
            sym_id,
            SymbolTableEntry {
                name: String::from(sym_name),
                variant: SymbolVariant::Entity(t),
            },
        );
    }

    /// Create a new scope.
    pub fn create_scope(&mut self, id: NodeId, variant: ScopeVariant, parent_id: NodeId) {
        // Insert the new child data
        self.scope_table
            .insert(id, ScopeTableEntry::new(variant, parent_id));

        // Update parent scope data
        self.scope_table
            .get_mut(&parent_id)
            .unwrap()
            .children_id
            .push(id);
    }

    /// Create a new scope in the inside of current active scope.
    pub fn create_scope_inside_current_scope(&mut self, id: NodeId, variant: ScopeVariant) {
        self.create_scope(id, variant, self.current_scope_id());
    }

    pub fn take_returned_type(&mut self) -> Type {
        mem::replace(&mut self.returned_type, Type::Void)
    }

    pub fn set_returned_type(&mut self, t: Type) {
        *self.returned_type = t;
    }

    pub fn current_scope_id(&self) -> NodeId {
        self.current_scope_id
    }

    fn get_sym(&self, sym_id: NodeId) -> &SymbolTableEntry {
        self.symbol_table.get(&sym_id).unwrap()
    }

    pub fn enter_scope(&mut self, scope_id: NodeId) {
        self.current_scope_id = scope_id;
    }

    fn get_scope(&self, scope_id: NodeId) -> &ScopeTableEntry {
        self.scope_table.get(&scope_id).unwrap()
    }

    fn get_scope_mut(&mut self, scope_id: NodeId) -> &mut ScopeTableEntry {
        self.scope_table.get_mut(&scope_id).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTableEntry {
    pub name: String,
    pub variant: SymbolVariant,
}

#[derive(Clone, Debug)]
pub enum SymbolVariant {
    // The compiler run through the program multiple times, to analyze the
    // declaration of symbols, and then to analyze their definitions.
    //
    // Because type declarations (such as record) are read first, there is a
    // state where the definition of a type is not yet known to the compiler.
    //
    // This enum variant act as the placeholder for the mentioned state.
    UndefinedType,

    Type(Type),

    // Entity represents any symbols that actually reside in the final memory
    // space.
    //
    // For example, variable and functions are entities, while record
    // definition is not.
    Entity(Type),
}

impl SymbolVariant {
    pub fn as_type_t(&self) -> &Type {
        if let Self::Type(t) = self {
            t
        } else {
            unreachable!()
        }
    }

    pub fn as_entity_t(&self) -> &Type {
        if let Self::Entity(ent) = self {
            ent
        } else {
            unreachable!()
        }
    }

    pub fn into_type_t(self) -> Type {
        if let Self::Type(t) = self {
            t
        } else {
            unreachable!()
        }
    }

    pub fn into_entity_t(self) -> Type {
        if let Self::Entity(ent) = self {
            ent
        } else {
            unreachable!()
        }
    }
}

#[derive(Clone, Debug)]
struct ScopeTableEntry {
    variant: ScopeVariant,
    symbols: HashMap<String, NodeId>,
    parent_id: Option<NodeId>,
    children_id: Vec<NodeId>,
}

impl ScopeTableEntry {
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
