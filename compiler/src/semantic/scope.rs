// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use super::{builtin_functions, types::Type};
use std::collections::HashMap;

pub struct ScopeStack {
    pub stack: Vec<Scope>,
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self {
            stack: vec![Scope::new_builtin_scope(), Scope::new_global_scope()],
        }
    }
}

pub struct Scope {
    pub symbols: HashMap<String, Type>,
    pub scope_t: ScopeType,
}

impl Scope {
    pub fn new_builtin_scope() -> Self {
        let symbols = builtin_functions::get_types();

        Self {
            symbols,
            scope_t: ScopeType::Builtin,
        }
    }

    pub fn new_global_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            scope_t: ScopeType::Global,
        }
    }

    pub fn new_conditional_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            scope_t: ScopeType::Conditional,
        }
    }

    pub fn new_loop_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            scope_t: ScopeType::Loop,
        }
    }

    pub fn new_function_scope(return_t: Type) -> Self {
        Self {
            symbols: HashMap::new(),
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
