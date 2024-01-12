// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use builtin::types::Type as BuiltinType;
use std::collections::HashMap;

pub struct Scope {
    pub symbols: HashMap<String, BuiltinType>,
    pub scope_type: ScopeType,
}

impl Scope {
    pub fn new_builtin_scope() -> Self {
        let symbols = builtin::functions::get_types();
        Self {
            symbols,
            scope_type: ScopeType::Builtin,
        }
    }

    pub fn new_global_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            scope_type: ScopeType::Global,
        }
    }

    pub fn new_conditional_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            scope_type: ScopeType::Conditional,
        }
    }

    pub fn new_loop_scope() -> Self {
        Self {
            symbols: HashMap::new(),
            scope_type: ScopeType::Loop,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ScopeType {
    Builtin,
    Global,
    Conditional,
    Loop,
}
