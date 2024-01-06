// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use builtin::types::Types as BuiltinTypes;
use std::collections::HashMap;

pub struct Scope {
    pub symbols: HashMap<String, BuiltinTypes>,
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
}

pub enum ScopeType {
    Builtin,
    Global,
    Conditional,
}
