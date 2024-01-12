// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! Contains the built-in functions of Kaba programming language
//! implementation.

use crate::types::Type;
use std::collections::HashMap;

pub fn get_types() -> HashMap<String, Type> {
    HashMap::from([(
        String::from("print"),
        Type::Callable {
            parameters: vec![Type::Any],
            return_type: Box::new(Type::Void),
        },
    )])
}
