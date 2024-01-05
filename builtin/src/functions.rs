// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! Contains the built-in functions of Kaba programming language
//! implementation.

use crate::types::Types;
use std::collections::HashMap;

pub fn get_types() -> HashMap<String, Types> {
    HashMap::from([(
        String::from("print"),
        Types::Callable {
            parameters: vec![Types::Any],
            return_type: Box::new(Types::Void),
        },
    )])
}
