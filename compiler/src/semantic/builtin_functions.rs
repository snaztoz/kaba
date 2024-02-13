// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use super::types::Type;
use std::collections::HashMap;

pub fn get_types() -> HashMap<String, Type> {
    HashMap::from([
        (
            String::from("print/Int"),
            Type::Callable {
                parameter_types: vec![Type::Int],
                return_type: Box::new(Type::Void),
            },
        ),
        (
            String::from("print/Float"),
            Type::Callable {
                parameter_types: vec![Type::Float],
                return_type: Box::new(Type::Void),
            },
        ),
        (
            String::from("print/Bool"),
            Type::Callable {
                parameter_types: vec![Type::Bool],
                return_type: Box::new(Type::Void),
            },
        ),
    ])
}
