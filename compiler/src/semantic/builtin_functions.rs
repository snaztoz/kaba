// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use super::types::{CallableParameters, Type};
use std::collections::{BTreeMap, HashMap};

pub fn get_types() -> HashMap<String, Type> {
    HashMap::from([(
        String::from("print"),
        Type::Callable {
            params: CallableParameters::from_btree_map(BTreeMap::new()),
            return_t: Box::new(Type::Void),
        },
    )])
}
