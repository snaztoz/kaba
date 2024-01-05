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
