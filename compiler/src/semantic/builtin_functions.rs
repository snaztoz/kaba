use super::types::Type;
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
