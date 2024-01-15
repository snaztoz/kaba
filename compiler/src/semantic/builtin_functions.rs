use super::types::Type;
use std::collections::{BTreeSet, HashMap};

pub fn get_types() -> HashMap<String, Type> {
    HashMap::from([(
        String::from("print"),
        Type::Callable {
            parameter_variants: BTreeSet::from([
                vec![Type::Int],
                vec![Type::Float],
                vec![Type::Bool],
            ]),
            return_type: Box::new(Type::Void),
        },
    )])
}
