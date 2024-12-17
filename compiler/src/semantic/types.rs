use crate::ast::{AstNode, TypeNotation};
use std::fmt::Display;

pub mod assert;

#[derive(Clone, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub enum Type {
    // An integer type that can be promoted into other integer types. For
    // example:
    //
    //  var x: int = 5;
    //
    // Here `5` type is LiteralInt, and then it can be assigned into `x`, which
    // has an `int` type.
    LiteralInt,

    Identifier(String),

    Array {
        elem_t: Option<Box<Type>>,
    },

    Callable {
        params_t: Vec<Type>,
        return_t: Box<Type>,
    },
}

impl Type {
    pub fn void() -> Type {
        Type::Identifier(String::from("void"))
    }

    pub fn bool() -> Type {
        Type::Identifier(String::from("bool"))
    }

    pub fn int() -> Type {
        Type::Identifier(String::from("int"))
    }

    pub fn float() -> Type {
        Type::Identifier(String::from("float"))
    }

    pub fn is_number(&self) -> bool {
        [Self::int(), Self::float()].contains(self) || self.is_number_literal()
    }

    pub const fn is_number_literal(&self) -> bool {
        matches!(self, Type::LiteralInt)
    }

    fn is_signable(&self) -> bool {
        [Self::int(), Self::float()].contains(self) || self.is_number_literal()
    }

    pub fn is_boolean(&self) -> bool {
        self == &Self::bool()
    }

    pub const fn is_array(&self) -> bool {
        matches!(self, Type::Array { .. })
    }

    pub fn is_array_with_unknown_elem_t(&self) -> bool {
        matches!(self, Type::Array { elem_t } if elem_t.is_none())
    }

    const fn is_callable(&self) -> bool {
        matches!(self, Self::Callable { .. })
    }

    pub fn is_assignable_to(&self, target: &Type) -> bool {
        if self == target || self.is_promotable_to(target) {
            return true;
        }

        if self.is_array() && target.is_array() {
            let self_elem_t = self.unwrap_array();
            let target_elem_t = target.unwrap_array();

            return self_elem_t
                .as_ref()
                .unwrap_or_else(|| {
                    // Infer self's element type from target's type, because
                    // target_elem_t will never has `None` value.
                    target_elem_t.as_ref().unwrap()
                })
                .is_assignable_to(target_elem_t.as_ref().unwrap());
        }

        false
    }

    // Check if `self` is promotable to `target`.
    //
    // For example, `LiteralInt` is promotable into `byte`, `short`, `int`,
    // etc.
    fn is_promotable_to(&self, target: &Type) -> bool {
        self == &Self::LiteralInt && [Self::int()].contains(target)
    }

    pub fn is_void(&self) -> bool {
        self == &Self::void()
    }

    pub fn promote_default(&self) -> Type {
        match self {
            Type::LiteralInt => Self::int(),

            Self::Array { elem_t } => Self::Array {
                elem_t: elem_t.as_ref().map(|t| Box::new(t.promote_default())),
            },

            t => t.clone(),
        }
    }

    pub fn unwrap_array(&self) -> &Option<Box<Type>> {
        if let Type::Array { elem_t, .. } = self {
            elem_t
        } else {
            panic!("trying to unwrap array on non-Array type")
        }
    }

    pub fn unwrap_callable(self) -> (Vec<Type>, Type) {
        if let Type::Callable { params_t, return_t } = self {
            (params_t, *return_t)
        } else {
            panic!("trying to unwrap callable on non-Callable type")
        }
    }
}

impl<'a> From<&'a AstNode> for Type {
    fn from(value: &'a AstNode) -> Self {
        if let AstNode::TypeNotation { tn, .. } = value {
            match tn {
                TypeNotation::Identifier(id) => Self::Identifier(id.to_string()),

                TypeNotation::Array { elem_tn } => Self::Array {
                    elem_t: Some(Box::new(Self::from(elem_tn.as_ref()))),
                },

                TypeNotation::Callable {
                    params_tn,
                    return_tn,
                } => {
                    let mut params_t = vec![];
                    for tn in params_tn {
                        params_t.push(Self::from(tn));
                    }
                    let return_t = Box::new(Self::from(return_tn.as_ref()));
                    Self::Callable { params_t, return_t }
                }
            }
        } else {
            unreachable!()
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LiteralInt => write!(f, "int"),

            Self::Identifier(id) => write!(f, "{id}"),

            Self::Array { elem_t, .. } => {
                let elem_t = if let Some(t) = elem_t {
                    t.to_string()
                } else {
                    String::from("{unknown}")
                };

                write!(f, "[]{elem_t}")
            }

            Self::Callable { params_t, return_t } => {
                let params_t = params_t
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "({params_t}) -> {return_t}")
            }
        }
    }
}
