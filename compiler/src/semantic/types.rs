use crate::ast::{AstNode, TypeNotation};
use std::fmt::Display;

pub mod assert;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Void,

    // Primitive types
    Bool,
    Int(IntType),
    Float,

    // Other types (e.g. class)
    Identifier(String),

    // Compound types
    Array {
        elem_t: Box<Self>,
    },
    Callable {
        params_t: Vec<Self>,
        return_t: Box<Self>,
    },
}

impl Type {
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }

        if self.is_unbounded_int() && other.is_bounded_int() {
            return self.is_convertible_to_bounded_int(other);
        } else if other.is_unbounded_int() && self.is_bounded_int() {
            return other.is_convertible_to_bounded_int(self);
        }

        false
    }

    pub fn is_assignable_to(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        if self.is_unbounded_int() {
            return self.is_convertible_to_bounded_int(other);
        }

        false
    }

    pub const fn is_unbounded_int(&self) -> bool {
        matches!(self, &Type::Int(IntType::Unbounded(_)))
    }

    pub fn is_bounded_int(&self) -> bool {
        matches!(self, Type::Int(t) if !matches!(t, IntType::Unbounded(_)))
    }

    fn is_convertible_to_bounded_int(&self, other: &Self) -> bool {
        if let Self::Int(IntType::Unbounded(n)) = self {
            return match other {
                // Try narrowing the value
                Self::Int(IntType::SByte) => i8::try_from(*n).is_ok(),
                Self::Int(IntType::Short) => i16::try_from(*n).is_ok(),

                // Always fit, does not need any checking
                Self::Int(IntType::Int) | Self::Int(IntType::Long) => true,

                _ => false,
            };
        }

        unreachable!()
    }

    pub fn unwrap_unbounded_int(&self) -> i32 {
        if let Self::Int(IntType::Unbounded(n)) = self {
            *n
        } else {
            unreachable!()
        }
    }

    pub fn unwrap_array(self) -> Self {
        if let Self::Array { elem_t, .. } = self {
            *elem_t
        } else {
            panic!("trying to unwrap array on non-Array type")
        }
    }

    pub fn unwrap_callable(self) -> (Vec<Self>, Self) {
        if let Self::Callable { params_t, return_t } = self {
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
                TypeNotation::Identifier(id) if id == "void" => Self::Void,
                TypeNotation::Identifier(id) if id == "bool" => Self::Bool,
                TypeNotation::Identifier(id) if id == "sbyte" => Self::Int(IntType::SByte),
                TypeNotation::Identifier(id) if id == "short" => Self::Int(IntType::Short),
                TypeNotation::Identifier(id) if id == "int" => Self::Int(IntType::Int),
                TypeNotation::Identifier(id) if id == "long" => Self::Int(IntType::Long),
                TypeNotation::Identifier(id) if id == "float" => Self::Float,
                TypeNotation::Identifier(id) => Self::Identifier(id.to_string()),

                TypeNotation::Array { elem_tn } => Self::Array {
                    elem_t: Box::new(Self::from(elem_tn.as_ref())),
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
            Self::Void => write!(f, "void"),

            Self::Bool => write!(f, "bool"),
            Self::Int(t) => t.fmt(f),
            Self::Float => write!(f, "float"),

            Self::Identifier(id) => write!(f, "{id}"),

            Self::Array { elem_t, .. } => write!(f, "[]{elem_t}"),

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

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum IntType {
    // An integer type that can be promoted into other integer types. For
    // example:
    //
    //  var x: int = 5;
    //
    // Here the type of `5` is unbounded int, which can be assigned into `x`
    // directly as `int` type.
    Unbounded(i32),

    SByte,
    Short,
    Int,
    Long,
}

impl Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unbounded(n) => write!(f, "unbounded int constant ({n})"),
            Self::SByte => write!(f, "sbyte"),
            Self::Short => write!(f, "short"),
            Self::Int => write!(f, "int"),
            Self::Long => write!(f, "long"),
        }
    }
}
