use crate::ast::{AstNode, TypeNotation};
use std::{cmp::Ordering, fmt::Display, hash::Hash};

pub mod assert;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Type {
    Void,

    // Primitive types
    Bool,
    Int(IntType),
    Float(FloatType),

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

        if self.is_unbounded_float() && other.is_bounded_float() {
            return self.is_convertible_to_bounded_float(other);
        } else if other.is_unbounded_float() && self.is_bounded_float() {
            return other.is_convertible_to_bounded_float(self);
        }

        false
    }

    pub fn is_assignable_to(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        if self.is_unbounded_int() {
            return self.is_convertible_to_bounded_int(other);
        } else if self.is_unbounded_float() {
            return self.is_convertible_to_bounded_float(other);
        }

        false
    }

    pub const fn is_unbounded_int(&self) -> bool {
        matches!(self, &Type::Int(IntType::Unbounded(_)))
    }

    pub fn is_bounded_int(&self) -> bool {
        matches!(self, Type::Int(t) if !matches!(t, IntType::Unbounded(_)))
    }

    pub const fn is_unbounded_float(&self) -> bool {
        matches!(self, &Type::Float(FloatType::Unbounded(_)))
    }

    pub fn is_bounded_float(&self) -> bool {
        matches!(self, Type::Float(t) if !matches!(t, FloatType::Unbounded(_)))
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

    fn is_convertible_to_bounded_float(&self, other: &Self) -> bool {
        if let Self::Float(FloatType::Unbounded(_)) = self {
            return match other {
                // Always fit, does not need any checking
                Self::Float(FloatType::Float) | Self::Float(FloatType::Double) => true,

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

    pub fn unwrap_unbounded_float(&self) -> f32 {
        if let Self::Float(FloatType::Unbounded(n)) = self {
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
                TypeNotation::Identifier(id) if id == "float" => Self::Float(FloatType::Float),
                TypeNotation::Identifier(id) if id == "double" => Self::Float(FloatType::Double),
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
            Self::Float(t) => t.fmt(f),

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

#[derive(Clone, Debug, Ord, PartialOrd)]
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

impl Hash for IntType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Unbounded(_) => state.write_u8(0),
            Self::SByte => state.write_u8(1),
            Self::Short => state.write_u8(2),
            Self::Int => state.write_u8(3),
            Self::Long => state.write_u8(4),
        }
    }
}

impl PartialEq for IntType {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Unbounded(_), Self::Unbounded(_))
                | (Self::SByte, Self::SByte)
                | (Self::Short, Self::Short)
                | (Self::Int, Self::Int)
                | (Self::Long, Self::Long),
        )
    }
}

impl Eq for IntType {}

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

#[derive(Clone, Debug)]
pub enum FloatType {
    // An integer type that can be promoted into other integer types. For
    // example:
    //
    //  var x: int = 5;
    //
    // Here the type of `5` is unbounded int, which can be assigned into `x`
    // directly as `int` type.
    Unbounded(f32),

    Float,
    Double,
}

impl Hash for FloatType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Unbounded(_) => state.write_u8(0),
            Self::Float => state.write_u8(1),
            Self::Double => state.write_u8(2),
        }
    }
}

impl PartialEq for FloatType {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Unbounded(_), Self::Unbounded(_))
                | (Self::Float, Self::Float)
                | (Self::Double, Self::Double)
        )
    }
}

impl Eq for FloatType {}

impl Ord for FloatType {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            Self::Unbounded(_) => match other {
                Self::Unbounded(_) => Ordering::Equal,
                _ => Ordering::Less,
            },

            Self::Float => match other {
                Self::Unbounded(_) => Ordering::Greater,
                Self::Float => Ordering::Equal,
                Self::Double => Ordering::Less,
            },

            Self::Double => match other {
                Self::Double => Ordering::Equal,
                _ => Ordering::Greater,
            },
        }
    }
}

impl PartialOrd for FloatType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unbounded(n) => write!(f, "unbounded float constant ({n})"),
            Self::Float => write!(f, "float"),
            Self::Double => write!(f, "double"),
        }
    }
}
