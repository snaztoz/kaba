use crate::ast::{AstNode, AstNodeVariant, RecordField, TypeNotation};
use std::{cmp::Ordering, collections::HashMap, fmt::Display, hash::Hash};

pub mod assert;
pub mod check;

pub static BASIC_T: &[&str] = &[
    "void", "bool", "sbyte", "short", "int", "long", "float", "double", "char", "string",
];

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    // Basic types
    Void,
    Bool,
    Int(IntType),
    Float(FloatType),
    Char,
    String,

    // Compound types
    Array {
        elem_t: Box<Self>,
    },
    Record {
        fields: HashMap<String, Self>,
    },
    Callable {
        params_t: Vec<Self>,
        return_t: Box<Self>,
    },

    // Other types (e.g. class)
    Symbol(String),
}

impl Type {
    pub const fn is_basic_t(&self) -> bool {
        matches!(
            self,
            Self::Void | Self::Bool | Self::Int(_) | Self::Float(_) | Self::Char | Self::String
        )
    }

    pub const fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub const fn is_record(&self) -> bool {
        matches!(self, Type::Record { .. })
    }

    pub const fn is_unbounded_int(&self) -> bool {
        matches!(self, &Type::Int(IntType::Unbounded(_)))
    }

    pub const fn is_bounded_int(&self) -> bool {
        !self.is_unbounded_int()
    }

    pub const fn is_unbounded_float(&self) -> bool {
        matches!(self, &Type::Float(FloatType::Unbounded(_)))
    }

    pub const fn is_bounded_float(&self) -> bool {
        !self.is_unbounded_float()
    }

    pub fn as_unbounded_int(&self) -> i32 {
        if let Self::Int(IntType::Unbounded(n)) = self {
            *n
        } else {
            unreachable!()
        }
    }

    pub fn as_unbounded_float(&self) -> f32 {
        if let Self::Float(FloatType::Unbounded(n)) = self {
            *n
        } else {
            unreachable!()
        }
    }

    pub fn as_array_elem_t(&self) -> &Self {
        if let Self::Array { elem_t, .. } = self {
            elem_t
        } else {
            panic!("trying to unwrap array on non-Array type")
        }
    }

    pub fn as_record_fields(&self) -> &HashMap<String, Self> {
        if let Self::Record { fields } = self {
            fields
        } else {
            unreachable!()
        }
    }

    pub fn as_callable_signature_t(&self) -> (&[Self], &Self) {
        if let Self::Callable { params_t, return_t } = self {
            (params_t, return_t)
        } else {
            panic!("trying to unwrap callable on non-Callable type")
        }
    }

    pub fn into_array_elem_t(self) -> Self {
        if let Self::Array { elem_t, .. } = self {
            *elem_t
        } else {
            panic!("trying to unwrap array on non-Array type")
        }
    }

    pub fn into_record_fields(self) -> HashMap<String, Self> {
        if let Self::Record { fields } = self {
            fields
        } else {
            unreachable!()
        }
    }

    pub fn into_callable_signature_t(self) -> (Vec<Self>, Self) {
        if let Self::Callable { params_t, return_t } = self {
            (params_t, *return_t)
        } else {
            panic!("trying to unwrap callable on non-Callable type")
        }
    }
}

impl<'a> From<&'a AstNode<'_>> for Type {
    fn from(value: &'a AstNode) -> Self {
        if let AstNodeVariant::TypeNotation { tn, .. } = &value.variant {
            match tn {
                TypeNotation::Symbol(sym) if *sym == "void" => Self::Void,
                TypeNotation::Symbol(sym) if *sym == "bool" => Self::Bool,
                TypeNotation::Symbol(sym) if *sym == "sbyte" => Self::Int(IntType::SByte),
                TypeNotation::Symbol(sym) if *sym == "short" => Self::Int(IntType::Short),
                TypeNotation::Symbol(sym) if *sym == "int" => Self::Int(IntType::Int),
                TypeNotation::Symbol(sym) if *sym == "long" => Self::Int(IntType::Long),
                TypeNotation::Symbol(sym) if *sym == "float" => Self::Float(FloatType::Float),
                TypeNotation::Symbol(sym) if *sym == "double" => Self::Float(FloatType::Double),
                TypeNotation::Symbol(sym) if *sym == "char" => Self::Char,
                TypeNotation::Symbol(sym) if *sym == "string" => Self::String,
                TypeNotation::Symbol(sym) => Self::Symbol(String::from(*sym)),

                TypeNotation::Array { elem_tn } => Self::Array {
                    elem_t: Box::new(Self::from(elem_tn.as_ref())),
                },

                TypeNotation::Record { fields } => {
                    let mut fields_t = HashMap::new();
                    for RecordField { sym, tn } in fields {
                        let sym_name = String::from(sym.variant.as_sym_name());
                        let sym_t = Self::from(tn);
                        fields_t.insert(sym_name, sym_t);
                    }
                    Self::Record { fields: fields_t }
                }

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
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "string"),

            Self::Symbol(id) => write!(f, "{id}"),

            Self::Array { elem_t, .. } => write!(f, "[]{elem_t}"),

            Self::Callable { params_t, return_t } => {
                let params_str = params_t
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "({params_str}) -> {return_t}")
            }

            Self::Record { fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, t)| format!("{name}: {t}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{ {fields_str} }}")
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
    // Similar to unbounded integer, but this is for float numbers instead.
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
