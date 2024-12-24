use crate::ast::{AstNode, TypeNotation};
use std::fmt::Display;

pub mod assert;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Void,

    // An integer type that can be promoted into other integer types. For
    // example:
    //
    //  var x: int = 5;
    //
    // Here the type of `5` is UnboundedInt, it can be assigned into `x`
    // directly, which has an `int` type.
    UnboundedInt,

    // Primitive types
    Bool,
    Float,
    Int,
    Long,
    SByte,
    Short,

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
    /// Get the largest numeric type between `a` and `b`.
    ///
    /// Smaller numeric type can be casted into a larger numeritc type
    /// automatically, with the ordering of the types is like the following:
    ///
    /// ## Signed integers
    ///
    /// 1. UnboundedInt
    /// 2. SByte (signed-byte)
    /// 3. Short
    /// 4. Int
    /// 5. Long
    ///
    /// # Example
    ///
    /// ```ignore
    /// let a = Type::UnboundedInt;
    /// let b = Type::Int;
    ///
    /// assert_eq!(Type::largest_numeric_t_between(&a, &b), &Type::Int)
    /// ```
    pub fn largest_numeric_t_between<'a>(a: &'a Self, b: &'a Self) -> &'a Self {
        if a == b {
            return b;
        }

        for t in &Self::signed_ints() {
            if t == a {
                return b;
            } else if t == b {
                return a;
            }
        }

        unreachable!()
    }

    fn signed_ints() -> Vec<Self> {
        vec![
            Self::UnboundedInt,
            Self::SByte,
            Self::Short,
            Self::Int,
            Self::Long,
        ]
    }

    fn numbers() -> Vec<Self> {
        [Self::signed_ints(), vec![Self::Float]].concat()
    }

    pub fn is_compatible_with(&self, other: &Type) -> bool {
        self == other || self.is_promotable_to(other) || other.is_promotable_to(self)
    }

    pub fn is_assignable_to(&self, other: &Self) -> bool {
        self == other || self.is_promotable_to(other)
    }

    /// Check if `self` is promotable to `other`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let t = Type::UnboundedInt;
    ///
    /// assert!(t.is_promotable_to(&Type::Int));
    /// ```
    fn is_promotable_to(&self, other: &Self) -> bool {
        self == &Type::UnboundedInt && Self::signed_ints().contains(other)
    }

    /// Promote some types into their default type.
    ///
    /// For example, unbounded integers will have the default type of `int`
    /// when assigned into a variable:
    ///
    /// ```text
    /// var x = 5;
    /// ```
    ///
    /// `5` is actually has the type of `Type::UnboundedInt`, but the `x`
    /// symbol will have the type of `Type::Int`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let t = Type::UnboundedInt;
    ///
    /// assert_eq!(t.promote_default(), &Type::Int);
    /// ```
    pub fn promote_default(&self) -> Self {
        match self {
            Self::UnboundedInt => Self::Int,

            t => t.clone(),
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
                TypeNotation::Identifier(id) if id == "sbyte" => Self::SByte,
                TypeNotation::Identifier(id) if id == "short" => Self::Short,
                TypeNotation::Identifier(id) if id == "int" => Self::Int,
                TypeNotation::Identifier(id) if id == "long" => Self::Long,
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

            Self::UnboundedInt => write!(f, "int"),
            Self::SByte => write!(f, "sbyte"),
            Self::Short => write!(f, "short"),
            Self::Int => write!(f, "int"),
            Self::Long => write!(f, "long"),
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
