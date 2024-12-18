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
    // Here the type of `5` is UnboundedInt, it can be assigned into `x`
    // directly, which has an `int` type.
    UnboundedInt,

    Identifier(String),

    Array {
        elem_t: Option<Box<Self>>,
    },

    Callable {
        params_t: Vec<Self>,
        return_t: Box<Self>,
    },
}

impl Type {
    pub fn void() -> Self {
        Self::Identifier(String::from("void"))
    }

    pub fn bool() -> Self {
        Self::Identifier(String::from("bool"))
    }

    pub fn sbyte() -> Self {
        Self::Identifier(String::from("sbyte"))
    }

    pub fn short() -> Self {
        Self::Identifier(String::from("short"))
    }

    pub fn int() -> Self {
        Self::Identifier(String::from("int"))
    }

    pub fn long() -> Self {
        Self::Identifier(String::from("long"))
    }

    pub fn float() -> Self {
        Self::Identifier(String::from("float"))
    }

    /// Get the largest numeric type between `a` and `b`.
    ///
    /// Smaller numeric type can be casted into a larger numeritc type
    /// automatically, with the ordering of the types is like the following:
    ///
    /// ## Signed integers
    ///
    /// 1. UnboundedInt
    /// 2. Short
    /// 3. Int
    /// 4. Long
    ///
    /// # Example
    ///
    /// ```ignore
    /// let a = Type::UnboundedInt;
    /// let b = Type::int();
    ///
    /// assert_eq!(Type::largest_numeric_t_between(&a, &b), &Type::int())
    /// ```
    pub fn largest_numeric_t_between<'a>(a: &'a Self, b: &'a Self) -> &'a Self {
        if a == b {
            return a;
        }

        for t in &Self::int_ordering() {
            if t == a {
                return b;
            } else if t == b {
                return a;
            }
        }

        unreachable!()
    }

    fn int_ordering() -> Vec<Self> {
        vec![
            Self::UnboundedInt,
            Self::sbyte(),
            Self::short(),
            Self::int(),
            Self::long(),
        ]
    }

    pub fn is_number(&self) -> bool {
        [
            Self::sbyte(),
            Self::short(),
            Self::int(),
            Self::long(),
            Self::float(),
        ]
        .contains(self)
            || self.is_unbounded_number()
    }

    pub const fn is_unbounded_number(&self) -> bool {
        matches!(self, Self::UnboundedInt)
    }

    fn is_signable(&self) -> bool {
        [
            Self::sbyte(),
            Self::short(),
            Self::int(),
            Self::long(),
            Self::float(),
        ]
        .contains(self)
            || self.is_unbounded_number()
    }

    pub fn is_void(&self) -> bool {
        self == &Self::void()
    }

    pub fn is_boolean(&self) -> bool {
        self == &Self::bool()
    }

    pub const fn is_array(&self) -> bool {
        matches!(self, Self::Array { .. })
    }

    pub fn is_array_with_unknown_elem_t(&self) -> bool {
        matches!(self, Self::Array { elem_t } if elem_t.is_none())
    }

    const fn is_callable(&self) -> bool {
        matches!(self, Self::Callable { .. })
    }

    pub fn is_assignable_to(&self, target: &Self) -> bool {
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

    /// Check if `self` is promotable to `target`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let t = Type::UnboundedInt;
    ///
    /// assert!(t.is_promotable_to(&Type::int()));
    /// ```
    fn is_promotable_to(&self, target: &Self) -> bool {
        let ord = Self::int_ordering();
        let self_idx = ord.iter().position(|t| t == self);
        let target_idx = ord.iter().position(|t| t == target);

        // NOTE: temporary solution
        if let Some(self_idx) = self_idx {
            if let Some(target_idx) = target_idx {
                return self_idx <= target_idx;
            }
        }

        false
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
    /// symbol will have the type of `Type::int()`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let t = Type::UnboundedInt;
    ///
    /// assert_eq!(t.promote_default(), &Type::int());
    /// ```
    pub fn promote_default(&self) -> Self {
        match self {
            Self::UnboundedInt => Self::int(),

            Self::Array { elem_t } => Self::Array {
                elem_t: elem_t.as_ref().map(|t| Box::new(t.promote_default())),
            },

            t => t.clone(),
        }
    }

    pub fn unwrap_array(&self) -> &Option<Box<Self>> {
        if let Self::Array { elem_t, .. } = self {
            elem_t
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
            Self::UnboundedInt => write!(f, "int"),

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
