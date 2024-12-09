use super::{error::Error, Result};
use crate::ast::{AstNode, TypeNotation};
use logos::Span;
use std::fmt::Display;

#[derive(Clone, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub enum Type {
    Identifier(String),

    // We specifically differentiate the type of literals to accommodate their
    // assignment into various types.
    //
    // For example, both assignments are valid:
    //
    //      var x: Byte = 16;
    //      var y: Int = 16;
    //
    // If the `16` is Self::Identifier("Int"), then the assignment into the
    // `Byte` type variable should results in error. We want to avoid that.
    UIntLiteral,
    IntLiteral,

    Array {
        elem_t: Option<Box<Type>>,
    },

    Callable {
        params_t: Vec<Type>,
        return_t: Box<Type>,
    },
}

impl Type {
    pub fn new(id: &str) -> Self {
        Self::Identifier(String::from(id))
    }

    pub fn assert_number<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_number() {
            Ok(())
        } else {
            Err(Error::NonNumberType { span: err_span() })
        }
    }

    pub fn assert_signable_number<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_signable_number() {
            Ok(())
        } else {
            Err(Error::NonSignableNumberType {
                t: t.clone(),
                span: err_span(),
            })
        }
    }

    pub fn assert_same<F>(type_a: &Self, type_b: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if type_a == type_b || type_a.is_morphable_to(type_b) || type_b.is_morphable_to(type_a) {
            return Ok(());
        }

        Err(Error::TypeMismatch {
            type_a: type_a.clone(),
            type_b: type_b.clone(),
            span: err_span(),
        })
    }

    pub fn assert_boolean<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_boolean() {
            Ok(())
        } else {
            Err(Error::NonBooleanType { span: err_span() })
        }
    }

    pub fn assert_callable<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_callable() {
            Ok(())
        } else {
            Err(Error::NonCallableType {
                t: t.clone(),
                span: err_span(),
            })
        }
    }

    pub fn assert_iterable<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_array() {
            Ok(())
        } else {
            Err(Error::NonIterableType {
                t: t.clone(),
                span: err_span(),
            })
        }
    }

    pub fn assert_indexable<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_array() {
            Ok(())
        } else {
            Err(Error::NonIndexableType {
                t: t.clone(),
                span: err_span(),
            })
        }
    }

    pub fn assert_assignable<F>(from: &Self, to: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if from.is_assignable_to(to) {
            Ok(())
        } else {
            Err(Error::InvalidAssignmentType {
                var_t: to.to_string(),
                val_t: from.to_string(),
                span: err_span(),
            })
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Self::Identifier(id) if id == "Int" || id == "Float")
            || self.is_number_literal()
    }

    pub fn is_number_literal(&self) -> bool {
        [Self::UIntLiteral, Self::IntLiteral].contains(self)
    }

    fn is_signable_number(&self) -> bool {
        matches!(self, Self::Identifier(id) if id == "Int" || id == "Float")
            || self.is_number_literal()
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Identifier(id) if id == "Bool")
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
        if self == target || self.is_morphable_to(target) {
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

    // Check if `self` is morphable to `target`
    //
    // For example, unsigned integer literals are morphable into `Byte`,
    // `Short`, `Int`, etc.
    fn is_morphable_to(&self, target: &Type) -> bool {
        self == &Type::UIntLiteral && [Type::new("Int"), Type::IntLiteral].contains(target)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Identifier(id) if id == "Void")
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
                TypeNotation::Identifier(id) => Self::new(id),

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
            Self::Identifier(id) => write!(f, "{id}"),

            Self::UIntLiteral => write!(f, "UInt"),
            Self::IntLiteral => write!(f, "Int"),

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
