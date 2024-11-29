use super::{error::Error, Result};
use crate::ast::{AstNode, Literal, TypeNotation};
use logos::Span;
use std::fmt::Display;

#[derive(Clone, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub enum Type {
    Identifier(String),
    Callable {
        params_t: Vec<Type>,
        return_t: Box<Type>,
    },
}

impl Type {
    pub fn new(id: &str) -> Self {
        Self::Identifier(String::from(id))
    }

    pub fn from_literal(lit: &Literal) -> Result<Self> {
        match lit {
            Literal::Void => Ok(Self::new("Void")),
            Literal::Integer(_) => Ok(Self::new("Int")),
            Literal::Float(_) => Ok(Self::new("Float")),
            Literal::Boolean(_) => Ok(Self::new("Bool")),
        }
    }

    pub fn from_tn(tn: &AstNode) -> Self {
        if let AstNode::TypeNotation { tn, .. } = tn {
            match tn {
                TypeNotation::Identifier(id) => Self::new(id),

                TypeNotation::Callable {
                    params_tn,
                    return_tn,
                } => {
                    let mut params_t = vec![];
                    for tn in params_tn {
                        params_t.push(Self::from_tn(tn));
                    }
                    let return_t = Box::new(Self::from_tn(return_tn));
                    Self::Callable { params_t, return_t }
                }
            }
        } else {
            unreachable!()
        }
    }

    pub fn assert_number<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_number() {
            Ok(())
        } else {
            Err(Error::NotANumber { span: err_span() })
        }
    }

    pub fn assert_boolean<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_boolean() {
            Ok(())
        } else {
            Err(Error::NotABoolean { span: err_span() })
        }
    }

    pub fn assert_callable<F>(t: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if t.is_callable() {
            Ok(())
        } else {
            Err(Error::NotAFunction { span: err_span() })
        }
    }

    pub fn assert_assignable<F>(from: &Self, to: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if from.is_assignable_to(to) {
            Ok(())
        } else {
            Err(Error::UnableToAssignValueType {
                var_t: to.to_string(),
                value_t: from.to_string(),
                span: err_span(),
            })
        }
    }

    pub fn assert_comparable<F>(a: &Self, b: &Self, err_span: F) -> Result<()>
    where
        F: FnOnce() -> Span,
    {
        if a == b {
            Ok(())
        } else {
            Err(Error::UnableToCompareTypes {
                type_a: a.clone(),
                type_b: b.clone(),
                span: err_span(),
            })
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Self::Identifier(id) if id == "Int" || id == "Float")
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Identifier(id) if id == "Bool")
    }

    fn is_callable(&self) -> bool {
        matches!(self, Self::Callable { .. })
    }

    pub fn is_assignable_to(&self, other: &Type) -> bool {
        self == other
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Identifier(id) if id == "Void")
    }

    pub fn unwrap_callable(self) -> (Vec<Type>, Type) {
        if let Type::Callable { params_t, return_t } = self {
            (params_t, *return_t)
        } else {
            panic!("trying to unwrap callable on non-Callable type")
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "{id}"),
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
