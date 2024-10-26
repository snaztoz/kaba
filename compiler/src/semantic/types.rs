use super::{error::Error, Result};
use crate::ast::{AstNode, TypeNotation, Value};
use logos::Span;
use std::fmt::Display;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    Callable {
        params_t: Vec<Type>,
        return_t: Box<Type>,
    },
}

impl Type {
    pub fn from_value(val: &Value) -> Result<Self> {
        match val {
            Value::Void => Ok(Self::Void),
            Value::Integer(_) => Ok(Self::Int),
            Value::Float(_) => Ok(Self::Float),
            Value::Boolean(_) => Ok(Self::Bool),
        }
    }

    pub fn from_type_notation(tn: &AstNode) -> Result<Self> {
        if let AstNode::TypeNotation { tn, span } = tn {
            match tn {
                TypeNotation::Identifier(id) if id == "Void" => Ok(Self::Void),
                TypeNotation::Identifier(id) if id == "Int" => Ok(Self::Int),
                TypeNotation::Identifier(id) if id == "Float" => Ok(Self::Float),
                TypeNotation::Identifier(id) if id == "Bool" => Ok(Self::Bool),

                TypeNotation::Callable {
                    params_tn,
                    return_tn,
                } => {
                    let mut params_t = vec![];
                    for tn in params_tn {
                        params_t.push(Self::from_type_notation(tn)?);
                    }
                    let return_t = Box::new(Self::from_type_notation(return_tn)?);
                    Ok(Self::Callable { params_t, return_t })
                }

                // TODO: Custom Type
                TypeNotation::Identifier(id) => Err(Error::TypeNotExist {
                    id: id.clone(),
                    span: span.clone(),
                }),
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
            Err(Error::UnableToCompareTypeAWithTypeB {
                type_a: a.clone(),
                type_b: b.clone(),
                span: err_span(),
            })
        }
    }

    pub fn is_number(&self) -> bool {
        *self == Self::Int || *self == Self::Float
    }

    pub fn is_boolean(&self) -> bool {
        *self == Self::Bool
    }

    pub fn is_assignable_to(&self, other: &Type) -> bool {
        self == other
    }

    pub fn is_void(&self) -> bool {
        *self == Self::Void
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::Callable { params_t, return_t } => {
                let params_t = params_t
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "({}) -> {}", params_t, return_t)
            }
        }
    }
}
