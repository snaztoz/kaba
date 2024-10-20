// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use super::{error::Error, Result};
use crate::ast::{AstNode, Value};
use logos::Span;
use std::{collections::BTreeMap, fmt::Display, str::FromStr};

pub type CallableSignature = (CallableParameters, Type);

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    Callable {
        params: CallableParameters,
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

    pub fn from_type_notation_node(tn: &AstNode) -> Result<Self> {
        let (id, span) = tn.unwrap_type_notation();
        Type::from_str(&id).map_err(|_| Error::TypeNotExist { id, span })
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
            Self::Callable { params, return_t } => {
                let params_t = params
                    .types()
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "({}) -> {}", params_t, return_t)
            }
        }
    }
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "Int" => Ok(Self::Int),
            "Float" => Ok(Self::Float),
            "Bool" => Ok(Self::Bool),

            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CallableParameters {
    ps: BTreeMap<String, Type>,
}

impl CallableParameters {
    pub fn from_ast_node_pairs(pairs: &[(AstNode, AstNode)]) -> Result<Self> {
        let mut ps = BTreeMap::new();

        for (i, t) in pairs.iter() {
            let (id, span) = i.unwrap_identifier();
            if ps.contains_key(&id) {
                return Err(Error::VariableAlreadyExist { id, span });
            }

            let t = Type::from_type_notation_node(t)?;

            ps.insert(id, t.clone());
        }

        Ok(Self { ps })
    }

    pub fn pairs(&self) -> Vec<(String, Type)> {
        self.ps
            .iter()
            .map(|(id, t)| (id.clone(), t.clone()))
            .collect()
    }

    pub fn types(&self) -> Vec<Type> {
        self.ps.values().cloned().collect::<Vec<_>>()
    }
}
