// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use std::{fmt::Display, str::FromStr};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    Callable {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl Type {
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
            Self::Callable {
                parameter_types, ..
            } => {
                let pts: Vec<_> = parameter_types.iter().map(|pt| pt.to_string()).collect();
                write!(f, "/{}", pts.join(","))
            }
        }
    }
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Int" => Ok(Self::Int),
            "Float" => Ok(Self::Float),
            "Bool" => Ok(Self::Bool),

            _ => Err(()),
        }
    }
}
