// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! Contains the built-in types of Kaba programming language
//! implementation.

use std::{fmt::Display, str::FromStr};

#[derive(Clone, Debug, PartialEq)]
pub enum Types {
    Any,
    Void,
    Int,
    Float,
    Bool,
    Callable {
        parameters: Vec<Types>,
        return_type: Box<Types>,
    },
}

impl Types {
    pub fn is_number(&self) -> bool {
        *self == Self::Int || *self == Self::Float
    }

    pub fn is_boolean(&self) -> bool {
        *self == Self::Bool
    }

    pub fn is_assignable_to(&self, other: &Types) -> bool {
        if *self == Self::Void || *other == Self::Void {
            return false;
        }

        (self == other) || (*other == Self::Any) || (*self == Self::Int && *other == Self::Float)
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "Any"),
            Self::Void => write!(f, "Void"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::Callable { .. } => write!(f, "Callable"),
        }
    }
}

impl FromStr for Types {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Any" => Ok(Self::Any),
            "Void" => Ok(Self::Void),
            "Int" => Ok(Self::Int),
            "Float" => Ok(Self::Float),
            "Bool" => Ok(Self::Bool),

            _ => Err(()),
        }
    }
}
