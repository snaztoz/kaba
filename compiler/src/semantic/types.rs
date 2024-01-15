// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use std::{collections::BTreeSet, fmt::Display, str::FromStr};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    Callable {
        parameter_variants: BTreeSet<Vec<Type>>,
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
        if *self == Self::Void || *other == Self::Void {
            return false;
        }

        self == other
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::Callable { .. } => write!(f, "Callable"),
        }
    }
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Void" => Ok(Self::Void),
            "Int" => Ok(Self::Int),
            "Float" => Ok(Self::Float),
            "Bool" => Ok(Self::Bool),

            _ => Err(()),
        }
    }
}
