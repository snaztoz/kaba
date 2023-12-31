// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains representation of the AST of Kaba program.
//!
//! Root of the tree will always be the [`Program`] that may contains
//! `>= 0` statements.

use logos::Span;
use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

/// The root of a Kaba source code's AST.
#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<AstNode>,
}

/// The representation of each node that make up a whole Kaba AST.
#[derive(Debug, PartialEq)]
pub enum AstNode {
    VariableDeclaration {
        identifier: String,
        r#type: Option<String>,
        value: Option<Box<AstNode>>,
    },
    ValueAssignment {
        lhs: Box<AstNode>,
        value: Box<AstNode>,
    },
    FunctionCall {
        callee: Box<AstNode>,
        args: Vec<AstNode>,
    },

    Add(Box<AstNode>, Box<AstNode>),
    Sub(Box<AstNode>, Box<AstNode>),
    Mul(Box<AstNode>, Box<AstNode>),
    Div(Box<AstNode>, Box<AstNode>),

    Neg(Box<AstNode>),

    Identifier(String),

    Literal {
        value: Value,
        span: Span,
    },
}

/// The representation of each value that may exists in a Kaba
/// source code, such as integer or string.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f64),
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l + r),
                Value::Float(r) => Value::Float(f64::from(l) + r),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l + f64::from(r)),
                Value::Float(r) => Value::Float(l + r),
            },
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l - r),
                Value::Float(r) => Value::Float(f64::from(l) - r),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l - f64::from(r)),
                Value::Float(r) => Value::Float(l - r),
            },
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l * r),
                Value::Float(r) => Value::Float(f64::from(l) * r),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l * f64::from(r)),
                Value::Float(r) => Value::Float(l * r),
            },
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l / r),
                Value::Float(r) => Value::Float(f64::from(l) / r),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l / f64::from(r)),
                Value::Float(r) => Value::Float(l / r),
            },
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Integer(n) => Value::Integer(-n),
            Value::Float(n) => Value::Float(-n),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
        }
    }
}
