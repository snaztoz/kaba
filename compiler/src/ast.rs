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
        identifier: Box<AstNode>,
        var_type: Option<Box<AstNode>>,
        value: Option<Box<AstNode>>,
        span: Span,
    },
    ValueAssignment {
        lhs: Box<AstNode>,
        value: Box<AstNode>,
        span: Span,
    },

    Add {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },
    Sub {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },
    Mul {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },
    Div {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Neg {
        child: Box<AstNode>,
        span: Span,
    },
    FunctionCall {
        callee: Box<AstNode>,
        args: Vec<AstNode>,
        span: Span,
    },

    // This variant only be used as the span information holder and
    // then should be removed. It won't be present in the final
    // resulting ASTs.
    Group {
        child: Box<AstNode>,
        span: Span,
    },

    Identifier {
        name: String,
        span: Span,
    },

    TypeNotation {
        name: String,
        span: Span,
    },

    Literal {
        value: Value,
        span: Span,
    },
}

impl AstNode {
    pub fn get_span(&self) -> Span {
        match self {
            Self::VariableDeclaration { span, .. }
            | Self::ValueAssignment { span, .. }
            | Self::Add { span, .. }
            | Self::Sub { span, .. }
            | Self::Mul { span, .. }
            | Self::Div { span, .. }
            | Self::Neg { span, .. }
            | Self::FunctionCall { span, .. }
            | Self::Group { span, .. }
            | Self::Identifier { span, .. }
            | Self::TypeNotation { span, .. }
            | Self::Literal { span, .. } => span.clone(),
        }
    }

    pub fn unwrap_identifier(&self) -> (String, Span) {
        if let AstNode::Identifier { name, span } = self {
            (name.clone(), span.clone())
        } else {
            panic!(
                "calling `unwrap_identifier` on non-identifier AstNode: {:?}",
                self
            )
        }
    }

    pub fn unwrap_type_notation(&self) -> (String, Span) {
        if let AstNode::TypeNotation { name, span } = self {
            (name.clone(), span.clone())
        } else {
            panic!(
                "calling `unwrap_type_notation` on non-type notation AstNode: {:?}",
                self
            )
        }
    }

    pub fn unwrap_group(self) -> AstNode {
        if let AstNode::Group { child, .. } = self {
            // unwrap recursively
            child.unwrap_group()
        } else {
            self
        }
    }
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
