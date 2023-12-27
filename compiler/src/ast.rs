// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains representation of the AST of Kaba program.
//!
//! Root of the tree will always be the [`Program`] that may contains
//! `>= 0` statements.

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

    Negation(Box<AstNode>),

    Identifier(String),

    Val(Value),
}

/// The representation of each value that may exists in a Kaba
/// source code, such as integer or string.
#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
}
