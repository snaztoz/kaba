// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<AstNode>,
}

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

    Identifier(String),

    Val(Value),
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
}
