// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use super::types::Type;
use logos::Span;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnableToInferVariableType {
        id: String,
        span: Span,
    },

    UnableToAssignValueType {
        var_t: String,
        value_t: String,
        span: Span,
    },

    InvalidAssignmentLhs {
        lhs: String,
        span: Span,
    },

    UnableToCompareTypeAWithTypeB {
        type_a: Type,
        type_b: Type,
        span: Span,
    },

    VariableAlreadyExist {
        id: String,
        span: Span,
    },

    VariableNotExist {
        id: String,
        span: Span,
    },

    TypeNotExist {
        id: String,
        span: Span,
    },

    NotANumber {
        span: Span,
    },

    NotABoolean {
        span: Span,
    },

    NotAFunction {
        span: Span,
    },

    NotExpectingStatementInGlobal {
        span: Span,
    },

    FunctionDefinitionNotInGlobal {
        span: Span,
    },

    FunctionAlreadyExist {
        id: String,
        span: Span,
    },

    InvalidFunctionCallArgument {
        args: Vec<Type>,
        span: Span,
    },

    FunctionNotReturningValue {
        expect: Type,
        span: Span,
    },

    ReturnNotInFunctionScope {
        span: Span,
    },

    ReturnTypeMismatch {
        expect: Type,
        get: Type,
        span: Span,
    },

    LoopControlNotInLoopScope {
        span: Span,
    },
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::UnableToInferVariableType { span, .. }
            | Self::UnableToAssignValueType { span, .. }
            | Self::InvalidAssignmentLhs { span, .. }
            | Self::UnableToCompareTypeAWithTypeB { span, .. }
            | Self::VariableNotExist { span, .. }
            | Self::VariableAlreadyExist { span, .. }
            | Self::TypeNotExist { span, .. }
            | Self::NotANumber { span, .. }
            | Self::NotABoolean { span, .. }
            | Self::NotAFunction { span, .. }
            | Self::NotExpectingStatementInGlobal { span }
            | Self::FunctionDefinitionNotInGlobal { span, .. }
            | Self::FunctionAlreadyExist { span, .. }
            | Self::InvalidFunctionCallArgument { span, .. }
            | Self::FunctionNotReturningValue { span, .. }
            | Self::ReturnNotInFunctionScope { span, .. }
            | Self::ReturnTypeMismatch { span, .. }
            | Self::LoopControlNotInLoopScope { span } => span,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnableToInferVariableType { id, .. } => {
                write!(f, "unable to infer the type of variable `{id}` because of no type or initial value were provided")
            }
            Self::UnableToAssignValueType { var_t, value_t, .. } => {
                write!(
                    f,
                    "unable to assign value of type `{value_t}` to type `{var_t}`"
                )
            }
            Self::InvalidAssignmentLhs { lhs, .. } => {
                write!(f, "{lhs} can not be an assignment's lhs")
            }
            Self::UnableToCompareTypeAWithTypeB { type_a, type_b, .. } => {
                write!(
                    f,
                    "unable to compare the value of type `{type_a}` with type `{type_b}`"
                )
            }
            Self::VariableAlreadyExist { id, .. } => {
                write!(f, "variable `{id}` already exists in current scope")
            }
            Self::VariableNotExist { id, .. } => {
                write!(f, "variable `{id}` is not exist in current scope")
            }
            Self::TypeNotExist { id, .. } => {
                write!(f, "type `{id}` is not exist")
            }
            Self::NotANumber { .. } => {
                write!(f, "not a number")
            }
            Self::NotABoolean { .. } => {
                write!(f, "not a boolean")
            }
            Self::NotAFunction { .. } => {
                write!(f, "not a function")
            }
            Self::NotExpectingStatementInGlobal { .. } => {
                write!(f, "expecting only function definitions in global scope")
            }
            Self::FunctionDefinitionNotInGlobal { .. } => {
                write!(f, "unable to define functions in non-global scope")
            }
            Self::FunctionAlreadyExist { id, .. } => {
                write!(f, "function `{id}` is already exists")
            }
            Self::InvalidFunctionCallArgument { args, .. } => {
                write!(
                    f,
                    "unable to call function with argument(s) of type {:?}",
                    args
                )
            }
            Self::FunctionNotReturningValue { expect, .. } => {
                write!(
                    f,
                    "expecting function to return a value of type {expect}, but none was returned",
                )
            }
            Self::ReturnNotInFunctionScope { .. } => {
                write!(
                    f,
                    "the usage of `return` statement must be within a function body",
                )
            }
            Self::ReturnTypeMismatch { expect, get, .. } => {
                write!(
                    f,
                    "expecting to returning value of type `{expect}`, but get `{get}` instead",
                )
            }
            Self::LoopControlNotInLoopScope { .. } => {
                write!(
                    f,
                    "the usage of `break` and `continue` statements must be within a loop"
                )
            }
        }
    }
}
