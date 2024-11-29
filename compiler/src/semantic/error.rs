use super::types::Type;
use logos::Span;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    VoidTypeVariable {
        span: Span,
    },

    UnableToAssignValueType {
        var_t: String,
        val_t: String,
        span: Span,
    },

    InvalidAssignmentLhs {
        lhs: String,
        span: Span,
    },

    UnableToCompareTypes {
        type_a: Type,
        type_b: Type,
        span: Span,
    },

    AlreadyExist {
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

    UnexpectedStatement {
        stmt_str: String,
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

    ReturnTypeMismatch {
        expect: Type,
        get: Type,
        span: Span,
    },

    DebugVoid {
        span: Span,
    },
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::VoidTypeVariable { span, .. }
            | Self::UnableToAssignValueType { span, .. }
            | Self::InvalidAssignmentLhs { span, .. }
            | Self::UnableToCompareTypes { span, .. }
            | Self::VariableNotExist { span, .. }
            | Self::AlreadyExist { span, .. }
            | Self::TypeNotExist { span, .. }
            | Self::NotANumber { span, .. }
            | Self::NotABoolean { span, .. }
            | Self::NotAFunction { span, .. }
            | Self::UnexpectedStatement { span, .. }
            | Self::InvalidFunctionCallArgument { span, .. }
            | Self::FunctionNotReturningValue { span, .. }
            | Self::ReturnTypeMismatch { span, .. }
            | Self::DebugVoid { span } => span,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VoidTypeVariable { .. } => {
                write!(f, "unable to create variable with `Void` type")
            }
            Self::UnableToAssignValueType { var_t, val_t, .. } => {
                write!(
                    f,
                    "unable to assign value of type `{val_t}` to type `{var_t}`"
                )
            }
            Self::InvalidAssignmentLhs { lhs, .. } => {
                write!(f, "{lhs} can not be an assignment's lhs")
            }
            Self::UnableToCompareTypes { type_a, type_b, .. } => {
                write!(
                    f,
                    "unable to compare the value of type `{type_a}` with type `{type_b}`"
                )
            }
            Self::AlreadyExist { id, .. } => {
                write!(f, "`{id}` already exists in current scope")
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
            Self::UnexpectedStatement { stmt_str, .. } => {
                write!(f, "unexpected {stmt_str}")
            }
            Self::InvalidFunctionCallArgument { args, .. } => {
                write!(
                    f,
                    "unable to call function with argument(s) of type {args:?}"
                )
            }
            Self::FunctionNotReturningValue { expect, .. } => {
                write!(
                    f,
                    "expecting function to return a value of type {expect}, but none was returned",
                )
            }
            Self::ReturnTypeMismatch { expect, get, .. } => {
                write!(
                    f,
                    "expecting to returning value of type `{expect}`, but get `{get}` instead",
                )
            }
            Self::DebugVoid { .. } => {
                write!(f, "unable to debug expressions that has `void` type")
            }
        }
    }
}
