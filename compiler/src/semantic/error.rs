use super::types::Type;
use logos::Span;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    VoidTypeVariable {
        span: Span,
    },

    TypeMismatch {
        type_a: Type,
        type_b: Type,
        span: Span,
    },

    UnexpectedStatement {
        stmt_str: String,
        span: Span,
    },

    InvalidAssignmentType {
        var_t: String,
        val_t: String,
        span: Span,
    },

    InvalidAssignmentLhs {
        lhs: String,
        span: Span,
    },

    UnableToInferType {
        span: Span,
    },

    SymbolAlreadyExist {
        id: String,
        span: Span,
    },

    SymbolDoesNotExist {
        id: String,
        span: Span,
    },

    NonNumberType {
        span: Span,
    },

    NonSignableNumberType {
        t: Type,
        span: Span,
    },

    NonBooleanType {
        span: Span,
    },

    NonCallableType {
        t: Type,
        span: Span,
    },

    NonIterableType {
        t: Type,
        span: Span,
    },

    NonIndexableType {
        t: Type,
        span: Span,
    },

    InvalidFunctionCallArgument {
        args: Vec<Type>,
        span: Span,
    },

    ReturnTypeMismatch {
        expected: Type,
        get: Type,
        span: Span,
    },

    UnexpectedVoidTypeExpression {
        span: Span,
    },
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::VoidTypeVariable { span, .. }
            | Self::InvalidAssignmentType { span, .. }
            | Self::InvalidAssignmentLhs { span, .. }
            | Self::UnableToInferType { span }
            | Self::SymbolAlreadyExist { span, .. }
            | Self::SymbolDoesNotExist { span, .. }
            | Self::NonNumberType { span, .. }
            | Self::NonSignableNumberType { span, .. }
            | Self::NonBooleanType { span, .. }
            | Self::UnexpectedStatement { span, .. }
            | Self::InvalidFunctionCallArgument { span, .. }
            | Self::NonCallableType { span, .. }
            | Self::NonIterableType { span, .. }
            | Self::NonIndexableType { span, .. }
            | Self::TypeMismatch { span, .. }
            | Self::ReturnTypeMismatch { span, .. }
            | Self::UnexpectedVoidTypeExpression { span } => span,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VoidTypeVariable { .. } => {
                write!(f, "unable to create variable with `Void` type")
            }
            Self::InvalidAssignmentType { var_t, val_t, .. } => {
                write!(
                    f,
                    "unable to assign value of type `{val_t}` to type `{var_t}`"
                )
            }
            Self::InvalidAssignmentLhs { lhs, .. } => {
                write!(f, "{lhs} can not be an assignment's lhs")
            }
            Self::UnableToInferType { .. } => {
                write!(f, "unable to infer type")
            }
            Self::SymbolAlreadyExist { id, .. } => {
                write!(f, "`{id}` already exists in current scope")
            }
            Self::SymbolDoesNotExist { id, .. } => {
                write!(f, "`{id}` does not exist in current scope")
            }
            Self::NonNumberType { .. } => {
                write!(f, "not a number")
            }
            Self::NonSignableNumberType { t, .. } => {
                write!(f, "not a signable number: `{t}`")
            }
            Self::NonBooleanType { .. } => {
                write!(f, "not a boolean")
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
            Self::NonCallableType { t, .. } => {
                write!(f, "unable to call a non-callable type: `{t}`")
            }
            Self::NonIterableType { t, .. } => {
                write!(f, "unable to iterate over non-iterable type: `{t}`")
            }
            Self::NonIndexableType { t, .. } => {
                write!(
                    f,
                    "unable to access the index of a non-indexable type: `{t}`"
                )
            }
            Self::TypeMismatch { type_a, type_b, .. } => {
                write!(f, "type mismatch: `{type_a}` and `{type_b}`",)
            }
            Self::ReturnTypeMismatch { expected, get, .. } => {
                write!(
                    f,
                    "expecting function to returns `{expected}`, but get `{get}` instead",
                )
            }
            Self::UnexpectedVoidTypeExpression { .. } => {
                write!(f, "unexpected `Void` type expression")
            }
        }
    }
}
