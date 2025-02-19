use super::types::Type;
use logos::Span;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, SemanticError>;

#[derive(Debug, PartialEq)]
pub struct SemanticError {
    pub variant: SemanticErrorVariant,
    pub span: Span,
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

#[derive(Debug, PartialEq)]
pub enum SemanticErrorVariant {
    VoidTypeVariable,
    TypeMismatch { type_a: Type, type_b: Type },
    InvalidAssignmentType { var_t: Type, val_t: Type },
    InvalidLValue,
    UnexpectedStatement(String),
    SymbolAlreadyExist(String),
    SymbolDoesNotExist(String),
    NonNumberType,
    NonSignableNumberType,
    NonBooleanType,
    NonCallableType,
    NonIterableType,
    NonFieldAccessibleType,
    NonIndexableType,
    FieldDoesNotExist { t: Type, field: String },
    ArgumentLengthMismatch { expected: usize, get: usize },
    InvalidArguments { args_t: Vec<Type> },
    ReturnTypeMismatch { expected: Type, get: Type },
    UnexpectedVoidTypeExpression,
}

impl Display for SemanticErrorVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VoidTypeVariable { .. } => {
                write!(f, "unable to create variable with `void` type")
            }
            Self::InvalidAssignmentType { var_t, val_t, .. } => {
                write!(
                    f,
                    "unable to assign value of type `{val_t}` to type `{var_t}`"
                )
            }
            Self::InvalidLValue => {
                write!(f, "not a valid lvalue")
            }
            Self::SymbolAlreadyExist(sym) => {
                write!(f, "`{sym}` already exists in the current scope")
            }
            Self::SymbolDoesNotExist(sym) => {
                write!(f, "`{sym}` does not exist in the current scope")
            }
            Self::NonNumberType { .. } => {
                write!(f, "not a number")
            }
            Self::NonSignableNumberType => {
                write!(f, "not a signable number")
            }
            Self::NonBooleanType { .. } => {
                write!(f, "not a boolean")
            }
            Self::UnexpectedStatement(stmt_str) => {
                write!(f, "unexpected {stmt_str}")
            }
            Self::ArgumentLengthMismatch { expected, get, .. } => {
                write!(f, "expecting {expected} argument(s), but get {get} instead")
            }
            Self::InvalidArguments { args_t, .. } => {
                write!(
                    f,
                    "unable to call function with argument(s) of type {args_t:?}"
                )
            }
            Self::NonCallableType => {
                write!(f, "not a callable type")
            }
            Self::NonIterableType => {
                write!(f, "not an iterable type")
            }
            Self::NonFieldAccessibleType => {
                write!(f, "not a field-accessible type")
            }
            Self::NonIndexableType => {
                write!(f, "not an indexable type")
            }
            Self::FieldDoesNotExist { t, field } => {
                write!(f, "field `{field}` does not exist in type `{t}`")
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
                write!(f, "unexpected `void` type expression")
            }
        }
    }
}
