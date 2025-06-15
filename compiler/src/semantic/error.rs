use super::typ::Type;
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
    ArgumentLengthMismatch { expected: usize, get: usize },
    FieldDoesNotExist { t: Type, field: String },
    InvalidArguments { args_t: Vec<Type> },
    InvalidAssignmentType { var_t: Type, val_t: Type },
    InvalidLValue,
    KeyvalInitializerInArrayCreation,
    KeyvalInitializerWithNonSymbolKey,
    MissingRecordFields(Vec<String>),
    NonBooleanType,
    NonCallableType,
    NonFieldAccessibleType,
    NonIndexableType,
    NonIterableType,
    NonNumberType,
    NonSignableNumberType,
    ReturnTypeMismatch { expected: Type, get: Type },
    SymbolAlreadyExist(String),
    SymbolDoesNotExist(String),
    TypeMismatch { type_a: Type, type_b: Type },
    UnexpectedStatement(String),
    UnexpectedVoidTypeExpression,
    VoidTypeVariable,
}

impl Display for SemanticErrorVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArgumentLengthMismatch { expected, get, .. } => {
                write!(f, "expecting {expected} argument(s), but get {get} instead")
            }
            Self::FieldDoesNotExist { t, field } => {
                write!(f, "field `{field}` does not exist in type `{t}`")
            }
            Self::InvalidArguments { args_t, .. } => {
                let joined = args_t
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "unable to call function with argument(s) of type [{joined}]"
                )
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
            Self::KeyvalInitializerInArrayCreation => {
                write!(
                    f,
                    "array creations must use empty / array initializer syntax"
                )
            }
            Self::KeyvalInitializerWithNonSymbolKey => {
                write!(f, "key-val initializer must uses symbols as its keys")
            }
            Self::MissingRecordFields(fields) => {
                let joined = fields
                    .iter()
                    .map(|f| format!("`{f}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "missing initializer field(s): {joined}")
            }
            Self::NonBooleanType { .. } => {
                write!(f, "not a boolean")
            }
            Self::NonCallableType => {
                write!(f, "not a callable type")
            }
            Self::NonFieldAccessibleType => {
                write!(f, "not a field-accessible type")
            }
            Self::NonIndexableType => {
                write!(f, "not an indexable type")
            }
            Self::NonIterableType => {
                write!(f, "not an iterable type")
            }
            Self::NonNumberType { .. } => {
                write!(f, "not a number")
            }
            Self::NonSignableNumberType => {
                write!(f, "not a signable number")
            }
            Self::ReturnTypeMismatch { expected, get, .. } => {
                write!(
                    f,
                    "expecting function to returns `{expected}`, but get `{get}` instead",
                )
            }
            Self::SymbolAlreadyExist(sym) => {
                write!(f, "`{sym}` already exists in the current scope")
            }
            Self::SymbolDoesNotExist(sym) => {
                write!(f, "`{sym}` does not exist in the current scope")
            }
            Self::TypeMismatch { type_a, type_b, .. } => {
                write!(f, "type mismatch: `{type_a}` and `{type_b}`",)
            }
            Self::UnexpectedStatement(stmt_str) => {
                write!(f, "unexpected {stmt_str}")
            }
            Self::UnexpectedVoidTypeExpression { .. } => {
                write!(f, "unexpected `void` type expression")
            }
            Self::VoidTypeVariable { .. } => {
                write!(f, "unable to create variable with `void` type")
            }
        }
    }
}
