use super::Type;
use crate::semantic::error::{Result, SemanticError, SemanticErrorVariant};
use logos::Span;

pub fn is_number<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if matches!(t, Type::Int(_)) || matches!(t, Type::Float(_)) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::NonNumberType,
            span: err_span(),
        })
    }
}

pub fn is_signable<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if matches!(t, Type::Int(_)) || matches!(t, Type::Float(_)) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::NonSignableNumberType,
            span: err_span(),
        })
    }
}

pub fn is_compatible<F>(a: &Type, b: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if a.is_compatible_with(b) {
        return Ok(());
    }

    Err(SemanticError {
        variant: SemanticErrorVariant::TypeMismatch {
            type_a: a.clone(),
            type_b: b.clone(),
        },
        span: err_span(),
    })
}

pub fn is_boolean<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if t == &Type::Bool {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::NonBooleanType,
            span: err_span(),
        })
    }
}

pub fn is_callable<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if matches!(t, Type::Callable { .. }) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::NonCallableType,
            span: err_span(),
        })
    }
}

pub fn is_iterable<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if matches!(t, Type::Array { .. }) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::NonIterableType,
            span: err_span(),
        })
    }
}

pub fn is_indexable<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if matches!(t, Type::Array { .. }) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::NonIndexableType,
            span: err_span(),
        })
    }
}

pub fn is_assignable<F>(from: &Type, to: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if from.is_assignable_to(to) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::InvalidAssignmentType {
                var_t: to.clone(),
                val_t: from.clone(),
            },
            span: err_span(),
        })
    }
}
