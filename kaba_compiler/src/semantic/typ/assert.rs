use super::{check, Type};
use crate::semantic::{
    error::{Result, SemanticError, SemanticErrorVariant},
    state::AnalyzerState,
};
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
    if check::are_types_compatible(a, b) {
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

pub fn is_field_accessible<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if matches!(t, Type::Record { .. }) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::NonFieldAccessibleType,
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

pub fn is_assignable<F>(t: &Type, to: &Type, state: &AnalyzerState, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if check::is_type_assignable_to(t, to, state) {
        Ok(())
    } else {
        Err(SemanticError {
            variant: SemanticErrorVariant::InvalidAssignmentType {
                var_t: to.clone(),
                val_t: t.clone(),
            },
            span: err_span(),
        })
    }
}
