use super::Type;
use crate::semantic::error::{Error, Result};
use logos::Span;

pub fn is_number<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if Type::numbers().contains(t) {
        Ok(())
    } else {
        Err(Error::NonNumberType { span: err_span() })
    }
}

pub fn is_signable<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if Type::numbers().contains(t) {
        Ok(())
    } else {
        Err(Error::NonSignableNumberType {
            t: t.clone(),
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

    Err(Error::TypeMismatch {
        type_a: a.clone(),
        type_b: b.clone(),
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
        Err(Error::NonBooleanType { span: err_span() })
    }
}

pub fn is_callable<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if matches!(t, Type::Callable { .. }) {
        Ok(())
    } else {
        Err(Error::NonCallableType {
            t: t.clone(),
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
        Err(Error::NonIterableType {
            t: t.clone(),
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
        Err(Error::NonIndexableType {
            t: t.clone(),
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
        Err(Error::InvalidAssignmentType {
            var_t: to.to_string(),
            val_t: from.to_string(),
            span: err_span(),
        })
    }
}
