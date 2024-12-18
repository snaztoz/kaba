use super::Type;
use crate::semantic::error::{Error, Result};
use logos::Span;

pub fn is_number<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if t.is_number() {
        Ok(())
    } else {
        Err(Error::NonNumberType { span: err_span() })
    }
}

pub fn is_signable<F>(t: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if t.is_signable() {
        Ok(())
    } else {
        Err(Error::NonSignableNumberType {
            t: t.clone(),
            span: err_span(),
        })
    }
}

pub fn is_compatible<F>(type_a: &Type, type_b: &Type, err_span: F) -> Result<()>
where
    F: FnOnce() -> Span,
{
    if type_a == type_b || type_a.is_promotable_to(type_b) || type_b.is_promotable_to(type_a) {
        return Ok(());
    }

    Err(Error::TypeMismatch {
        type_a: type_a.clone(),
        type_b: type_b.clone(),
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
    if t.is_callable() {
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
    if t.is_array() {
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
    if t.is_array() {
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
