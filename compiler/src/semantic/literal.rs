use super::{
    error::Result,
    typ::{FloatType, IntType, Type},
};
use crate::{ast::Literal, AstNode};
use std::borrow::Cow;

pub fn analyze<'a>(node: &AstNode) -> Result<Cow<'a, Type>> {
    let lit = node.variant.as_literal();

    let t = match lit {
        Literal::Void => Type::Void,

        Literal::Bool(_) => Type::Bool,
        Literal::Char(_) => Type::Char,
        Literal::Float(n) => Type::Float(FloatType::Unbounded(*n)),
        Literal::Int(n) => Type::Int(IntType::Unbounded(*n)),
        Literal::String(_) => Type::String,
    };

    Ok(Cow::Owned(t))
}
