use crate::{
    ast::AstNode,
    semantic::{
        error::Result,
        expression,
        state::AnalyzerState,
        typ::{assert, Type},
    },
};
use std::borrow::Cow;

/// Analyze index access expression.
pub fn analyze<'a>(state: &'a AnalyzerState, node: &AstNode) -> Result<Cow<'a, Type>> {
    let obj = node.variant.as_accessed_object();
    let obj_t = expression::analyze(state, obj)?;
    assert::is_indexable(&obj_t, || obj.span.clone())?;

    let index = node.variant.as_accessed_field();
    let index_t = expression::analyze(state, index)?;
    assert::is_number(&index_t, || index.span.clone())?;

    Ok(match obj_t {
        Cow::Borrowed(t) => Cow::Borrowed(t.as_array_elem_t()),
        Cow::Owned(t) => Cow::Owned(t.into_array_elem_t()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::{test_util::assert_expr_type, typ::IntType};

    #[test]
    fn index_accessing() {
        assert_expr_type(
            "[[]int [int 1, 2]][0];",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Int(IntType::Int)),
            },
        );
    }
}
