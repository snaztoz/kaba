use crate::{
    ast::AstNode,
    semantic::{
        error::Result,
        expression,
        state::AnalyzerState,
        typ::{assert, Type},
    },
    AstNodeVariant,
};

/// Analyze index access expression.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let obj = unwrap_obj(node);
    let obj_t = expression::analyze(state, obj)?;
    assert::is_indexable(&obj_t, || obj.span.clone())?;

    let index = unwrap_index(node);
    let index_t = expression::analyze(state, index)?;
    assert::is_number(&index_t, || index.span.clone())?;

    match obj_t {
        Type::Array { elem_t } => Ok(*elem_t),

        _ => unreachable!(),
    }
}

fn unwrap_obj<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    if let AstNodeVariant::IndexAccess { object, .. } = &node.variant {
        object
    } else {
        unreachable!()
    }
}

fn unwrap_index<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    if let AstNodeVariant::IndexAccess { index, .. } = &node.variant {
        index
    } else {
        unreachable!()
    }
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
