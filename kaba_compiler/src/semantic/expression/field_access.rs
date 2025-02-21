use crate::{
    ast::AstNode,
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
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
    assert::is_field_accessible(&obj_t, || obj.span.clone())?;

    let field = node.variant.as_accessed_field();
    let field_name = field.variant.as_sym_name();

    if !obj_t.as_record_fields().contains_key(field_name) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::FieldDoesNotExist {
                t: obj_t.into_owned(),
                field: String::from(field_name),
            },
            span: field.span.clone(),
        });
    }

    Ok(match obj_t {
        Cow::Borrowed(t) => Cow::Borrowed(t.as_record_fields().get(field_name).unwrap()),
        Cow::Owned(t) => Cow::Owned(t.into_record_fields().remove(field_name).unwrap()),
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
