use crate::{
    ast::AstNode,
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        expression,
        state::AnalyzerState,
        typ::{assert, Type},
    },
};

/// Analyze index access expression.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let obj = node.variant.as_accessed_object();
    let obj_t = expression::analyze(state, obj)?;
    assert::is_field_accessible(&obj_t, || obj.span.clone())?;

    let field = node.variant.as_accessed_field();
    let field_name = field.variant.as_sym_name();
    let field_t = obj_t.get_field(field_name);

    if field_t.is_none() {
        return Err(SemanticError {
            variant: SemanticErrorVariant::FieldDoesNotExist {
                t: obj_t,
                field: String::from(field_name),
            },
            span: field.span.clone(),
        });
    }

    Ok(field_t.unwrap().clone())
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
