use crate::ast::AstNode;
use crate::semantic::{
    body,
    error::{Result, SemanticError, SemanticErrorVariant},
    state::AnalyzerState,
    types::Type,
};

pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let function_name = node.sym().sym_name();
    let return_t = if let Type::Callable { return_t, .. } = state
        .get_sym_t(function_name)
        .unwrap()
        .clone()
        .into_entity_t()
    {
        return_t
    } else {
        unreachable!()
    };

    state.with_function_scope(node.id, *return_t.clone(), || {
        let body_t = body::analyze(state, node)?;

        if !return_t.is_void() && body_t.is_void() {
            return Err(SemanticError {
                variant: SemanticErrorVariant::ReturnTypeMismatch {
                    expected: *return_t,
                    get: Type::Void,
                },
                span: node.sym().span.clone(),
            });
        }

        Ok(())
    })?;

    Ok(Type::Void)
}
