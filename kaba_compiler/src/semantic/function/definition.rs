use crate::ast::AstNode;
use crate::semantic::{
    body,
    error::{Result, SemanticError, SemanticErrorVariant},
    state::AnalyzerState,
    typ::Type,
};

pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let function_name = node.sym().sym_name();

    let return_t = if let Type::Callable { return_t, .. } = state
        .get_sym_variant(function_name)
        .unwrap()
        .clone()
        .into_entity_t()
    {
        return_t
    } else {
        unreachable!()
    };

    state.with_function_scope(node.id, *return_t.clone(), || {
        body::analyze(state, node)?;

        // Because the `body::analyze` only checks the encountered statements,
        // it is unable to detect the case where the body does not return
        // anything explicitly.
        //
        // For example:
        //
        // ```
        // def foo: int {
        //     return "no";
        // }
        // ```
        //
        // The analyzer is able to detect that the `return` statement above
        // is invalid (due to the types are not compatible).
        //
        // But in the case of:
        //
        // ```
        // def foo: int {}
        // ```
        //
        // The analyzer will miss this return type checking, because there is
        // no `return` statement at all inside the body.
        //
        // For this case, we add this checking here...

        let body_returned_t = state.take_returned_type();

        if !return_t.is_void() && body_returned_t.is_void() {
            return Err(SemanticError {
                variant: SemanticErrorVariant::ReturnTypeMismatch {
                    expected: *return_t,
                    get: Type::Void,
                },
                span: node.sym().span.clone(),
            });
        }

        Ok(())
    })
}
