use crate::{
    ast::AstNode,
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        expression,
        state::AnalyzerState,
        typ::{assert, check, Type},
    },
};

/// Analyze function call expression.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let fn_t = expression::analyze(state, node.variant.as_accessed_object())?;
    assert::is_callable(&fn_t, || node.variant.as_accessed_object().span.clone())?;

    let args_t = to_args_t(state, node)?;
    let (params_t, return_t) = fn_t.into_callable_signature_t();

    if params_t.len() != args_t.len() {
        return Err(SemanticError {
            variant: SemanticErrorVariant::ArgumentLengthMismatch {
                expected: params_t.len(),
                get: args_t.len(),
            },
            span: node.span.clone(),
        });
    }

    for (param_t, arg_t) in params_t.iter().zip(&args_t) {
        if !check::is_type_assignable_to(arg_t, param_t) {
            return Err(SemanticError {
                variant: SemanticErrorVariant::InvalidArguments { args_t },
                span: node.span.clone(),
            });
        }
    }

    Ok(return_t)
}

// Transform arguments into their respective type
fn to_args_t(state: &AnalyzerState, node: &AstNode) -> Result<Vec<Type>> {
    let mut args_t = vec![];
    for arg in node.variant.as_function_call_args() {
        let t = expression::analyze(state, arg)?;
        args_t.push(t);
    }
    Ok(args_t)
}
