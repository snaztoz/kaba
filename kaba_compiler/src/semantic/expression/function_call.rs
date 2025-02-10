use crate::{
    ast::AstNode,
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        expression,
        state::AnalyzerState,
        types::{assert, Type},
    },
};

/// Analyze function call expression.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let fn_t = expression::analyze(state, unwrap_callee(node))?;
    assert::is_callable(&fn_t, || unwrap_callee(node).span().clone())?;

    let args_t = get_args_t(state, node)?;
    let (params_t, return_t) = fn_t.unwrap_callable();

    if params_t.len() != args_t.len() {
        return Err(SemanticError {
            variant: SemanticErrorVariant::ArgumentLengthMismatch {
                expected: params_t.len(),
                get: args_t.len(),
            },
            span: node.span().clone(),
        });
    }

    for (param_t, arg_t) in params_t.iter().zip(&args_t) {
        if !arg_t.is_assignable_to(param_t) {
            return Err(SemanticError {
                variant: SemanticErrorVariant::InvalidArguments { args_t },
                span: node.span().clone(),
            });
        }
    }

    Ok(return_t)
}

// Transform arguments into their respective type
fn get_args_t(state: &AnalyzerState, node: &AstNode) -> Result<Vec<Type>> {
    let mut args_t = vec![];
    for arg in unwrap_args(node) {
        let t = expression::analyze(state, arg)?;
        args_t.push(t);
    }
    Ok(args_t)
}

fn unwrap_callee<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    if let AstNode::FunctionCall { callee, .. } = node {
        callee
    } else {
        unreachable!()
    }
}

fn unwrap_args<'src, 'a>(node: &'a AstNode<'src>) -> &'a [AstNode<'src>] {
    if let AstNode::FunctionCall { args, .. } = node {
        args
    } else {
        unreachable!()
    }
}
