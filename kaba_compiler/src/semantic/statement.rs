use super::{
    conditional, each_loop,
    error::{Result, SemanticError, SemanticErrorVariant},
    expression,
    state::AnalyzerState,
    types::{assert, Type},
    variable, while_loop,
};
use crate::ast::AstNode;

/// Analyze a statement.
///
/// It analyzes simple statements, such as loop control analyzeing, and also
/// acts as an aggregate for another (more specific) statement analyzers, such
/// as the AssignmentAnalyzer.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    match node {
        AstNode::VariableDeclaration { .. } => variable::analyze(state, node),
        AstNode::If { .. } => conditional::analyze(state, node),
        AstNode::While { .. } => while_loop::analyze(state, node),
        AstNode::Each { .. } => each_loop::analyze(state, node),
        AstNode::Break { .. } | AstNode::Continue { .. } => analyze_loop_control(state, node),

        AstNode::FunctionDefinition { sym, .. } => Err(SemanticError {
            variant: SemanticErrorVariant::UnexpectedStatement(node.to_string()),
            span: sym.span().clone(),
        }),

        AstNode::Return { .. } => analyze_return(state, node),
        AstNode::Debug { .. } => analyze_debug(state, node),

        expr => expression::analyze(state, expr),
    }
}

fn analyze_loop_control(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let span = match node {
        AstNode::Break { span, .. } | AstNode::Continue { span, .. } => span,
        _ => unreachable!(),
    };

    if !state.is_inside_loop() {
        return Err(SemanticError {
            variant: SemanticErrorVariant::UnexpectedStatement(node.to_string()),
            span: span.clone(),
        });
    }

    Ok(Type::Void)
}

fn analyze_return(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let (expr, span) = if let AstNode::Return { expr, span } = node {
        (expr, span)
    } else {
        unreachable!()
    };

    let expr_t = expr
        .as_ref()
        .map(|expr| expression::analyze(state, expr))
        .unwrap_or(Ok(Type::Void))?;

    let return_t = state.nearest_return_t().ok_or_else(|| SemanticError {
        variant: SemanticErrorVariant::UnexpectedStatement(node.to_string()),
        span: span.clone(),
    })?;

    assert::is_assignable(&expr_t, &return_t, || match expr {
        Some(expr) => expr.span().clone(),
        None => span.clone(),
    })
    .map_err(|err| SemanticError {
        variant: SemanticErrorVariant::ReturnTypeMismatch {
            expected: return_t.clone(),
            get: expr_t,
        },
        ..err
    })?;

    Ok(return_t)
}

fn analyze_debug(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let expr = if let AstNode::Debug { expr, .. } = node {
        expr
    } else {
        unreachable!()
    };

    let expr_t = expression::analyze(state, expr)?;
    if expr_t == Type::Void {
        return Err(SemanticError {
            variant: SemanticErrorVariant::UnexpectedVoidTypeExpression,
            span: expr.span().clone(),
        });
    }

    Ok(Type::Void)
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn debug_expression() {
        assert_is_ok(indoc! {"
                def main {
                    debug 17 * 5;
                }
            "});
    }

    #[test]
    fn debug_expression_with_void_type() {
        assert_is_err(indoc! {"
                def main {
                    debug this_is_void();
                }

                def this_is_void {}
            "});
    }
}
