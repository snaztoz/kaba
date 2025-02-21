use super::{
    conditional, each_loop,
    error::{Result, SemanticError, SemanticErrorVariant},
    expression,
    state::AnalyzerState,
    typ::{assert, Type},
    variable, while_loop,
};
use crate::ast::{AstNode, AstNodeVariant};

/// Analyze a statement.
///
/// It analyzes simple statements, such as loop control analyzeing, and also
/// acts as an aggregate for another (more specific) statement analyzers, such
/// as the AssignmentAnalyzer.
pub fn analyze(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    match &node.variant {
        AstNodeVariant::VariableDeclaration { .. } => variable::analyze(state, node),
        AstNodeVariant::If { .. } => conditional::analyze(state, node),
        AstNodeVariant::While { .. } => while_loop::analyze(state, node),
        AstNodeVariant::Each { .. } => each_loop::analyze(state, node),
        AstNodeVariant::Break { .. } | AstNodeVariant::Continue { .. } => {
            analyze_loop_control(state, node)
        }

        // Don't allow function and record definitions in non-global scope.
        AstNodeVariant::FunctionDefinition { sym, .. }
        | AstNodeVariant::RecordDefinition { sym, .. } => Err(SemanticError {
            variant: SemanticErrorVariant::UnexpectedStatement(node.to_string()),
            span: sym.span.clone(),
        }),

        AstNodeVariant::Return { .. } => analyze_return(state, node),
        AstNodeVariant::Debug { .. } => analyze_debug(state, node),

        _ => {
            expression::analyze(state, node)?;
            Ok(())
        }
    }
}

fn analyze_loop_control(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    if !state.is_inside_loop() {
        return Err(SemanticError {
            variant: SemanticErrorVariant::UnexpectedStatement(node.to_string()),
            span: node.span.clone(),
        });
    }

    Ok(())
}

fn analyze_return(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    let expr = if let AstNodeVariant::Return { expr } = &node.variant {
        expr
    } else {
        unreachable!()
    };

    let expr_t = expr
        .as_ref()
        .map(|expr| expression::analyze(state, expr))
        .unwrap_or(Ok(Type::Void))?;

    let return_t = state.nearest_return_t().ok_or_else(|| SemanticError {
        variant: SemanticErrorVariant::UnexpectedStatement(node.to_string()),
        span: node.span.clone(),
    })?;

    assert::is_assignable(&expr_t, &return_t, || match expr {
        Some(expr) => expr.span.clone(),
        None => node.span.clone(),
    })
    .map_err(|err| SemanticError {
        variant: SemanticErrorVariant::ReturnTypeMismatch {
            expected: return_t.clone(),
            get: expr_t,
        },
        ..err
    })?;

    state.set_returned_type(return_t);

    Ok(())
}

fn analyze_debug(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let expr = if let AstNodeVariant::Debug { expr, .. } = &node.variant {
        expr
    } else {
        unreachable!()
    };

    let expr_t = expression::analyze(state, expr)?;
    if expr_t == Type::Void {
        return Err(SemanticError {
            variant: SemanticErrorVariant::UnexpectedVoidTypeExpression,
            span: expr.span.clone(),
        });
    }

    Ok(())
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
