use super::{
    body,
    error::Result,
    expression,
    state::{AnalyzerState, ScopeVariant},
    typ::{assert, Type},
};
use crate::{ast::AstNode, AstNodeVariant};

/// Analyze conditional branch statement.
///
/// ### ✅ Valid Examples
///
/// * The provided condition must be an expression that returns a boolean:
///
/// ```text
/// if true {
///     // ...
/// }
/// ```
///
/// * This statement can have multiple branches:
///
/// ```text
/// if false {
///     // Won't be executed
/// } else if !true {
///     // Won't be executed
/// } else {
///     // Will be executed
/// }
/// ```
///
/// * It can be the last statement of a function:
///
/// ```text
/// def foo(): int {
///     if false {
///         return 5;
///     } else {
///         return 99;
///     }
/// }
/// ```
///
/// ### ❌ Invalid Examples
///
/// * The provided condition can't be any other than expression that returns a
///   boolean:
///
/// ```text
/// if 1 + 1 {
///     // Invalid
/// }
/// ```
///
/// * For a series of conditional statement branches to be able to be placed as
///   the last statement of a function, all of the branches must return values
///   (exhaustive). Else, the compiler will throw an error.
///
/// ```text
/// def foo(): int {
///     if !true {
///         return 1;
///     } else {
///         // Error: This branch should returns something!
///     }
/// }
/// ```
pub fn analyze(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    let cond_t = expression::analyze(state, node.variant.as_exec_cond())?;
    assert::is_boolean(&cond_t, || node.variant.as_exec_cond().span.clone())?;

    let returned_t_before_this = state.take_returned_type();

    let exit_scope_id = state.current_scope_id();
    state.create_scope(node.id, ScopeVariant::Conditional, exit_scope_id);
    state.enter_scope(node.id);

    body::analyze(state, node)?;

    state.enter_scope(exit_scope_id);

    let return_t = state.take_returned_type();

    if node.variant.as_or_else_branch().is_none() {
        // Non-exhaustive branch(es).
        //
        // We must set body's returned type back to the type before we
        // encountered this statement.
        //
        // For example:
        //
        // ```
        // def foo: int {
        //     return 1;  // The returned type is "int" here
        //
        //     if !true {
        //         return 5;
        //     }
        //
        //     // Because the above if-else statement is not exhaustive, we
        //     // can't take the returned type (the `5`).
        //     //
        //     // Instead, we must set it back to the previous returned type,
        //     // that is, the one where `1` was returned.
        // }
        // ```
        state.set_returned_type(returned_t_before_this);
        return Ok(());
    }

    let or_else_branch = node.variant.as_or_else_branch().unwrap();
    match &or_else_branch.variant {
        AstNodeVariant::If { .. } => {
            // All conditional branches must returning a value (exhaustive)
            // for this statement to be considered as returning value

            analyze(state, or_else_branch)?;

            let branch_return_t = state.take_returned_type();

            if return_t != Type::Void && branch_return_t != Type::Void {
                state.set_returned_type(return_t);
            } else {
                state.set_returned_type(returned_t_before_this);
            }
        }

        AstNodeVariant::Else { .. } => {
            // Check all statements inside the body with a new scope

            let exit_scope_id = state.current_scope_id();
            state.create_scope(or_else_branch.id, ScopeVariant::Conditional, exit_scope_id);
            state.enter_scope(or_else_branch.id);

            body::analyze(state, or_else_branch)?;

            state.enter_scope(exit_scope_id);

            let branch_return_t = state.take_returned_type();

            if return_t != Type::Void && branch_return_t != Type::Void {
                state.set_returned_type(return_t);
            } else {
                state.set_returned_type(returned_t_before_this);
            }
        }

        _ => unreachable!(),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn if_else_statements() {
        assert_is_ok(indoc! {"
                def main {
                    var condition1 = 5 < 10;
                    var condition2 = 0.5 < 0.75;

                    if condition1 {
                        debug condition1;
                        debug 1;
                    } else if condition2 {
                        debug condition2;
                        debug 2;
                    } else {
                        debug 0;
                    }
                }
            "})
    }

    #[test]
    fn nested_if_statements() {
        assert_is_ok(indoc! {"
                def main {
                    if 1 + 1 == 2 {
                        if 2 + 2 == 4 {
                            if 3 + 3 == 6 {
                                debug true;
                            }
                        }
                    }
                }
            "})
    }

    #[test]
    fn using_variable_declared_inside_conditional_scope_from_outside() {
        assert_is_err(indoc! {"
                def main {
                    if true {
                        var x = 50;
                        debug x;
                    }

                    debug x;
                }
            "})
    }

    #[test]
    fn using_math_expression_as_condition_in_if_statement() {
        assert_is_err(indoc! {"
                def main {
                    if 1 + 1 {
                        debug 1;
                    }
                }
            "})
    }

    #[test]
    fn using_variable_declared_in_sibling_branch_scope() {
        assert_is_err(indoc! {"
                def main {
                    if true {
                        var x = 50;
                    } else {
                        debug x;
                    }
                }
            "})
    }
}
