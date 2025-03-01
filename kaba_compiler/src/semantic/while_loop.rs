use super::{
    body,
    error::Result,
    expression,
    state::{AnalyzerState, ScopeVariant},
    typ::assert,
};
use crate::ast::AstNode;

/// Analyze `while` loop statement.
///
/// ### ✅ Valid Examples
///
/// * The provided condition must be an expression that returns a boolean:
///
/// ```text
/// while true {
///     # ...
/// }
/// ```
///
/// * With `break` or `continue` statement:
///
/// ```text
/// while true {
///     if !false {
///         break;
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
/// while 1 + 1 {
///     # Invalid
/// }
/// ```
pub fn analyze(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    // Expecting boolean type for the condition

    let cond_t = expression::analyze(state, node.variant.as_exec_cond())?;
    assert::is_boolean(&cond_t, || node.variant.as_exec_cond().span.clone())?;

    // Function return type can't be taken from a `while-loop` body, so we must
    // reset the type after the loop body is evaluated.
    let returned_t_before_this = state.take_returned_type();

    // Check all statements inside the body with a new scope
    let exit_scope_id = state.current_scope_id();
    state.create_scope(node.id, ScopeVariant::Loop, exit_scope_id);
    state.enter_scope(node.id);

    body::analyze(state, node)?;

    state.enter_scope(exit_scope_id);

    state.set_returned_type(returned_t_before_this);

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn while_loop_statements() {
        assert_is_ok(indoc! {"
                def main {
                    while 2 > 5 {
                        debug 1;
                    }

                    var a = 5;
                    while true {
                        if a == 5 {
                            break;
                        }
                        debug 0;
                    }
                }
            "})
    }

    #[test]
    fn using_math_expression_as_condition_in_while_statement() {
        assert_is_err(indoc! {"
                def main {
                    while 5 + 5 {}
                }
            "})
    }

    #[test]
    fn using_break_statement_not_in_loop_scope() {
        assert_is_err(indoc! {"
                def main {
                    if true {
                        break;
                    }
                }
            "})
    }

    #[test]
    fn using_invalid_statement_after_loop_control() {
        assert_is_err(indoc! {"
                def main {
                    while true {
                        break;
                        1 + true; // this should be error
                    }
                }
            "})
    }
}
