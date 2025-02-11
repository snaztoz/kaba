use super::{
    body,
    error::Result,
    expression,
    state::AnalyzerState,
    types::{assert, Type},
};
use crate::ast::{AstNode, AstNodeVariant};

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
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    // Expecting boolean type for the condition

    let cond_t = expression::analyze(state, unwrap_cond(node))?;
    assert::is_boolean(&cond_t, || unwrap_cond(node).span.clone())?;

    // Check all statements inside the body with a new scope

    state.with_loop_scope(node.variant.scope_id(), || body::analyze(state, node))?;

    Ok(Type::Void)
}

fn unwrap_cond<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    if let AstNodeVariant::While { cond, .. } = &node.variant {
        cond
    } else {
        unreachable!()
    }
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
