use super::{
    body,
    error::Result,
    expression,
    state::{AnalyzerState, ScopeVariant},
    typ::assert,
};
use crate::ast::AstNode;

/// Analyze `each` loop statement.
///
/// ### ✅ Valid Examples
///
/// * The provided expression must evaluates to an array:
///
/// ```text
/// each n in [int 0, 1, 2] {
///     // ...
/// }
/// ```
///
/// * With `break` or `continue` statement:
///
/// ```text
/// each n in [int 0, 1, 2] {
///     if !false {
///         break;
///     }
/// }
/// ```
///
/// ### ❌ Invalid Examples
///
/// * The provided expression can't evaluates to types other than array:
///
/// ```text
/// each n in true {
///     // Invalid
/// }
/// ```
pub fn analyze(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    let expr_t = expression::analyze(state, node.variant.as_each_loop_iterable())?;
    assert::is_iterable(&expr_t, || {
        node.variant.as_each_loop_iterable().span.clone()
    })?;

    let elem_sym = node.variant.as_each_loop_elem_sym();
    let elem_sym_name = elem_sym.variant.as_sym_name();
    let elem_t = expr_t.into_array_elem_t();

    // Function return type can't be taken from an `each-loop` body, so we must
    // reset the type after the loop body is evaluated.
    let returned_t_before_this = state.take_returned_type();

    // Check all statements inside the body with a new scope
    let exit_scope_id = state.current_scope_id();
    state.create_scope(node.id, ScopeVariant::Loop, exit_scope_id);
    state.enter_scope(node.id);

    state.save_entity(elem_sym.id, elem_sym_name, elem_t);
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
    fn each_loop_statements() {
        assert_is_ok(indoc! {"
                def main {
                    each n in [int 1, 2, 3] {
                        debug n;
                    }

                    var arr = [int 4, 5, 6];
                    each n in arr {
                        debug n;
                    }
                }
            "});
    }

    #[test]
    fn each_loop_statement_with_non_array_expression() {
        assert_is_err(indoc! {"
                def main {
                    each n in true {
                        debug n;
                    }
                }
            "});
    }

    #[test]
    fn accessing_elem_id_outside_scope() {
        assert_is_err(indoc! {"
                def main {
                    each n in [int 1, 2] {}

                    debug n;
                }
            "});
    }
}
