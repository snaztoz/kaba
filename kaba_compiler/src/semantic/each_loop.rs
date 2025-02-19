use super::{body, error::Result, expression, state::AnalyzerState, typ::assert};
use crate::{ast::AstNode, AstNodeVariant};

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
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let expr_t = expression::analyze(state, unwrap_iterable(node))?;
    assert::is_iterable(&expr_t, || unwrap_iterable(node).span.clone())?;

    let elem_sym = unwrap_elem_sym(node);
    let elem_sym_name = elem_sym.sym_name();
    let elem_t = expr_t.unwrap_array();

    // Function return type can't be taken from an `each-loop` body, so we must
    // reset the type after the loop body is evaluated.
    let returned_t_before_this = state.take_returned_type();

    // Check all statements inside the body with a new scope
    state.with_loop_scope(node.id, || {
        state.save_entity(elem_sym.id, elem_sym_name, elem_t);
        body::analyze(state, node)
    })?;

    state.set_returned_type(returned_t_before_this);

    Ok(())
}

fn unwrap_iterable<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    if let AstNodeVariant::Each { iterable, .. } = &node.variant {
        iterable
    } else {
        unreachable!()
    }
}

fn unwrap_elem_sym<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    if let AstNodeVariant::Each { elem_sym, .. } = &node.variant {
        elem_sym
    } else {
        unreachable!()
    }
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
