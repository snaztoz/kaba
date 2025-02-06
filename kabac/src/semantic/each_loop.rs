use super::{
    body,
    error::Result,
    expression,
    state::{AnalyzerState, ScopeVariant},
    types::{assert, Type},
};
use crate::ast::{AstNode, SymbolId};

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
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let expr_t = expression::analyze(state, unwrap_iterable(node))?;
    assert::is_iterable(&expr_t, || unwrap_iterable(node).span().clone())?;

    let elem_sym = unwrap_elem_sym(node).unwrap_symbol().0;
    let elem_t = expr_t.unwrap_array();

    // Check all statements inside the body with a new scope

    state.with_scope(node.scope_id(), ScopeVariant::Loop, || {
        state
            .save_entity_or_else(
                unwrap_elem_sym_id(node),
                &elem_sym,
                elem_t.clone(),
                || unreachable!(),
            )
            .unwrap();

        body::analyze(state, node)
    })?;

    Ok(Type::Void)
}

fn unwrap_iterable(node: &AstNode) -> &AstNode {
    if let AstNode::Each { iterable, .. } = node {
        iterable
    } else {
        unreachable!()
    }
}

fn unwrap_elem_sym(node: &AstNode) -> &AstNode {
    if let AstNode::Each { elem_sym, .. } = node {
        elem_sym
    } else {
        unreachable!()
    }
}

fn unwrap_elem_sym_id(node: &AstNode) -> SymbolId {
    if let AstNode::Each { elem_sym_id, .. } = node {
        *elem_sym_id
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
