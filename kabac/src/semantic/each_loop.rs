use super::{
    body::BodyAnalyzer,
    error::Result,
    expression::ExpressionAnalyzer,
    state::{AnalyzerState, ScopeVariant},
    types::{assert, Type},
};
use crate::ast::{AstNode, SymbolId};

/// Analyzer for `each` loop statement.
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
pub struct EachLoopAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> EachLoopAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
        Self { node, state }
    }
}

impl EachLoopAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let expr_t = ExpressionAnalyzer::new(self.iterable(), self.state).analyze()?;

        assert::is_iterable(&expr_t, || self.iterable().span().clone())?;

        let elem_sym = self.elem_sym().unwrap_symbol().0;
        let elem_t = expr_t.unwrap_array();

        // Check all statements inside the body with a new scope

        self.state
            .with_scope(self.scope_id(), ScopeVariant::Loop, || {
                self.state
                    .save_entity_or_else(
                        self.elem_sym_id(),
                        &elem_sym,
                        elem_t.clone(),
                        || unreachable!(),
                    )
                    .unwrap();

                BodyAnalyzer::new(self.node, self.state).analyze()
            })?;

        Ok(Type::Void)
    }

    fn iterable(&self) -> &AstNode {
        if let AstNode::Each { iterable, .. } = self.node {
            iterable
        } else {
            unreachable!()
        }
    }

    fn elem_sym(&self) -> &AstNode {
        if let AstNode::Each { elem_sym, .. } = self.node {
            elem_sym
        } else {
            unreachable!()
        }
    }

    fn elem_sym_id(&self) -> SymbolId {
        if let AstNode::Each { elem_sym_id, .. } = self.node {
            *elem_sym_id
        } else {
            unreachable!()
        }
    }

    fn scope_id(&self) -> SymbolId {
        if let AstNode::Each { scope_id, .. } = self.node {
            *scope_id
        } else {
            unreachable!()
        }
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
