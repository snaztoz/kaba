use super::{
    body::BodyAnalyzer,
    error::Result,
    expression::ExpressionAnalyzer,
    state::{AnalyzerState, ScopeVariant},
    types::{assert, Type},
};
use crate::ast::{AstNode, ScopeId};

/// Analyzer for `while` loop statement.
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
pub struct WhileLoopAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> WhileLoopAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
        Self { node, state }
    }
}

impl WhileLoopAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        // Expecting boolean type for the condition

        let cond_t = ExpressionAnalyzer::new(self.cond(), self.state).analyze()?;
        assert::is_boolean(&cond_t, || self.cond().span().clone())?;

        // Check all statements inside the body with a new scope

        self.state
            .with_scope(self.scope_id(), ScopeVariant::Loop, || {
                BodyAnalyzer::new(self.node, self.state).analyze()
            })?;

        Ok(Type::Void)
    }

    fn cond(&self) -> &AstNode {
        if let AstNode::While { cond, .. } = self.node {
            cond
        } else {
            unreachable!()
        }
    }

    fn scope_id(&self) -> ScopeId {
        if let AstNode::While { scope_id, .. } = self.node {
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
