use super::{
    body::BodyAnalyzer,
    error::Result,
    expression::ExpressionAnalyzer,
    state::{AnalyzerState, ScopeVariant},
    types::{assert, Type},
};
use crate::ast::{AstNode, ScopeId};

/// Analyzer for conditional branch statement.
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
/// fn foo(): int {
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
/// fn foo(): int {
///     if !true {
///         return 1;
///     } else {
///         // Error: This branch should returns something!
///     }
/// }
/// ```
pub struct ConditionalBranchAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> ConditionalBranchAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
        Self { node, state }
    }
}

impl ConditionalBranchAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let cond_t = ExpressionAnalyzer::new(self.cond(), self.state).analyze()?;
        assert::is_boolean(&cond_t, || self.cond().span().clone())?;

        // Check all statements inside the body with a new scope

        let return_t = self
            .state
            .with_scope(self.scope_id(), ScopeVariant::Conditional, || {
                BodyAnalyzer::new(self.node, self.state).analyze()
            })?;

        if self.or_else().is_none() {
            // Non-exhaustive branches, set to "Void"
            return Ok(Type::Void);
        }

        match self.or_else().unwrap() {
            AstNode::If { .. } => {
                // All conditional branches must returning a value (exhaustive)
                // for this statement to be considered as returning value

                let branch_return_t =
                    ConditionalBranchAnalyzer::new(self.or_else().unwrap(), self.state)
                        .analyze()?;

                if return_t != Type::Void && branch_return_t != Type::Void {
                    Ok(return_t)
                } else {
                    Ok(Type::Void)
                }
            }

            AstNode::Else { scope_id, .. } => {
                // Check all statements inside the body with a new scope

                let branch_return_t =
                    self.state
                        .with_scope(*scope_id, ScopeVariant::Conditional, || {
                            BodyAnalyzer::new(self.or_else().unwrap(), self.state).analyze()
                        })?;

                if return_t != Type::Void && branch_return_t != Type::Void {
                    Ok(return_t)
                } else {
                    Ok(Type::Void)
                }
            }

            _ => unreachable!(),
        }
    }

    fn cond(&self) -> &AstNode {
        if let AstNode::If { cond, .. } = self.node {
            cond
        } else {
            unreachable!()
        }
    }

    fn scope_id(&self) -> ScopeId {
        if let AstNode::If { scope_id, .. } = self.node {
            *scope_id
        } else {
            unreachable!()
        }
    }

    fn or_else(&self) -> Option<&AstNode> {
        if let AstNode::If { or_else, .. } = self.node {
            or_else.as_deref()
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
