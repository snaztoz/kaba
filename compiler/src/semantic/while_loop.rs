use super::{
    body::BodyChecker,
    error::Result,
    expression::ExpressionChecker,
    scope::{Scope, ScopeStack},
    types::Type,
};
use crate::ast::AstNode;

/// Checker for `while` loop statement.
///
/// ### ✅ Valid Examples
///
/// * The provided condition must be an expression that returns a boolean:
///
/// ```text
/// while true do
///     # ...
/// end
/// ```
///
/// * With `break` or `continue` statement:
///
/// ```text
/// while true do
///     if !false do
///         break;
///     end
/// end
/// ```
///
/// ### ❌ Invalid Examples
///
/// * The provided condition can't be any other than expression that returns a
///   boolean:
///
/// ```text
/// while 1 + 1 do
///     # Invalid
/// end
/// ```
pub struct WhileLoopChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> WhileLoopChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl WhileLoopChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        // Expecting boolean type for the condition

        let cond_t = ExpressionChecker::new(self.ss, self.cond()).check()?;
        Type::assert_boolean(&cond_t, || self.cond().span().clone())?;

        // Check all statements inside the body with a new scope

        self.ss.with_scope(Scope::new_loop_scope(), || {
            BodyChecker::new(self.ss, self.node).check()
        })?;

        Ok(Type::new("Void"))
    }

    fn cond(&self) -> &AstNode {
        if let AstNode::While { cond, .. } = self.node {
            cond
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
                fn main() do
                    while 2 > 5 do
                        debug 1;
                    end

                    var a = 5;
                    while true do
                        if a == 5 do
                            break;
                        end
                        debug 0;
                    end
                end
            "})
    }

    #[test]
    fn using_math_expression_as_condition_in_while_statement() {
        assert_is_err(indoc! {"
                fn main() do
                    while 5 + 5 do end
                end
            "})
    }

    #[test]
    fn using_break_statement_not_in_loop_scope() {
        assert_is_err(indoc! {"
                fn main() do
                    if true do
                        break;
                    end
                end
            "})
    }

    #[test]
    fn using_invalid_statement_after_loop_control() {
        assert_is_err(indoc! {"
                fn main() do
                    while true do
                        break;
                        1 + true; # this should be error
                    end
                end
            "})
    }
}
