use super::{
    body::BodyChecker,
    error::Result,
    expression::ExpressionChecker,
    scope::{Scope, ScopeStack},
    types::Type,
};
use crate::ast::AstNode;

/// Checker for conditional branch statement.
///
/// ### ✅ Valid Examples
///
/// * The provided condition must be an expression that returns a boolean:
///
/// ```text
/// if true do
///     # ...
/// end
/// ```
///
/// * This statement can have multiple branches:
///
/// ```text
/// if false do
///     # Won't be executed
/// else if !true do
///     # Won't be executed
/// else do
///     # Will be executed
/// end
/// ```
///
/// * It can be the last statement of a function:
///
/// ```text
/// fn foo(): Int do
///     if false do
///         return 5;
///     else do
///         return 99;
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
/// if 1 + 1 do
///     # Invalid
/// end
/// ```
///
/// * If a conditional statement branch returns a value (when it is residing
///   inside a function), then any other branch in the same statement must also
///   returning values as well to make it be able to be the last statement in
///   the function:
///
/// ```text
/// fn foo(): Int do
///     if !true do
///         return 1;
///     else do
///         # Error: This branch should returns something!
///     end
/// end
/// ```
pub struct ConditionalBranchChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> ConditionalBranchChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl ConditionalBranchChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let cond_t = ExpressionChecker::new(self.ss, self.cond()).check()?;
        Type::assert_boolean(&cond_t, || self.cond().span().clone())?;

        // Check all statements inside the body with a new scope

        let return_t = self.ss.with_scope(Scope::new_conditional_scope(), || {
            BodyChecker::new(self.ss, self.node).check()
        })?;

        if self.or_else().is_none() {
            // Non-exhaustive branches, set to "Void"
            return Ok(Type::new("Void"));
        }

        match self.or_else().unwrap() {
            AstNode::If { .. } => {
                // All conditional branches must returning a value (exhaustive)
                // for this statement to be considered as returning value

                let branch_return_t =
                    ConditionalBranchChecker::new(self.ss, self.or_else().unwrap()).check()?;

                if !return_t.is_void() && !branch_return_t.is_void() {
                    Ok(return_t)
                } else {
                    Ok(Type::new("Void"))
                }
            }

            AstNode::Else { .. } => {
                // Check all statements inside the body with a new scope

                let branch_return_t = self.ss.with_scope(Scope::new_conditional_scope(), || {
                    BodyChecker::new(self.ss, self.or_else().unwrap()).check()
                })?;

                if !return_t.is_void() && !branch_return_t.is_void() {
                    Ok(return_t)
                } else {
                    Ok(Type::new("Void"))
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
                fn main() do
                    var condition1 = 5 < 10;
                    var condition2 = 0.5 < 0.75;

                    if condition1 do
                        debug condition1;
                        debug 1;
                    else if condition2 do
                        debug condition2;
                        debug 2;
                    else do
                        debug 0;
                    end
                end
            "})
    }

    #[test]
    fn nested_if_statements() {
        assert_is_ok(indoc! {"
                fn main() do
                    if 1 + 1 == 2 do
                        if 2 + 2 == 4 do
                            if 3 + 3 == 6 do
                                debug true;
                            end
                        end
                    end
                end
            "})
    }

    #[test]
    fn using_variable_declared_inside_conditional_scope_from_outside() {
        assert_is_err(indoc! {"
                fn main() do
                    if true do
                        var x = 50;
                        debug x;
                    end

                    debug x;
                end
            "})
    }

    #[test]
    fn using_math_expression_as_condition_in_if_statement() {
        assert_is_err(indoc! {"
                fn main() do
                    if 1 + 1 do
                        debug 1;
                    end
                end
            "})
    }

    #[test]
    fn using_variable_declared_in_sibling_branch_scope() {
        assert_is_err(indoc! {"
                fn main() do
                    if true do
                        var x = 50;
                    else do
                        debug x;
                    end
                end
            "})
    }
}
