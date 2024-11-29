use super::{
    error::{Error, Result},
    expression::ExpressionChecker,
    scope::ScopeStack,
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

/// Checker for assignment statements.
///
/// This checker handles the checking of plain assignment and also shorthand
/// assignment.
///
/// ### ✅ Valid Examples
///
/// * Plain assignment:
///
/// ```text
/// var x = 5;
/// x = 10;
/// ```
///
/// * Shorthand assignments:
///
/// ```text
/// var x = 1;
/// x += 2;
/// x -= 1;
/// x *= 10;
/// x /= 5;
/// x %= 2;
/// ```
///
/// ### ❌ Invalid Examples
///
/// * Variable can't be assigned with a value with incompatible type:
///
/// ```text
/// var x = 5;
/// x = 3.14;
/// ```
///
/// * Shorthand assignments (currently) only support value with number types
///   (integer and float):
///
/// ```text
/// var x = true;
/// x += false;
/// ```
pub struct AssignmentChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> AssignmentChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl AssignmentChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        match self.node {
            AstNode::Assign { lhs, rhs, span } => self.check_assignment(lhs, rhs, span),

            AstNode::AddAssign { lhs, rhs, span }
            | AstNode::SubAssign { lhs, rhs, span }
            | AstNode::MulAssign { lhs, rhs, span }
            | AstNode::DivAssign { lhs, rhs, span }
            | AstNode::ModAssign { lhs, rhs, span } => {
                self.check_shorthand_assignment(lhs, rhs, span)
            }

            _ => unreachable!(),
        }
    }

    fn check_assignment(&self, lhs: &AstNode, rhs: &AstNode, span: &Span) -> Result<Type> {
        if !lhs.is_valid_assignment_lhs() {
            return Err(Error::InvalidAssignmentLhs {
                lhs: lhs.to_string(),
                span: lhs.span().clone(),
            });
        }

        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_assignable(&rhs_t, &lhs_t, || span.clone())?;

        Ok(Type::new("Void"))
    }

    fn check_shorthand_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        self.check_assignment(lhs, rhs, span)
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{check_and_assert_is_err, check_and_assert_is_ok};
    use indoc::indoc;

    #[test]
    fn assigning_variables() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x = 0;
                    x = 10;

                    var y: Float = 0.0;
                    y = 5.0;

                    var z = false;
                    z = true;
                end
            "})
    }

    #[test]
    fn assigning_variable_using_shorthand_forms() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var i = 0;
                    i += 1;
                    i -= 2;
                    i *= 3;
                    i /= 4;
                    i %= 5;
                end
            "})
    }

    #[test]
    fn mod_assign_with_float_value() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var i = 5.0;
                    i %= 2.5;
                end
            "})
    }

    #[test]
    fn assigning_value_with_non_existing_variable() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: Float = 5.0;
                    x = y;
                end
            "})
    }

    #[test]
    fn using_math_expression_as_lhs_in_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    1 + 1 = 5;
                end
            "})
    }

    #[test]
    fn using_boolean_expression_as_lhs_in_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    true || false = false;
                end
            "})
    }

    #[test]
    fn using_integer_grouped_expression_as_lhs_in_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    (50) = true;
                end
            "})
    }

    #[test]
    fn using_boolean_type_in_shorthand_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    true += true;
                end
            "})
    }
}
