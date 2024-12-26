use super::{
    error::{Error, Result},
    expression::ExpressionAnalyzer,
    state::SharedState,
    types::{assert, Type},
};
use crate::ast::AstNode;
use logos::Span;

/// Analyzer for assignment statements.
///
/// It handles the analyzing of plain and shorthand assignment operations.
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
pub struct AssignmentAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> AssignmentAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl AssignmentAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        self.analyze_lhs()?;

        match self.node {
            AstNode::Assign { lhs, rhs, span } => self.analyze_assignment(lhs, rhs, span),

            AstNode::AddAssign { lhs, rhs, span }
            | AstNode::SubAssign { lhs, rhs, span }
            | AstNode::MulAssign { lhs, rhs, span }
            | AstNode::DivAssign { lhs, rhs, span }
            | AstNode::ModAssign { lhs, rhs, span } => {
                self.analyze_shorthand_assignment(lhs, rhs, span)
            }

            _ => unreachable!(),
        }
    }

    fn analyze_lhs(&self) -> Result<()> {
        if !self.lhs().is_valid_assignment_lhs() {
            return Err(Error::InvalidAssignmentLhs {
                lhs: self.lhs().to_string(),
                span: self.lhs().span().clone(),
            });
        }

        Ok(())
    }

    fn analyze_assignment(&self, lhs: &AstNode, rhs: &AstNode, span: &Span) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        assert::is_assignable(&rhs_t, &lhs_t, || span.clone())?;

        Ok(Type::Void)
    }

    fn analyze_shorthand_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        assert::is_number(&lhs_t, || lhs.span().clone())?;
        assert::is_number(&rhs_t, || rhs.span().clone())?;
        assert::is_assignable(&rhs_t, &lhs_t, || span.clone())?;

        Ok(Type::Void)
    }

    fn lhs(&self) -> &AstNode {
        match self.node {
            AstNode::Assign { lhs, .. }
            | AstNode::AddAssign { lhs, .. }
            | AstNode::SubAssign { lhs, .. }
            | AstNode::MulAssign { lhs, .. }
            | AstNode::DivAssign { lhs, .. }
            | AstNode::ModAssign { lhs, .. } => lhs,

            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn assigning_variables() {
        assert_is_ok(indoc! {"
                fn main() do
                    var x = 0;
                    x = 10;

                    var y: float = 0.0;
                    y = 5.0;

                    var z = false;
                    z = true;
                end
            "})
    }

    #[test]
    fn assigning_variable_using_shorthand_forms() {
        assert_is_ok(indoc! {"
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
        assert_is_ok(indoc! {"
                fn main() do
                    var i = 5.0;
                    i %= 2.5;
                end
            "})
    }

    #[test]
    fn assigning_value_with_non_existing_variable() {
        assert_is_err(indoc! {"
                fn main() do
                    var x: float = 5.0;
                    x = y;
                end
            "})
    }

    #[test]
    fn assigning_overflowed_value() {
        assert_is_err(indoc! {"
                fn main() do
                    var x: sbyte = 0;
                    x = 128;
                end
            "})
    }

    #[test]
    fn using_math_expression_as_lhs_in_assignment() {
        assert_is_err(indoc! {"
                fn main() do
                    1 + 1 = 5;
                end
            "})
    }

    #[test]
    fn using_boolean_expression_as_lhs_in_assignment() {
        assert_is_err(indoc! {"
                fn main() do
                    true || false = false;
                end
            "})
    }

    #[test]
    fn using_integer_grouped_expression_as_lhs_in_assignment() {
        assert_is_err(indoc! {"
                fn main() do
                    (50) = true;
                end
            "})
    }

    #[test]
    fn using_boolean_type_in_shorthand_assignment() {
        assert_is_err(indoc! {"
                fn main() do
                    true += true;
                end
            "})
    }

    //
    // Arrays
    //

    #[test]
    fn assign_to_array_element() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr = []bool{ true, false, true };

                    arr[1] = true;
                end
            "});
    }

    #[test]
    fn shorthand_assign_to_array_element() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr = []float{ 0.5, 1.1, 2.3 };

                    arr[0] += 5.5;
                end
            "});
    }
}
