use super::{
    assignment::AssignmentAnalyzer,
    error::{Error, Result},
    literal::LiteralAnalyzer,
    state::SharedState,
    types::Type,
};
use crate::ast::AstNode;
use function_call::FunctionCallAnalyzer;
use index_access::IndexAccessAnalyzer;

mod function_call;
mod index_access;

/// Analyzer for expression rules.
pub struct ExpressionAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> ExpressionAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl ExpressionAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        match self.node {
            AstNode::Assign { .. }
            | AstNode::AddAssign { .. }
            | AstNode::SubAssign { .. }
            | AstNode::MulAssign { .. }
            | AstNode::DivAssign { .. }
            | AstNode::ModAssign { .. } => AssignmentAnalyzer::new(self.node, self.state).analyze(),

            AstNode::Eq { lhs, rhs, .. } | AstNode::Neq { lhs, rhs, .. } => {
                self.analyze_equality_operation(lhs, rhs)
            }

            AstNode::Or { lhs, rhs, .. } | AstNode::And { lhs, rhs, .. } => {
                self.analyze_logical_and_or_operation(lhs, rhs)
            }

            AstNode::Gt { lhs, rhs, .. }
            | AstNode::Gte { lhs, rhs, .. }
            | AstNode::Lt { lhs, rhs, .. }
            | AstNode::Lte { lhs, rhs, .. } => self.analyze_comparison_operation(lhs, rhs),

            AstNode::Add { lhs, rhs, .. }
            | AstNode::Sub { lhs, rhs, .. }
            | AstNode::Mul { lhs, rhs, .. }
            | AstNode::Div { lhs, rhs, .. }
            | AstNode::Mod { lhs, rhs, .. } => self.analyze_math_binary_operation(lhs, rhs),

            AstNode::Not { child, .. } => self.analyze_logical_not_operation(child),
            AstNode::Neg { child, .. } => self.analyze_neg_operation(child),

            AstNode::Identifier { name, span } => {
                self.state
                    .get_sym_t(name)
                    .ok_or_else(|| Error::SymbolDoesNotExist {
                        id: String::from(name),
                        span: span.clone(),
                    })
            }

            AstNode::Literal { lit, .. } => LiteralAnalyzer::new(lit, self.state).analyze(),

            AstNode::FunctionCall { .. } => {
                FunctionCallAnalyzer::new(self.node, self.state).analyze()
            }
            AstNode::IndexAccess { .. } => {
                IndexAccessAnalyzer::new(self.node, self.state).analyze()
            }

            _ => unreachable!(),
        }
    }

    fn analyze_logical_and_or_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        Type::assert_boolean(&lhs_t, || lhs.span().clone())?;
        Type::assert_boolean(&rhs_t, || rhs.span().clone())?;

        Ok(Type::bool())
    }

    fn analyze_equality_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        Type::assert_same(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::bool())
    }

    fn analyze_comparison_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;
        Type::assert_same(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::bool())
    }

    fn analyze_math_binary_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;
        Type::assert_same(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        // TODO: do type promotion if applicable

        Ok(lhs_t)
    }

    fn analyze_logical_not_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionAnalyzer::new(child, self.state).analyze()?;

        Type::assert_boolean(&child_t, || child.span().clone())?;

        Ok(Type::bool())
    }

    fn analyze_neg_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionAnalyzer::new(child, self.state).analyze()?;

        Type::assert_signable_number(&child_t, || child.span().clone())?;

        Ok(child_t)
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::{
        test_util::{assert_expression_is_err, assert_expression_type},
        types::Type,
    };

    #[test]
    fn math_expression_returning_int_type() {
        assert_expression_type("-5 + 50 * 200 / 7 - 999;", Type::LiteralInt);
    }

    #[test]
    fn float_modulo_operation() {
        assert_expression_type("99.9 % 0.1;", Type::float());
    }

    #[test]
    fn comparison_and_equality_operations() {
        assert_expression_type("767 >= 900 == (45 < 67);", Type::bool());
    }

    #[test]
    fn logical_or_and_and_operations() {
        assert_expression_type("false || !false && 50 > 0;", Type::bool());
    }

    #[test]
    fn math_expression_with_int_and_float_operands() {
        assert_expression_is_err("-5 + -0.25;");
    }

    #[test]
    fn non_existing_identifier() {
        assert_expression_is_err("100 - not_exist;");
    }

    #[test]
    fn negating_boolean_value() {
        assert_expression_is_err("-true;");
    }

    #[test]
    fn comparing_boolean_values() {
        assert_expression_is_err("true > false;");
    }

    #[test]
    fn analyzeing_equality_of_int_and_float() {
        assert_expression_is_err("93 == 93.0;");
    }

    #[test]
    fn negating_int_value() {
        assert_expression_is_err("!5;");
    }

    #[test]
    fn logical_and_with_int_value() {
        assert_expression_is_err("false || !false && 50;");
    }
}
