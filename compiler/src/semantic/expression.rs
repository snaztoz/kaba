use std::cmp;

use super::{
    assignment::AssignmentAnalyzer,
    error::{Error, Result},
    literal::LiteralAnalyzer,
    state::SharedState,
    types::{assert, Type},
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
                self.analyze_equality_expr(lhs, rhs)
            }

            AstNode::Or { lhs, rhs, .. } | AstNode::And { lhs, rhs, .. } => {
                self.analyze_logical_and_or_expr(lhs, rhs)
            }

            AstNode::Gt { lhs, rhs, .. }
            | AstNode::Gte { lhs, rhs, .. }
            | AstNode::Lt { lhs, rhs, .. }
            | AstNode::Lte { lhs, rhs, .. } => self.analyze_comparison_expr(lhs, rhs),

            AstNode::Add { lhs, rhs, .. }
            | AstNode::Sub { lhs, rhs, .. }
            | AstNode::Mul { lhs, rhs, .. }
            | AstNode::Div { lhs, rhs, .. }
            | AstNode::Mod { lhs, rhs, .. } => self.analyze_binary_math_expr(lhs, rhs),

            AstNode::Not { child, .. } => self.analyze_logical_not_expr(child),
            AstNode::Neg { child, .. } => self.analyze_neg_expr(child),

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

    fn analyze_logical_and_or_expr(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        assert::is_boolean(&lhs_t, || lhs.span().clone())?;
        assert::is_boolean(&rhs_t, || rhs.span().clone())?;

        Ok(Type::Bool)
    }

    fn analyze_equality_expr(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        assert::is_compatible(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::Bool)
    }

    fn analyze_comparison_expr(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        assert::is_number(&lhs_t, || lhs.span().clone())?;
        assert::is_number(&rhs_t, || rhs.span().clone())?;
        assert::is_compatible(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::Bool)
    }

    fn analyze_binary_math_expr(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        assert::is_number(&lhs_t, || lhs.span().clone())?;
        assert::is_number(&rhs_t, || rhs.span().clone())?;
        assert::is_compatible(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(cmp::max(lhs_t, rhs_t))
    }

    fn analyze_logical_not_expr(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionAnalyzer::new(child, self.state).analyze()?;

        assert::is_boolean(&child_t, || child.span().clone())?;

        Ok(Type::Bool)
    }

    fn analyze_neg_expr(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionAnalyzer::new(child, self.state).analyze()?;

        assert::is_signable(&child_t, || child.span().clone())?;

        Ok(child_t)
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::{
        test_util::{assert_expr_is_err, assert_expr_type},
        types::{IntType, Type},
    };

    #[test]
    fn math_expression_with_int_literals() {
        assert_expr_type(
            "-5 + 50 * 200 / 7 - 999;",
            &[],
            Type::Int(IntType::Unbounded),
        );
    }

    #[test]
    fn math_expression_with_sbyte() {
        let symbols = [("x", Type::Int(IntType::SByte))];
        assert_expr_type("5 + x;", &symbols, Type::Int(IntType::SByte));
    }

    #[test]
    fn math_expression_with_short() {
        let symbols = [("x", Type::Int(IntType::Short))];
        assert_expr_type("x + 5;", &symbols, Type::Int(IntType::Short));
    }

    #[test]
    fn math_expression_with_int() {
        let symbols = [("x", Type::Int(IntType::Int))];
        assert_expr_type("5 + x;", &symbols, Type::Int(IntType::Int));
    }

    #[test]
    fn math_expression_with_long() {
        let symbols = [("x", Type::Int(IntType::Long))];
        assert_expr_type("10 + x;", &symbols, Type::Int(IntType::Long));
    }

    #[test]
    fn math_expression_with_different_types() {
        let symbols = [
            ("a", Type::Int(IntType::SByte)),
            ("b", Type::Int(IntType::Short)),
        ];
        assert_expr_is_err("5 + a * b;", &symbols);
    }

    #[test]
    fn float_modulo_expression() {
        assert_expr_type("99.9 % 0.1;", &[], Type::Float);
    }

    #[test]
    fn comparison_and_equality_expressions() {
        assert_expr_type("767 >= 900 == (45 < 67);", &[], Type::Bool);
    }

    #[test]
    fn logical_or_and_and_expressions() {
        assert_expr_type("false || !false && 50 > 0;", &[], Type::Bool);
    }

    #[test]
    fn math_expression_with_int_and_float_operands() {
        assert_expr_is_err("-5 + -0.25;", &[]);
    }

    #[test]
    fn non_existing_identifier() {
        assert_expr_is_err("100 - not_exist;", &[]);
    }

    #[test]
    fn negating_boolean_value() {
        assert_expr_is_err("-true;", &[]);
    }

    #[test]
    fn comparing_boolean_values() {
        assert_expr_is_err("true > false;", &[]);
    }

    #[test]
    fn analyzing_equality_of_int_and_float() {
        assert_expr_is_err("93 == 93.0;", &[]);
    }

    #[test]
    fn negating_int_value() {
        assert_expr_is_err("!5;", &[]);
    }

    #[test]
    fn logical_and_with_int_value() {
        assert_expr_is_err("false || !false && 50;", &[]);
    }
}
