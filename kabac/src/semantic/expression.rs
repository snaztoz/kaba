use std::cmp;

use super::{
    assignment::AssignmentAnalyzer,
    error::{Result, SemanticError, SemanticErrorVariant},
    literal::LiteralAnalyzer,
    state::AnalyzerState,
    types::{assert, FloatType, IntType, Type},
};
use crate::ast::AstNode;
use function_call::FunctionCallAnalyzer;
use index_access::IndexAccessAnalyzer;

mod function_call;
mod index_access;

/// Analyzer for expression rules.
pub struct ExpressionAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> ExpressionAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
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

            AstNode::Not { expr, .. } => self.analyze_logical_not_expr(expr),
            AstNode::Neg { expr, .. } => self.analyze_neg_expr(expr),

            AstNode::Symbol { name, span } => self
                .state
                .get_sym_t(name)
                .ok_or_else(|| SemanticError {
                    variant: SemanticErrorVariant::SymbolDoesNotExist(String::from(name)),
                    span: span.clone(),
                })
                .map(|st| st.unwrap_entity()),

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

        if lhs_t.is_bounded_int()
            || rhs_t.is_bounded_int()
            || lhs_t.is_unbounded_float()
            || rhs_t.is_unbounded_float()
        {
            assert::is_compatible(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;
        }

        Ok(Type::Bool)
    }

    fn analyze_binary_math_expr(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionAnalyzer::new(lhs, self.state).analyze()?;
        let rhs_t = ExpressionAnalyzer::new(rhs, self.state).analyze()?;

        assert::is_number(&lhs_t, || lhs.span().clone())?;
        assert::is_number(&rhs_t, || rhs.span().clone())?;

        if lhs_t.is_unbounded_int() && rhs_t.is_unbounded_int() {
            return Ok(self.compute_unbounded_int_t(&lhs_t, &rhs_t));
        } else if lhs_t.is_unbounded_float() && rhs_t.is_unbounded_float() {
            return Ok(self.compute_unbounded_float_t(&lhs_t, &rhs_t));
        }

        assert::is_compatible(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        // Handle the possibility of converting unbounded into bounded type.
        match (lhs_t, rhs_t) {
            (Type::Int(i), Type::Int(j)) => {
                let max = cmp::max(i.clone(), j.clone());
                Ok(Type::Int(max))
            }

            (Type::Float(i), Type::Float(j)) => {
                let max = cmp::max(i.clone(), j.clone());
                Ok(Type::Float(max))
            }

            _ => unreachable!(),
        }
    }

    fn analyze_logical_not_expr(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionAnalyzer::new(child, self.state).analyze()?;

        assert::is_boolean(&child_t, || child.span().clone())?;

        Ok(Type::Bool)
    }

    fn analyze_neg_expr(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionAnalyzer::new(child, self.state).analyze()?;

        assert::is_signable(&child_t, || child.span().clone())?;

        let t = match child_t {
            Type::Int(IntType::Unbounded(n)) => Type::Int(IntType::Unbounded(-n)),
            _ => child_t,
        };

        Ok(t)
    }

    fn compute_unbounded_int_t(&self, lhs_t: &Type, rhs_t: &Type) -> Type {
        let lhs_val = lhs_t.unwrap_unbounded_int();
        let rhs_val = rhs_t.unwrap_unbounded_int();

        let n = match self.node {
            AstNode::Add { .. } => lhs_val.wrapping_add(rhs_val),
            AstNode::Sub { .. } => lhs_val.wrapping_sub(rhs_val),
            AstNode::Mul { .. } => lhs_val.wrapping_mul(rhs_val),
            AstNode::Div { .. } => lhs_val.wrapping_div(rhs_val),
            AstNode::Mod { .. } => lhs_val.wrapping_rem(rhs_val),
            _ => unreachable!(),
        };

        Type::Int(IntType::Unbounded(n))
    }

    fn compute_unbounded_float_t(&self, lhs_t: &Type, rhs_t: &Type) -> Type {
        let lhs_val = lhs_t.unwrap_unbounded_float();
        let rhs_val = rhs_t.unwrap_unbounded_float();

        let n = match self.node {
            AstNode::Add { .. } => lhs_val + rhs_val,
            AstNode::Sub { .. } => lhs_val - rhs_val,
            AstNode::Mul { .. } => lhs_val * rhs_val,
            AstNode::Div { .. } => lhs_val / rhs_val,
            AstNode::Mod { .. } => lhs_val % rhs_val,
            _ => unreachable!(),
        };

        Type::Float(FloatType::Unbounded(n))
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::{
        test_util::{assert_expr_is_err, assert_expr_type, eval_expr},
        types::{FloatType, IntType, Type},
    };

    #[test]
    fn math_expression_with_int_literals() {
        let res = eval_expr("-2 + 50 * 200 / 10 - 999;", &[]);

        assert!(res.is_ok());
        assert!(matches!(res.unwrap(), Type::Int(IntType::Unbounded(-1))));
    }

    #[test]
    fn math_with_overflowing_literals_expression() {
        let res = eval_expr("2147483647 + 1;", &[]);

        assert!(res.is_ok());
        assert!(matches!(
            res.unwrap(),
            Type::Int(IntType::Unbounded(-2147483648))
        ));
    }

    #[test]
    fn math_expression_with_float_literals() {
        let res = eval_expr("0.1 + 0.3 * 0.2 / 0.4;", &[]);

        assert!(res.is_ok());
        assert!(matches!(
            res.unwrap(),
            Type::Float(FloatType::Unbounded(0.25))
        ));
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
    fn math_expression_with_overflowed_operand() {
        let symbols = [("x", Type::Int(IntType::SByte))];
        assert_expr_is_err("128 + x;", &symbols);
    }

    #[test]
    fn math_expression_with_different_int_types() {
        let symbols = [
            ("a", Type::Int(IntType::SByte)),
            ("b", Type::Int(IntType::Short)),
        ];
        assert_expr_is_err("5 + a * b;", &symbols);
    }

    #[test]
    fn math_expression_with_different_float_types() {
        let symbols = [
            ("a", Type::Float(FloatType::Float)),
            ("b", Type::Float(FloatType::Double)),
        ];
        assert_expr_is_err("5 + a * b;", &symbols);
    }

    #[test]
    fn float_modulo_expression() {
        let result = eval_expr("3.5 % 0.1;", &[]);

        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap(),
            Type::Float(FloatType::Unbounded(_))
        ))
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
