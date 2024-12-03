use super::{
    assignment::AssignmentChecker,
    error::{Error, Result},
    literal::LiteralChecker,
    scope::ScopeStack,
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

/// Checker for expression rules.
pub struct ExpressionChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> ExpressionChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl ExpressionChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        match self.node {
            AstNode::Assign { .. }
            | AstNode::AddAssign { .. }
            | AstNode::SubAssign { .. }
            | AstNode::MulAssign { .. }
            | AstNode::DivAssign { .. }
            | AstNode::ModAssign { .. } => AssignmentChecker::new(self.ss, self.node).check(),

            AstNode::Eq { lhs, rhs, .. } | AstNode::Neq { lhs, rhs, .. } => {
                self.check_equality_operation(lhs, rhs)
            }

            AstNode::Or { lhs, rhs, .. } | AstNode::And { lhs, rhs, .. } => {
                self.check_logical_and_or_operation(lhs, rhs)
            }

            AstNode::Gt { lhs, rhs, .. }
            | AstNode::Gte { lhs, rhs, .. }
            | AstNode::Lt { lhs, rhs, .. }
            | AstNode::Lte { lhs, rhs, .. } => self.check_comparison_operation(lhs, rhs),

            AstNode::Add { lhs, rhs, .. }
            | AstNode::Sub { lhs, rhs, .. }
            | AstNode::Mul { lhs, rhs, .. }
            | AstNode::Div { lhs, rhs, .. }
            | AstNode::Mod { lhs, rhs, .. } => self.check_math_binary_operation(lhs, rhs),

            AstNode::Not { child, .. } => self.check_logical_not_operation(child),
            AstNode::Neg { child, .. } => self.check_neg_operation(child),

            AstNode::Identifier { name, span } => {
                self.ss
                    .get_symbol_type(name)
                    .ok_or_else(|| Error::SymbolDoesNotExist {
                        id: String::from(name),
                        span: span.clone(),
                    })
            }

            AstNode::Literal { lit, .. } => LiteralChecker::new(self.ss, lit).check(),

            AstNode::FunctionCall { .. } => FunctionCallChecker::new(self.ss, self.node).check(),
            AstNode::IndexAccess { .. } => IndexAccessChecker::new(self.ss, self.node).check(),

            _ => unreachable!(),
        }
    }

    fn check_logical_and_or_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_boolean(&lhs_t, || lhs.span().clone())?;
        Type::assert_boolean(&rhs_t, || rhs.span().clone())?;

        Ok(Type::new("Bool"))
    }

    fn check_equality_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_same(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::new("Bool"))
    }

    fn check_comparison_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_same(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        Ok(Type::new("Bool"))
    }

    fn check_math_binary_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;
        Type::assert_same(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(lhs_t)
    }

    fn check_logical_not_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionChecker::new(self.ss, child).check()?;
        Type::assert_boolean(&child_t, || child.span().clone())?;
        Ok(Type::new("Bool"))
    }

    fn check_neg_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = ExpressionChecker::new(self.ss, child).check()?;
        Type::assert_number(&child_t, || child.span().clone())?;
        Ok(child_t)
    }
}

/// Checker for function call expression rule.
struct FunctionCallChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionCallChecker<'a> {
    const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionCallChecker<'_> {
    fn check(&self) -> Result<Type> {
        let fn_t = ExpressionChecker::new(self.ss, self.callee()).check()?;
        Type::assert_callable(&fn_t, || self.callee().span().clone())?;

        let args_t = self.args_t()?;
        let (params_t, return_t) = fn_t.unwrap_callable();

        for (param_t, arg_t) in params_t.iter().zip(&args_t) {
            if !arg_t.is_assignable_to(param_t) {
                return Err(Error::InvalidFunctionCallArgument {
                    args: args_t,
                    span: self.span().clone(),
                });
            }
        }

        Ok(return_t)
    }

    // Transform arguments into their respective type
    fn args_t(&self) -> Result<Vec<Type>> {
        let mut args_t = vec![];
        for arg in self.args() {
            let t = ExpressionChecker::new(self.ss, arg).check()?;
            args_t.push(t);
        }

        Ok(args_t)
    }

    fn callee(&self) -> &AstNode {
        if let AstNode::FunctionCall { callee, .. } = self.node {
            callee
        } else {
            unreachable!()
        }
    }

    fn args(&self) -> &[AstNode] {
        if let AstNode::FunctionCall { args, .. } = self.node {
            args
        } else {
            unreachable!()
        }
    }

    fn span(&self) -> &Span {
        if let AstNode::FunctionCall { span, .. } = self.node {
            span
        } else {
            unreachable!()
        }
    }
}

/// Checker for index access expression rule.
struct IndexAccessChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> IndexAccessChecker<'a> {
    const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl IndexAccessChecker<'_> {
    fn check(&self) -> Result<Type> {
        let obj_t = ExpressionChecker::new(self.ss, self.obj()).check()?;
        Type::assert_indexable(&obj_t, || self.obj().span().clone())?;

        let index_t = ExpressionChecker::new(self.ss, self.index()).check()?;
        Type::assert_number(&index_t, || self.index().span().clone())?;

        match obj_t {
            Type::Array { elem_t, .. } => Ok(*elem_t.unwrap()),

            _ => unreachable!(),
        }
    }

    fn obj(&self) -> &AstNode {
        if let AstNode::IndexAccess { object, .. } = self.node {
            object
        } else {
            unreachable!()
        }
    }

    fn index(&self) -> &AstNode {
        if let AstNode::IndexAccess { index, .. } = self.node {
            index
        } else {
            unreachable!()
        }
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
        assert_expression_type("-5 + 50 * 200 / 7 - 999;", Type::new("Int"));
    }

    #[test]
    fn float_modulo_operation() {
        assert_expression_type("99.9 % 0.1;", Type::new("Float"));
    }

    #[test]
    fn comparison_and_equality_operations() {
        assert_expression_type("767 >= 900 == (45 < 67);", Type::new("Bool"));
    }

    #[test]
    fn logical_or_and_and_operations() {
        assert_expression_type("false || !false && 50 > 0;", Type::new("Bool"));
    }

    #[test]
    fn index_accessing() {
        assert_expression_type(
            "[[1, 2]][0];",
            Type::Array {
                size: Some(2),
                elem_t: Some(Box::new(Type::new("Int"))),
            },
        );
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
    fn checking_equality_of_int_and_float() {
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
