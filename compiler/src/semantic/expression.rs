use super::{
    error::{Error, Result},
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

            AstNode::Literal { lit, .. } => Type::from_literal(lit),

            AstNode::FunctionCall { .. } => FunctionCallChecker::new(self.ss, self.node).check(),

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

        Type::assert_comparable(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::new("Bool"))
    }

    fn check_comparison_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_comparable(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        Ok(Type::new("Bool"))
    }

    fn check_math_binary_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        if lhs_t == Type::new("Int") && rhs_t == Type::new("Int") {
            Ok(Type::new("Int"))
        } else {
            Ok(Type::new("Float"))
        }
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
        let fn_t = self.fn_t()?;
        Type::assert_callable(&fn_t, || self.callee().span().clone())?;

        let args_t = self.args_t()?;
        let (params_t, return_t) = fn_t.unwrap_callable();

        if params_t != args_t {
            return Err(Error::InvalidFunctionCallArgument {
                args: args_t,
                span: self.span().clone(),
            });
        }

        Ok(return_t)
    }

    fn fn_t(&self) -> Result<Type> {
        match self.callee() {
            AstNode::Identifier { name, span } => {
                self.ss
                    .get_symbol_type(name)
                    .ok_or_else(|| Error::SymbolDoesNotExist {
                        id: String::from(name),
                        span: span.clone(),
                    })
            }

            AstNode::FunctionCall { .. } => {
                FunctionCallChecker::new(self.ss, self.callee()).check()
            }

            _ => todo!("functions stored in array, method, etc"),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};

    fn check_and_assert_type(input: &str, expected_t: Type) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let result = if let AstNode::Program { body } = &ast {
            let scopes = ScopeStack::default();
            ExpressionChecker::new(&scopes, &body[0]).check()
        } else {
            unreachable!();
        };

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_t);
    }

    fn check_and_assert_is_err(input: &str) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let result = if let AstNode::Program { body } = &ast {
            let scopes = ScopeStack::default();
            ExpressionChecker::new(&scopes, &body[0]).check()
        } else {
            unreachable!();
        };

        assert!(result.is_err());
    }

    #[test]
    fn math_expression_returning_int_type() {
        check_and_assert_type("-5 + 50 * 200 / 7 - 999;", Type::new("Int"));
    }

    #[test]
    fn math_expression_with_int_and_float_operands() {
        check_and_assert_type("-5 + -0.25;", Type::new("Float"));
    }

    #[test]
    fn float_modulo_operation() {
        check_and_assert_type("99.9 % 0.1;", Type::new("Float"));
    }

    #[test]
    fn comparison_and_equality_operations() {
        check_and_assert_type("767 >= 900 == (45 < 67);", Type::new("Bool"));
    }

    #[test]
    fn logical_or_and_and_operations() {
        check_and_assert_type("false || !false && 50 > 0;", Type::new("Bool"));
    }

    #[test]
    fn non_existing_identifier() {
        check_and_assert_is_err("100 - not_exist;");
    }

    #[test]
    fn negating_boolean_value() {
        check_and_assert_is_err("-true;");
    }

    #[test]
    fn comparing_boolean_values() {
        check_and_assert_is_err("true > false;");
    }

    #[test]
    fn checking_equality_of_int_and_float() {
        check_and_assert_is_err("93 != 93.0;");
    }

    #[test]
    fn negating_int_value() {
        check_and_assert_is_err("!5;");
    }

    #[test]
    fn logical_and_with_int_value() {
        check_and_assert_is_err("false || !false && 50;");
    }
}
