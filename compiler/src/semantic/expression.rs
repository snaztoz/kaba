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
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
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
                    .ok_or_else(|| Error::VariableNotExist {
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
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
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
                    .ok_or_else(|| Error::VariableNotExist {
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
