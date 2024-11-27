use super::{
    context::Context,
    error::{Error, Result},
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

/// Semantic checker for all expression rules.
pub struct ExpressionChecker<'a> {
    ctx: &'a Context,
}

impl<'a> ExpressionChecker<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    pub fn check(&self, node: &AstNode) -> Result<Type> {
        match node {
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
                self.ctx
                    .get_symbol_type(name)
                    .ok_or_else(|| Error::VariableNotExist {
                        id: String::from(name),
                        span: span.clone(),
                    })
            }

            AstNode::Literal { lit, .. } => Type::from_literal(lit),

            AstNode::FunctionCall { callee, args, span } => {
                self.check_function_call(callee, args, span)
            }

            _ => unreachable!(),
        }
    }

    fn check_logical_and_or_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check(lhs)?;
        let rhs_t = self.check(rhs)?;

        Type::assert_boolean(&lhs_t, || lhs.span().clone())?;
        Type::assert_boolean(&rhs_t, || rhs.span().clone())?;

        Ok(Type::new("Bool"))
    }

    fn check_equality_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check(lhs)?;
        let rhs_t = self.check(rhs)?;

        Type::assert_comparable(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::new("Bool"))
    }

    fn check_comparison_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check(lhs)?;
        let rhs_t = self.check(rhs)?;

        Type::assert_comparable(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        Ok(Type::new("Bool"))
    }

    fn check_math_binary_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check(lhs)?;
        let rhs_t = self.check(rhs)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        if lhs_t == Type::new("Int") && rhs_t == Type::new("Int") {
            Ok(Type::new("Int"))
        } else {
            Ok(Type::new("Float"))
        }
    }

    fn check_logical_not_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = self.check(child)?;
        Type::assert_boolean(&child_t, || child.span().clone())?;
        Ok(Type::new("Bool"))
    }

    fn check_neg_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = self.check(child)?;
        Type::assert_number(&child_t, || child.span().clone())?;
        Ok(child_t)
    }

    fn check_function_call(&self, callee: &AstNode, args: &[AstNode], span: &Span) -> Result<Type> {
        // transform the arguments into their respective type
        let mut args_t = vec![];
        for arg in args {
            args_t.push(self.check(arg)?)
        }

        let t = match callee {
            AstNode::Identifier { name, span } => {
                self.ctx
                    .get_symbol_type(name)
                    .ok_or_else(|| Error::VariableNotExist {
                        id: String::from(name),
                        span: span.clone(),
                    })?
            }

            _ => todo!(),
        };

        return match &t {
            Type::Callable { params_t, return_t } => {
                if params_t != &args_t {
                    return Err(Error::InvalidFunctionCallArgument {
                        args: args_t,
                        span: span.clone(),
                    });
                }
                Ok(*return_t.clone())
            }

            _ => {
                let span = callee.span().clone();
                Err(Error::NotAFunction { span })
            }
        };
    }
}
