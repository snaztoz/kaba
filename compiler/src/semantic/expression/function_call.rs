use super::ExpressionChecker;
use crate::{
    ast::AstNode,
    semantic::{
        error::{Error, Result},
        scope::ScopeStack,
        types::Type,
    },
};
use logos::Span;

/// Checker for function call expression rule.
pub struct FunctionCallChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionCallChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionCallChecker<'_> {
    pub fn check(&self) -> Result<Type> {
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