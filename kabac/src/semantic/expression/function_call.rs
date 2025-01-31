use super::ExpressionAnalyzer;
use crate::{
    ast::AstNode,
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        state::AnalyzerState,
        types::{assert, Type},
    },
};
use logos::Span;

/// Analyzer for function call expression rule.
pub struct FunctionCallAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> FunctionCallAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
        Self { node, state }
    }
}

impl FunctionCallAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let fn_t = ExpressionAnalyzer::new(self.callee(), self.state).analyze()?;
        assert::is_callable(&fn_t, || self.callee().span().clone())?;

        let args_t = self.args_t()?;
        let (params_t, return_t) = fn_t.unwrap_callable();

        if params_t.len() != args_t.len() {
            return Err(SemanticError {
                variant: SemanticErrorVariant::ArgumentLengthMismatch {
                    expected: params_t.len(),
                    get: args_t.len(),
                },
                span: self.span().clone(),
            });
        }

        for (param_t, arg_t) in params_t.iter().zip(&args_t) {
            if !arg_t.is_assignable_to(param_t) {
                return Err(SemanticError {
                    variant: SemanticErrorVariant::InvalidArguments { args_t },
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
            let t = ExpressionAnalyzer::new(arg, self.state).analyze()?;
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
