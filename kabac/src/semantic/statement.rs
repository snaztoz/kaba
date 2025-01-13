use super::{
    conditional::ConditionalBranchAnalyzer,
    each_loop::EachLoopAnalyzer,
    error::{Error, Result},
    expression::ExpressionAnalyzer,
    state::SharedState,
    types::{assert, Type},
    variable::VariableDeclarationAnalyzer,
    while_loop::WhileLoopAnalyzer,
};
use crate::ast::AstNode;
use logos::Span;

/// Analyzer for a single statement.
///
/// It analyzes simple statements, such as loop control analyzeing, and also
/// acts as an aggregate for another (more specific) statement analyzers, such
/// as the AssignmentAnalyzer.
pub struct StatementAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> StatementAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl StatementAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        match self.node {
            AstNode::VariableDeclaration { .. } => {
                VariableDeclarationAnalyzer::new(self.node, self.state).analyze()
            }

            AstNode::If { .. } => ConditionalBranchAnalyzer::new(self.node, self.state).analyze(),

            AstNode::While { .. } => WhileLoopAnalyzer::new(self.node, self.state).analyze(),
            AstNode::Each { .. } => EachLoopAnalyzer::new(self.node, self.state).analyze(),

            AstNode::Break { span } | AstNode::Continue { span } => self.analyze_loop_control(span),

            AstNode::FunctionDefinition { sym, .. } => Err(Error::UnexpectedStatement {
                stmt_str: self.node.to_string(),
                span: sym.span().clone(),
            }),

            AstNode::Return { expr, span } => self.analyze_return(expr, span),

            AstNode::Debug { expr, .. } => self.analyze_debug(expr),

            expr => ExpressionAnalyzer::new(expr, self.state).analyze(),
        }
    }

    fn analyze_loop_control(&self, span: &Span) -> Result<Type> {
        if !self.state.is_inside_loop() {
            return Err(Error::UnexpectedStatement {
                stmt_str: self.node.to_string(),
                span: span.clone(),
            });
        }

        Ok(Type::Void)
    }

    fn analyze_return(&self, expr: &Option<Box<AstNode>>, span: &Span) -> Result<Type> {
        let expr_t = expr
            .as_ref()
            .map(|expr| ExpressionAnalyzer::new(expr, self.state).analyze())
            .unwrap_or(Ok(Type::Void))?;

        let return_t = self
            .state
            .nearest_return_t()
            .ok_or_else(|| Error::UnexpectedStatement {
                stmt_str: self.node.to_string(),
                span: span.clone(),
            })?;

        assert::is_assignable(&expr_t, &return_t, || match expr {
            Some(expr) => expr.span().clone(),
            None => span.clone(),
        })
        .map_err(|err| Error::ReturnTypeMismatch {
            expected: return_t.clone(),
            get: expr_t,
            span: err.span().clone(),
        })?;

        Ok(return_t)
    }

    fn analyze_debug(&self, expr: &AstNode) -> Result<Type> {
        let expr_t = ExpressionAnalyzer::new(expr, self.state).analyze()?;
        if expr_t == Type::Void {
            return Err(Error::UnexpectedVoidTypeExpression {
                span: expr.span().clone(),
            });
        }

        Ok(Type::Void)
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn debug_expression() {
        assert_is_ok(indoc! {"
                fn main() {
                    debug 17 * 5;
                }
            "});
    }

    #[test]
    fn debug_expression_with_void_type() {
        assert_is_err(indoc! {"
                fn main() {
                    debug this_is_void();
                }

                fn this_is_void() {
                }
            "});
    }
}
