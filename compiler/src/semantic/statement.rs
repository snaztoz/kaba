use super::{
    conditional::ConditionalBranchChecker,
    each_loop::EachLoopChecker,
    error::{Error, Result},
    expression::ExpressionChecker,
    state::SharedState,
    types::Type,
    variable::VariableDeclarationChecker,
    while_loop::WhileLoopChecker,
};
use crate::ast::AstNode;
use logos::Span;

/// Checker for a single statement.
///
/// It checks simple statements, such as loop control checking, and also acts as
/// an aggregate for another (more specific) statement checkers, such as the
/// AssignmentChecker.
pub struct StatementChecker<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> StatementChecker<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl StatementChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        match self.node {
            AstNode::VariableDeclaration { .. } => {
                VariableDeclarationChecker::new(self.node, self.state).check()
            }

            AstNode::If { .. } => ConditionalBranchChecker::new(self.node, self.state).check(),

            AstNode::While { .. } => WhileLoopChecker::new(self.node, self.state).check(),
            AstNode::Each { .. } => EachLoopChecker::new(self.node, self.state).check(),

            AstNode::Break { span } | AstNode::Continue { span } => self.check_loop_control(span),

            AstNode::FunctionDefinition { id, .. } => Err(Error::UnexpectedStatement {
                stmt_str: self.node.to_string(),
                span: id.span().clone(),
            }),

            AstNode::Return { expr, span } => self.check_return(expr, span),

            AstNode::Debug { expr, span } => self.check_debug(expr, span),

            expr => ExpressionChecker::new(expr, self.state).check(),
        }
    }

    fn check_loop_control(&self, span: &Span) -> Result<Type> {
        if !self.state.ss.is_inside_loop() {
            return Err(Error::UnexpectedStatement {
                stmt_str: self.node.to_string(),
                span: span.clone(),
            });
        }

        Ok(Type::Void)
    }

    fn check_return(&self, expr: &Option<Box<AstNode>>, span: &Span) -> Result<Type> {
        let expr_t = expr
            .as_ref()
            .map(|expr| ExpressionChecker::new(expr, self.state).check())
            .unwrap_or(Ok(Type::Void))?;

        let return_t = self.state.ss.current_function_return_t().ok_or_else(|| {
            Error::UnexpectedStatement {
                stmt_str: self.node.to_string(),
                span: span.clone(),
            }
        })?;

        Type::assert_assignable(&expr_t, &return_t, || span.clone())
            .map_err(|err| Error::ReturnTypeMismatch {
                expected: return_t.clone(),
                get: expr_t,
                span: err.span().clone(),
            })
            .map(|_| return_t)
    }

    fn check_debug(&self, expr: &AstNode, span: &Span) -> Result<Type> {
        let expr_t = ExpressionChecker::new(expr, self.state).check()?;
        if expr_t.is_void() {
            return Err(Error::UnexpectedVoidTypeExpression { span: span.clone() });
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
                fn main() do
                    debug 17 * 5;
                end
            "});
    }

    #[test]
    fn debug_expression_with_void_type() {
        assert_is_err(indoc! {"
                fn main() do
                    debug this_is_void();
                end

                fn this_is_void() do
                end
            "});
    }
}
