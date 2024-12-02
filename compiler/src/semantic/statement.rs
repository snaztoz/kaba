use super::{
    assignment::AssignmentChecker,
    conditional::ConditionalBranchChecker,
    error::{Error, Result},
    expression::ExpressionChecker,
    scope::ScopeStack,
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
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> StatementChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl StatementChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        match self.node {
            AstNode::VariableDeclaration { .. } => {
                VariableDeclarationChecker::new(self.ss, self.node).check()
            }

            AstNode::If { .. } => ConditionalBranchChecker::new(self.ss, self.node).check(),

            AstNode::While { .. } => WhileLoopChecker::new(self.ss, self.node).check(),

            AstNode::Break { span } | AstNode::Continue { span } => self.check_loop_control(span),

            AstNode::FunctionDefinition { id, .. } => {
                return Err(Error::UnexpectedStatement {
                    stmt_str: self.node.to_string(),
                    span: id.span().clone(),
                })
            }

            AstNode::Return { expr, span } => self.check_return(expr, span),

            AstNode::Debug { expr, span } => self.check_debug(expr, span),

            AstNode::Assign { .. }
            | AstNode::AddAssign { .. }
            | AstNode::SubAssign { .. }
            | AstNode::MulAssign { .. }
            | AstNode::DivAssign { .. }
            | AstNode::ModAssign { .. } => AssignmentChecker::new(self.ss, self.node).check(),

            expr => ExpressionChecker::new(self.ss, expr).check(),
        }
    }

    fn check_loop_control(&self, span: &Span) -> Result<Type> {
        if !self.ss.is_inside_loop() {
            return Err(Error::UnexpectedStatement {
                stmt_str: self.node.to_string(),
                span: span.clone(),
            });
        }

        Ok(Type::new("Void"))
    }

    fn check_return(&self, expr: &Option<Box<AstNode>>, span: &Span) -> Result<Type> {
        let expr_t = expr
            .as_ref()
            .map(|expr| ExpressionChecker::new(self.ss, expr).check())
            .unwrap_or(Ok(Type::new("Void")))?;

        let return_t =
            self.ss
                .current_function_return_type()
                .ok_or_else(|| Error::UnexpectedStatement {
                    stmt_str: self.node.to_string(),
                    span: span.clone(),
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
        let expr_t = ExpressionChecker::new(self.ss, expr).check()?;
        if expr_t.is_void() {
            return Err(Error::DebugVoid { span: span.clone() });
        }

        Ok(Type::new("Void"))
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{check_and_assert_is_err, check_and_assert_is_ok};
    use indoc::indoc;

    #[test]
    fn debug_expression() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    debug 17 * 5;
                end
            "});
    }

    #[test]
    fn debug_expression_with_void_type() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    debug this_is_void();
                end

                fn this_is_void() do
                end
            "});
    }
}
