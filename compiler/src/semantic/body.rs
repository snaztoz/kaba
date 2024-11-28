use super::{error::Result, scope::ScopeStack, statement::StatementChecker, types::Type};
use crate::ast::AstNode;

/// Checker for a statement body.
///
/// Statement bodies are consist of `>= 0` statements, so this checker will call
/// the StatementChecker on each statement found in current body.
pub struct BodyChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> BodyChecker<'a> {
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl BodyChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let mut body_t = Type::new("Void");

        for stmt in self.body() {
            let t = StatementChecker::new(self.ss, stmt).check()?;
            if body_t.is_void() {
                body_t = t;
            }
        }

        Ok(body_t)
    }

    fn body(&self) -> &[AstNode] {
        match self.node {
            AstNode::FunctionDefinition { body, .. }
            | AstNode::If { body, .. }
            | AstNode::Else { body, .. }
            | AstNode::While { body, .. } => body,

            _ => unreachable!(),
        }
    }
}
