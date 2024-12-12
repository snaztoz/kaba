use super::{error::Result, state::SharedState, statement::StatementChecker, types::Type};
use crate::ast::AstNode;

/// Checker for a statement body.
///
/// Statement bodies are consist of `>= 0` statements, so this checker will call
/// the StatementChecker on each statement found in current body.
pub struct BodyChecker<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> BodyChecker<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl BodyChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let mut body_t = Type::Void;

        for stmt in self.body() {
            let t = StatementChecker::new(stmt, self.state).check()?;
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
            | AstNode::While { body, .. }
            | AstNode::Each { body, .. } => body,

            _ => unreachable!(),
        }
    }
}
