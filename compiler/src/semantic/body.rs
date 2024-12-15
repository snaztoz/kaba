use super::{error::Result, state::SharedState, statement::StatementAnalyzer, types::Type};
use crate::ast::AstNode;

/// Analyzer for a statement body.
///
/// Statement bodies are consist of `>= 0` statements, so this analyzer will
/// call the [`StatementAnalyzer`] on each statement found in current body.
pub struct BodyAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> BodyAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl BodyAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let mut body_t = Type::void();

        for stmt in self.body() {
            let t = StatementAnalyzer::new(stmt, self.state).analyze()?;
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
