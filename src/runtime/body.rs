use super::{state::RuntimeState, statement::StatementRunner, Result};
use compiler::ast::AstNode;

pub struct BodyRunner<'a> {
    ast: &'a AstNode,
    root: &'a AstNode,

    state: &'a RuntimeState<'a>,
}

impl<'a> BodyRunner<'a> {
    pub fn new(ast: &'a AstNode, root: &'a AstNode, state: &'a RuntimeState<'a>) -> Self {
        Self { ast, root, state }
    }

    pub fn run(&self) -> Result<()> {
        let body = self.body();

        for stmt in body {
            if self.state.is_stop_executing() {
                break;
            }
            StatementRunner::new(stmt, self.root, self.state).run()?;
        }

        Ok(())
    }

    fn body(&self) -> &'a [AstNode] {
        match self.ast {
            AstNode::Program { body }
            | AstNode::If { body, .. }
            | AstNode::Else { body, .. }
            | AstNode::While { body, .. }
            | AstNode::FunctionDefinition { body, .. } => body,

            _ => unreachable!(),
        }
    }
}
