use super::{error::Result, state::RuntimeState, statement::StatementRunner};
use kaba_compiler::{AstNode, AstNodeVariant};

pub struct BodyRunner<'src, 'a> {
    ast: &'a AstNode<'src>,
    root: &'a AstNode<'src>,

    state: &'a RuntimeState<'a>,
}

impl<'src, 'a> BodyRunner<'src, 'a> {
    pub fn new(
        ast: &'a AstNode<'src>,
        root: &'a AstNode<'src>,
        state: &'a RuntimeState<'a>,
    ) -> Self {
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

    fn body(&self) -> &'a [AstNode<'src>] {
        match &self.ast.variant {
            AstNodeVariant::Program { body, .. }
            | AstNodeVariant::If { body, .. }
            | AstNodeVariant::Else { body, .. }
            | AstNodeVariant::While { body, .. }
            | AstNodeVariant::Each { body, .. }
            | AstNodeVariant::FunctionDefinition { body, .. } => body,

            _ => unreachable!(),
        }
    }
}
