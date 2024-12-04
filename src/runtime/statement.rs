use super::{
    body::BodyRunner, expression::ExpressionRunner, state::RuntimeState, value::RuntimeValue,
    Result,
};
use compiler::ast::AstNode;
use std::collections::HashMap;

pub struct StatementRunner<'a> {
    ast: &'a AstNode,
    root: &'a AstNode,

    state: &'a RuntimeState<'a>,
}

impl<'a> StatementRunner<'a> {
    pub fn new(ast: &'a AstNode, root: &'a AstNode, state: &'a RuntimeState<'a>) -> Self {
        Self { ast, root, state }
    }

    fn run_debug_statement(&self, expr: &'a AstNode) -> Result<()> {
        let val = ExpressionRunner::new(expr, self.root, self.state).run()?;
        writeln!(self.state.streams.borrow_mut().out, "{val}").unwrap();
        Ok(())
    }
}

impl StatementRunner<'_> {
    pub fn run(&self) -> Result<()> {
        match self.ast {
            AstNode::VariableDeclaration { id, val, .. } => {
                let name = id.unwrap_identifier().0;
                let val = ExpressionRunner::new(val, self.root, self.state).run()?;
                self.state.store_value(&name, val);
            }

            AstNode::If { .. } => self.run_conditional_branch()?,

            AstNode::While { .. } => self.run_while()?,

            AstNode::Break { .. } => {
                self.state.stop_execution();
                self.state.exit_loop();
            }

            AstNode::Continue { .. } => {
                self.state.stop_execution();
            }

            AstNode::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let val = ExpressionRunner::new(expr, self.root, self.state).run()?;
                    self.state.set_return_value(val);
                };
                self.state.stop_execution();
            }

            AstNode::Debug { expr, .. } => self.run_debug_statement(expr)?,

            node => {
                ExpressionRunner::new(node, self.root, self.state).run()?;
            }
        };

        Ok(())
    }

    fn run_conditional_branch(&self) -> Result<()> {
        let cond_val = if let AstNode::If { cond, .. } = self.ast {
            ExpressionRunner::new(cond, self.root, self.state).run()?
        } else {
            unreachable!()
        };

        let should_exec = match cond_val {
            RuntimeValue::Boolean(b) => b,
            _ => unreachable!(),
        };

        if should_exec {
            self.state.ss.borrow_mut().push(HashMap::new());
            BodyRunner::new(self.ast, self.root, self.state).run()?;
            self.state.ss.borrow_mut().pop();

            return Ok(());
        }

        if let AstNode::If { or_else, .. } = self.ast {
            if or_else.is_none() {
                return Ok(());
            }

            let alt = or_else.as_ref().unwrap();
            match alt.as_ref() {
                AstNode::If { .. } => {
                    StatementRunner::new(alt, self.root, self.state).run()?;
                }

                AstNode::Else { .. } => {
                    self.state.ss.borrow_mut().push(HashMap::new());
                    BodyRunner::new(alt, self.root, self.state).run()?;
                    self.state.ss.borrow_mut().pop();
                }

                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn run_while(&self) -> Result<()> {
        loop {
            let cond_val = if let AstNode::While { cond, .. } = self.ast {
                ExpressionRunner::new(cond, self.root, self.state).run()?
            } else {
                unreachable!()
            };

            let should_exec = match cond_val {
                RuntimeValue::Boolean(b) => b,
                _ => unreachable!(),
            };

            if should_exec {
                self.state.ss.borrow_mut().push(HashMap::new());
                BodyRunner::new(self.ast, self.root, self.state).run()?;
                self.state.ss.borrow_mut().pop();

                if self.state.is_stop_executing() {
                    self.state.resume_execution();
                }

                if self.state.is_exiting_loop() {
                    self.state.reset_loop_state();
                    break;
                }
            } else {
                break;
            }
        }

        Ok(())
    }
}
