use super::{
    body::BodyRunner, error::Result, expression::ExpressionRunner, state::RuntimeState,
    value::RuntimeValue,
};
use kaba_compiler::{AstNode, AstNodeVariant};
use std::collections::HashMap;

pub struct StatementRunner<'src, 'a> {
    ast: &'a AstNode<'src>,
    root: &'a AstNode<'src>,

    state: &'a RuntimeState<'a>,
}

impl<'src, 'a> StatementRunner<'src, 'a> {
    pub fn new(
        ast: &'a AstNode<'src>,
        root: &'a AstNode<'src>,
        state: &'a RuntimeState<'a>,
    ) -> Self {
        Self { ast, root, state }
    }

    fn run_debug_statement(&self, expr: &'a AstNode) -> Result<()> {
        let val = ExpressionRunner::new(expr, self.root, self.state).run()?;
        writeln!(self.state.streams.borrow_mut().out, "{val}").unwrap();
        Ok(())
    }
}

impl StatementRunner<'_, '_> {
    pub fn run(&self) -> Result<()> {
        match &self.ast.variant {
            AstNodeVariant::VariableDeclaration { sym, val, .. } => {
                let name = sym.sym_name();
                let val = ExpressionRunner::new(val, self.root, self.state).run()?;
                self.state.store_value(name, val);
            }

            AstNodeVariant::If { .. } => self.run_conditional_branch()?,

            AstNodeVariant::While { .. } => self.run_while()?,
            AstNodeVariant::Each { .. } => self.run_each()?,

            AstNodeVariant::Break => {
                self.state.stop_execution();
                self.state.exit_loop();
            }

            AstNodeVariant::Continue => {
                self.state.stop_execution();
            }

            AstNodeVariant::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let val = ExpressionRunner::new(expr, self.root, self.state).run()?;
                    self.state.set_return_value(val);
                };
                self.state.stop_execution();
            }

            AstNodeVariant::Debug { expr, .. } => self.run_debug_statement(expr)?,

            _ => {
                ExpressionRunner::new(self.ast, self.root, self.state).run()?;
            }
        };

        Ok(())
    }

    fn run_conditional_branch(&self) -> Result<()> {
        let cond_val = if let AstNodeVariant::If { cond, .. } = &self.ast.variant {
            ExpressionRunner::new(cond, self.root, self.state).run()?
        } else {
            unreachable!()
        };

        let should_exec = match cond_val {
            RuntimeValue::Bool(b) => b,
            _ => unreachable!(),
        };

        if should_exec {
            self.state.ss.borrow_mut().push(HashMap::new());
            BodyRunner::new(self.ast, self.root, self.state).run()?;
            self.state.ss.borrow_mut().pop();

            return Ok(());
        }

        if let AstNodeVariant::If { or_else, .. } = &self.ast.variant {
            if or_else.is_none() {
                return Ok(());
            }

            let alt = or_else.as_ref().unwrap();
            match &alt.as_ref().variant {
                AstNodeVariant::If { .. } => {
                    StatementRunner::new(alt, self.root, self.state).run()?;
                }

                AstNodeVariant::Else { .. } => {
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
            let cond_val = if let AstNodeVariant::While { cond, .. } = &self.ast.variant {
                ExpressionRunner::new(cond, self.root, self.state).run()?
            } else {
                unreachable!()
            };

            let should_exec = match cond_val {
                RuntimeValue::Bool(b) => b,
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

    fn run_each(&self) -> Result<()> {
        if let AstNodeVariant::Each {
            iterable, elem_sym, ..
        } = &self.ast.variant
        {
            let val = ExpressionRunner::new(iterable, self.root, self.state).run()?;
            let iterable = if let RuntimeValue::Array(ptr) = val {
                &self.state.array_arena.borrow()[ptr]
            } else {
                unreachable!()
            };

            let sym = elem_sym.sym_name();

            for item in iterable {
                let scope = HashMap::from([(String::from(sym), item.clone())]);

                self.state.ss.borrow_mut().push(scope);
                BodyRunner::new(self.ast, self.root, self.state).run()?;
                self.state.ss.borrow_mut().pop();

                if self.state.is_stop_executing() {
                    self.state.resume_execution();
                }

                if self.state.is_exiting_loop() {
                    self.state.reset_loop_state();
                    break;
                }
            }
        } else {
            unreachable!()
        }

        Ok(())
    }
}
