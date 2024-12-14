//! This module contains the implementation for type checking stage of the
//! compiler.

use self::{
    error::{Error, Result},
    types::Type,
};
use crate::ast::AstNode;
use function::{FunctionDeclarationChecker, FunctionDefinitionChecker};
use state::SharedState;

mod assignment;
mod body;
mod conditional;
mod each_loop;
mod error;
mod expression;
mod function;
mod literal;
mod state;
mod statement;
#[cfg(test)]
mod test_util;
mod tn;
mod types;
mod variable;
mod while_loop;

/// Provides a quick way to run semantic analysis on a Kaba AST.
pub fn check(program: &AstNode) -> Result<Type> {
    ProgramChecker::new(program).check()
}

struct ProgramChecker<'a> {
    program: &'a AstNode,
    state: SharedState,
}

impl<'a> ProgramChecker<'a> {
    fn new(program: &'a AstNode) -> Self {
        Self {
            program,
            state: SharedState::new(),
        }
    }
}

impl ProgramChecker<'_> {
    fn check(&self) -> Result<Type> {
        for stmt in self.body() {
            self.ensure_global_statement(stmt)?;
            FunctionDeclarationChecker::new(stmt, &self.state).check()?;
        }

        for stmt in self.body() {
            FunctionDefinitionChecker::new(stmt, &self.state).check()?;
        }

        Ok(Type::Void)
    }

    fn ensure_global_statement(&self, stmt: &AstNode) -> Result<()> {
        // We are expecting that in global scope, statements (currently) are
        // only consisted of function definitions, while other statements are
        // rejected in this scope.
        //
        // TODO: review other statements for possibilities to be applied here

        match stmt {
            AstNode::FunctionDefinition { .. } => Ok(()),

            n => Err(Error::UnexpectedStatement {
                stmt_str: n.to_string(),
                span: stmt.span().clone(),
            }),
        }
    }

    fn body(&self) -> &[AstNode] {
        if let AstNode::Program { body, .. } = self.program {
            body
        } else {
            unreachable!()
        }
    }
}
