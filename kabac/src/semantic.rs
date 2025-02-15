//! This module contains the implementation for semantic analyzing stage of the
//! compiler.

use self::error::{Result, SemanticError};
use crate::ast::AstNode;
use error::SemanticErrorVariant;
use function::{FunctionDeclarationAnalyzer, FunctionDefinitionAnalyzer};
use state::{AnalyzerState, SymbolTableData};

mod assignment;
mod body;
mod conditional;
mod each_loop;
mod error;
mod expression;
mod function;
mod literal;
pub mod state;
mod statement;
#[cfg(test)]
mod test_util;
mod tn;
mod types;
mod variable;
mod while_loop;

/// Provides a quick way to run semantic analysis on a Kaba AST.
pub fn analyze(program: &AstNode) -> Result<SymbolTableData> {
    ProgramAnalyzer::new(program).analyze()
}

struct ProgramAnalyzer<'a> {
    program: &'a AstNode,
    state: AnalyzerState,
}

impl<'a> ProgramAnalyzer<'a> {
    fn new(program: &'a AstNode) -> Self {
        Self {
            program,
            state: AnalyzerState::new(),
        }
    }
}

impl ProgramAnalyzer<'_> {
    fn analyze(self) -> Result<SymbolTableData> {
        for stmt in self.body() {
            self.ensure_global_statement(stmt)?;
            FunctionDeclarationAnalyzer::new(stmt, &self.state).analyze()?;
        }

        for stmt in self.body() {
            FunctionDefinitionAnalyzer::new(stmt, &self.state).analyze()?;
        }

        Ok(self.state.take_symbols())
    }

    fn ensure_global_statement(&self, stmt: &AstNode) -> Result<()> {
        // We are expecting that in global scope, statements (currently) are
        // only consisted of function definitions, while other statements are
        // rejected in this scope.
        //
        // TODO: review other statements for possibilities to be applied here

        match stmt {
            AstNode::FunctionDefinition { .. } => Ok(()),

            n => Err(SemanticError {
                variant: SemanticErrorVariant::UnexpectedStatement(n.to_string()),
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
