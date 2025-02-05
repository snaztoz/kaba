//! This module contains the implementation for semantic analyzing stage of the
//! compiler.

use self::error::{Result, SemanticError};
use crate::ast::AstNode;
use error::SemanticErrorVariant;
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
    let state = AnalyzerState::new();

    for stmt in program.body() {
        ensure_permitted_in_global(stmt)?;
        function::analyze_declaration(&state, stmt)?;
    }

    for stmt in program.body() {
        function::analyze_definition(&state, stmt)?;
    }

    Ok(state.take_symbols())
}

fn ensure_permitted_in_global(stmt: &AstNode) -> Result<()> {
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
