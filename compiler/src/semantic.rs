//! This module contains the implementation for semantic analyzing stage of the
//! compiler.

use self::error::{Result, SemanticError};
use crate::ast::{AstNode, AstNodeVariant};
use error::SemanticErrorVariant;
use state::{AnalyzerState, SymbolTable};

mod assignment;
mod body;
mod conditional;
mod each_loop;
mod error;
mod expression;
mod function;
mod literal;
mod record;
pub mod state;
mod statement;
#[cfg(test)]
mod test_util;
mod tn;
mod typ;
mod variable;
mod while_loop;

/// Provides a quick way to run semantic analysis on a Kaba AST.
pub fn analyze(program: &AstNode) -> Result<SymbolTable> {
    let mut state = AnalyzerState::new(program.id);

    ensure_all_permitted_in_global(program.variant.as_body_statements())?;
    analyze_declarations(&mut state, program.variant.as_body_statements())?;
    analyze_definitions(&mut state, program.variant.as_body_statements())?;

    Ok(state.take_symbol_table())
}

fn analyze_declarations(state: &mut AnalyzerState, stmts: &[AstNode]) -> Result<()> {
    for stmt in stmts {
        if stmt.is_record_definition() {
            record::declaration::analyze(state, stmt)?;
        }
    }

    for stmt in stmts {
        if stmt.is_function_definition() {
            function::declaration::analyze(state, stmt)?;
        }
    }

    Ok(())
}

fn analyze_definitions(state: &mut AnalyzerState, stmts: &[AstNode]) -> Result<()> {
    for stmt in stmts {
        if stmt.is_record_definition() {
            record::definition::analyze(state, stmt)?;
        }
    }

    for stmt in stmts {
        if stmt.is_function_definition() {
            function::definition::analyze(state, stmt)?;
        }
    }

    Ok(())
}

fn ensure_all_permitted_in_global(stmts: &[AstNode]) -> Result<()> {
    // We are expecting that in global scope, statements (currently) are
    // only consisted of function definitions, while other statements are
    // rejected in this scope.
    //
    // TODO: review other statements for possibilities to be applied here

    for stmt in stmts {
        match &stmt.variant {
            AstNodeVariant::FunctionDefinition { .. } | AstNodeVariant::RecordDefinition { .. } => {
            }

            variant => {
                return Err(SemanticError {
                    variant: SemanticErrorVariant::UnexpectedStatement(variant.to_string()),
                    span: stmt.span.clone(),
                })
            }
        }
    }

    Ok(())
}
