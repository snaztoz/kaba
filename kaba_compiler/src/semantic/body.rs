use super::{error::Result, state::AnalyzerState, statement};
use crate::ast::AstNode;

/// Analyze a statement body.
///
/// Statement bodies are consist of `>= 0` statements, so this analyzer will
/// call the [`StatementAnalyzer`] on each statement found in current body.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    for stmt in node.body() {
        statement::analyze(state, stmt)?;
    }

    Ok(())
}
