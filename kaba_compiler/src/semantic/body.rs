use super::{error::Result, state::AnalyzerState, statement, types::Type};
use crate::ast::AstNode;

/// Analyze a statement body.
///
/// Statement bodies are consist of `>= 0` statements, so this analyzer will
/// call the [`StatementAnalyzer`] on each statement found in current body.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let mut body_t = Type::Void;

    for stmt in node.variant.body() {
        let t = statement::analyze(state, stmt)?;
        if body_t == Type::Void {
            body_t = t;
        }
    }

    Ok(body_t)
}
