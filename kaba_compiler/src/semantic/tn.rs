use super::{
    error::{Result, SemanticError, SemanticErrorVariant},
    state::AnalyzerState,
    types::Type,
};
use crate::ast::AstNode;

pub fn analyze(state: &AnalyzerState, node: &AstNode, allow_void: bool) -> Result<Type> {
    let t = Type::from(node);

    // The provided type must exist in the current scope
    if !state.has_t(&t) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::SymbolDoesNotExist(t.to_string()),
            span: node.span.clone(),
        });
    }

    if !allow_void && t.is_void() {
        return Err(SemanticError {
            variant: SemanticErrorVariant::VoidTypeVariable,
            span: node.span.clone(),
        });
    }

    Ok(t)
}
