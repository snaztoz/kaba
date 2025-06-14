use super::{
    error::{Result, SemanticError, SemanticErrorVariant},
    state::AnalyzerState,
    typ::Type,
};
use crate::ast::AstNode;
use std::collections::HashSet;

pub fn analyze(state: &AnalyzerState, node: &AstNode, allow_void: bool) -> Result<Type> {
    if node.variant.as_tn().is_record() {
        let mut names = HashSet::new();

        for field in node.variant.as_tn().as_record_fields() {
            let sym_name = field.sym.variant.as_sym_name();

            if names.contains(sym_name) {
                return Err(SemanticError {
                    variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(sym_name)),
                    span: field.sym.span.clone(),
                });
            }

            names.insert(sym_name);
        }
    }

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
