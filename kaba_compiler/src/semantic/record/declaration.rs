use crate::{
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        state::AnalyzerState,
    },
    AstNode,
};

pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let sym = node.variant.as_sym();
    let sym_name = sym.variant.as_sym_name();

    if !state.can_save_sym(sym_name) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(sym_name)),
            span: sym.span.clone(),
        });
    }

    state.save_type_declaration(sym.id, sym_name);

    Ok(())
}
