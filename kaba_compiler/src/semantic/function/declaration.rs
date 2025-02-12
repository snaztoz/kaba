use crate::{
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        state::AnalyzerState,
        tn,
        types::Type,
    },
    AstNode, FunctionParam,
};

pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    analyze_params_tn(state, node)?;
    analyze_return_tn(state, node)?;

    save_function_t(state, node)
}

fn analyze_params_tn(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    for FunctionParam { tn: param_tn, .. } in node.variant.params() {
        tn::analyze(state, param_tn, false)?;
    }
    Ok(())
}

fn analyze_return_tn(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    if let Some(return_tn) = node.variant.return_tn() {
        tn::analyze(state, return_tn, true)?;
    }
    Ok(())
}

fn save_function_t(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let sym = node.variant.sym();
    let sym_str = sym.variant.unwrap_symbol();

    let params_t = node
        .variant
        .params()
        .iter()
        .map(|p| Type::from(&p.tn))
        .collect();
    let return_t = Box::new(node.variant.return_tn().map_or(Type::Void, Type::from));
    let function_t = Type::Callable { params_t, return_t };

    if !state.can_save_sym(sym_str) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(sym_str)),
            span: sym.span.clone(),
        });
    }

    state.save_entity(sym.id, sym_str, function_t);

    Ok(())
}
