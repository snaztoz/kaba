use crate::{
    ast::NodeId,
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        state::{AnalyzerState, ScopeVariant},
        tn,
        typ::Type,
    },
    AstNode, FunctionParam,
};
use std::collections::HashSet;

pub fn analyze(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    let sym = node.variant.as_sym();
    let sym_name = sym.variant.as_sym_name();

    if !state.can_save_sym(sym_name) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(sym_name)),
            span: sym.span.clone(),
        });
    }

    let params = analyze_params(state, node)?;
    let return_t = analyze_return_tn(state, node)?;

    let function_t = Type::Callable {
        params_t: params.iter().map(|p| p.2.clone()).collect(),
        return_t: Box::new(return_t.clone()),
    };

    state.save_entity(sym.id, sym_name, function_t);

    state.create_scope_inside_current_scope(node.id, ScopeVariant::Function { return_t });

    for (param_id, param_name, param_t) in params {
        state.save_entity_to(node.id, param_id, param_name, param_t);
    }

    Ok(())
}

fn analyze_params<'a>(
    state: &AnalyzerState,
    node: &'a AstNode,
) -> Result<Vec<(NodeId, &'a str, Type)>> {
    let mut params_sym = HashSet::new();
    let mut params = vec![];

    for FunctionParam {
        sym: param_sym,
        tn: param_tn,
    } in node.variant.as_function_params()
    {
        let param_name = param_sym.variant.as_sym_name();
        if params_sym.contains(&param_name) {
            return Err(SemanticError {
                variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(param_name)),
                span: param_sym.span.clone(),
            });
        }

        let t = tn::analyze(state, param_tn, false)?;
        params.push((param_sym.id, param_name, t));

        params_sym.insert(param_name);
    }

    Ok(params)
}

fn analyze_return_tn(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    match node.variant.as_function_return_tn() {
        Some(return_tn) => tn::analyze(state, return_tn, true),
        None => Ok(Type::Void),
    }
}
