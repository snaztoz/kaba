use crate::semantic::{
    body,
    error::{Result, SemanticError, SemanticErrorVariant},
    state::AnalyzerState,
    types::Type,
};
use crate::{ast::AstNode, AstNodeVariant};

pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let (_, return_t) = get_function_t(state, node).unwrap_callable();

    state.with_function_scope(node.id, return_t.clone(), || {
        let body_t = body::analyze(state, node)?;

        if !return_t.is_void() && body_t.is_void() {
            return Err(SemanticError {
                variant: SemanticErrorVariant::ReturnTypeMismatch {
                    expected: return_t,
                    get: Type::Void,
                },
                span: node.variant.sym().span.clone(),
            });
        }

        Ok(())
    })?;

    Ok(Type::Void)
}

fn get_function_t(state: &AnalyzerState, node: &AstNode) -> Type {
    if let AstNodeVariant::FunctionDefinition { sym, .. } = &node.variant {
        let sym_string = &sym.variant.unwrap_symbol();
        state.get_sym_t(sym_string).unwrap().clone().unwrap_entity()
    } else {
        unreachable!()
    }
}
