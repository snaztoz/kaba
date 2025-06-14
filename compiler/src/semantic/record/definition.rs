use crate::ast::AstNode;
use crate::semantic::error::{SemanticError, SemanticErrorVariant};
use crate::semantic::tn;
use crate::semantic::{error::Result, state::AnalyzerState, typ::Type};
use std::collections::HashMap;

pub fn analyze(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    let mut fields = HashMap::new();

    for field in node.variant.as_record_fields() {
        let field_sym = &field.sym;
        let field_name = field_sym.variant.as_sym_name();

        if fields.contains_key(field_name) {
            return Err(SemanticError {
                variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(field_name)),
                span: field_sym.span.clone(),
            });
        }

        let field_t = tn::analyze(state, &field.tn, false)?;

        fields.insert(String::from(field_name), field_t);
    }

    state.set_type_definition(node.variant.as_sym().id, Type::Record { fields });

    Ok(())
}
