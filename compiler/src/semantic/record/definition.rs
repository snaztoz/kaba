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

    let record_sym = node.variant.as_sym();

    state.define_type(
        record_sym.id,
        Type::Record {
            name: String::from(record_sym.variant.as_sym_name()),
            fields,
        },
    );

    Ok(())
}
