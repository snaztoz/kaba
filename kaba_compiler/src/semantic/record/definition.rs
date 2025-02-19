use crate::ast::{AstNode, RecordField};
use crate::semantic::error::{SemanticError, SemanticErrorVariant};
use crate::semantic::tn;
use crate::semantic::{error::Result, state::AnalyzerState, types::Type};
use crate::AstNodeVariant;
use std::collections::HashMap;

pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let mut fields = HashMap::new();

    for field in record_fields(node) {
        let field_sym = &field.sym;
        let field_name = field_sym.sym_name();

        if fields.contains_key(field_name) {
            return Err(SemanticError {
                variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(field_name)),
                span: field_sym.span.clone(),
            });
        }

        let field_t = tn::analyze(state, &field.tn, false)?;

        fields.insert(String::from(field_name), field_t);
    }

    state.set_type_definition(node.sym().id, Type::Record { fields });

    Ok(())
}

fn record_fields<'a>(node: &'a AstNode) -> &'a [RecordField<'a>] {
    if let AstNodeVariant::RecordDefinition { fields, .. } = &node.variant {
        fields
    } else {
        unreachable!()
    }
}
