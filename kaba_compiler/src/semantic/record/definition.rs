use crate::ast::{AstNode, RecordField};
use crate::semantic::error::{SemanticError, SemanticErrorVariant};
use crate::semantic::tn;
use crate::semantic::{error::Result, state::AnalyzerState, types::Type};
use crate::AstNodeVariant;
use std::collections::HashSet;

pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let mut taken_name = HashSet::new();
    let mut fields = vec![];

    for field in record_fields(node) {
        let field_sym = &field.sym;
        let field_name = field_sym.sym_name();

        if taken_name.contains(field_name) {
            return Err(SemanticError {
                variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(field_name)),
                span: field_sym.span.clone(),
            });
        }

        let field_t = tn::analyze(state, &field.tn, false)?;
        fields.push((String::from(field_name), field_t));

        taken_name.insert(field_name);
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
