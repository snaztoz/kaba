use crate::{
    ast::{AstNode, ObjectInitializer},
    semantic::{
        error::{Result, SemanticError, SemanticErrorVariant},
        expression,
        state::AnalyzerState,
        tn,
        typ::{assert, Type},
    },
};
use std::{borrow::Cow, collections::HashSet};

pub fn analyze<'a>(state: &'a AnalyzerState, node: &AstNode) -> Result<Cow<'a, Type>> {
    let tn = node.variant.as_object_creation_tn();

    let t = match tn::analyze(state, tn, false)? {
        Type::Symbol(sym_name) => {
            Cow::Borrowed(state.get_sym_variant(&sym_name).unwrap().as_type_t())
        }

        t => Cow::Owned(t),
    };

    if t.is_array() {
        analyze_array_creation(state, node, t)
    } else if t.is_record() {
        analyze_record_creation(state, node, t)
    } else {
        todo!("creation for other object types")
    }
}

fn analyze_array_creation<'a>(
    state: &'a AnalyzerState,
    node: &AstNode,
    obj_t: Cow<'a, Type>,
) -> Result<Cow<'a, Type>> {
    let elem_t = obj_t.as_array_elem_t();
    let initializer = node.variant.as_object_creation_initializer();

    match initializer {
        ObjectInitializer::Array(arr) => {
            for item in arr {
                let t = expression::analyze(state, item)?;
                assert::is_assignable(&t, elem_t, state, || item.span.clone())?;
            }
        }

        ObjectInitializer::KeyVal(_) => {
            return Err(SemanticError {
                variant: SemanticErrorVariant::KeyvalInitializerInArrayCreation,
                span: node.span.clone(),
            });
        }

        ObjectInitializer::Empty => (),
    };

    Ok(obj_t)
}

fn analyze_record_creation<'a>(
    state: &'a AnalyzerState,
    node: &AstNode,
    obj_t: Cow<'a, Type>,
) -> Result<Cow<'a, Type>> {
    let initializer = node.variant.as_object_creation_initializer();
    let record_fields = obj_t.as_record_fields();

    match initializer {
        ObjectInitializer::Array(_) => {
            todo!("record creation with array initializer syntax")
        }

        ObjectInitializer::KeyVal(fields) => {
            let mut unassigned = record_fields.keys().cloned().collect::<HashSet<_>>();

            for (key, val) in fields {
                if !key.is_symbol() {
                    return Err(SemanticError {
                        variant: SemanticErrorVariant::KeyvalInitializerWithNonSymbolKey,
                        span: key.span.clone(),
                    });
                }

                let key_sym = key.variant.as_sym_name();

                if !record_fields.contains_key(key_sym) {
                    return Err(SemanticError {
                        variant: SemanticErrorVariant::FieldDoesNotExist {
                            t: obj_t.into_owned(),
                            field: String::from(key_sym),
                        },
                        span: key.span.clone(),
                    });
                }

                let val_t = expression::analyze(state, val)?;

                assert::is_assignable(&val_t, record_fields.get(key_sym).unwrap(), state, || {
                    val.span.clone()
                })?;

                unassigned.remove(key_sym);
            }

            if !unassigned.is_empty() {
                let mut fields = unassigned.into_iter().collect::<Vec<_>>();
                fields.sort();

                return Err(SemanticError {
                    variant: SemanticErrorVariant::MissingRecordFields(fields),
                    span: node.span.clone(),
                });
            }
        }

        ObjectInitializer::Empty => {
            if !record_fields.is_empty() {
                let fields = record_fields.keys().cloned().collect::<Vec<_>>();

                return Err(SemanticError {
                    variant: SemanticErrorVariant::MissingRecordFields(fields),
                    span: node.span.clone(),
                });
            }
        }
    };

    Ok(obj_t)
}
