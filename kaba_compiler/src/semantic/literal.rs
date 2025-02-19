use super::{
    error::{Result, SemanticError, SemanticErrorVariant},
    expression,
    state::AnalyzerState,
    tn,
    typ::{assert, FloatType, IntType, Type},
};
use crate::{ast::Literal, AstNode, AstNodeVariant};
use std::collections::HashMap;

/// Analyze literal expressions, such as numbers or arrays.
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let lit = unwrap_literal(node);

    match lit {
        Literal::Void => Ok(Type::Void),

        Literal::Bool(_) => Ok(Type::Bool),
        Literal::Int(n) => Ok(Type::Int(IntType::Unbounded(*n))),
        Literal::Float(n) => Ok(Type::Float(FloatType::Unbounded(*n))),
        Literal::Char(_) => Ok(Type::Char),
        Literal::String(_) => Ok(Type::String),

        Literal::Array { .. } => analyze_array(state, lit),
        Literal::Record { .. } => analyze_record(state, lit),
    }
}

fn analyze_array(state: &AnalyzerState, lit: &Literal) -> Result<Type> {
    let (elem_t, elems) = match lit {
        Literal::Array { elem_tn, elems } => {
            let elem_t = tn::analyze(state, elem_tn, false)?;
            (elem_t, elems)
        }

        _ => unreachable!(),
    };

    for elem in elems {
        let t = expression::analyze(state, elem)?;
        assert::is_compatible(&elem_t, &t, || elem.span.clone())?;
    }

    Ok(Type::Array {
        elem_t: Box::new(elem_t),
    })
}

fn analyze_record(state: &AnalyzerState, lit: &Literal) -> Result<Type> {
    let fields = if let Literal::Record { fields } = lit {
        fields
    } else {
        unreachable!()
    };

    let mut fields_t = HashMap::new();

    for (name, val) in fields {
        let field_name = name.sym_name();

        if fields_t.contains_key(field_name) {
            return Err(SemanticError {
                variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(field_name)),
                span: name.span.clone(),
            });
        }

        let t = match expression::analyze(state, val)? {
            // Deduce unbounded types into bounded ones
            expr_t if expr_t.is_unbounded_int() => Type::Int(IntType::Int),
            expr_t if expr_t.is_unbounded_float() => Type::Float(FloatType::Double),

            expr_t => expr_t,
        };

        fields_t.insert(String::from(field_name), t);
    }

    Ok(Type::Record { fields: fields_t })
}

fn unwrap_literal<'src, 'a>(node: &'a AstNode<'src>) -> &'a Literal<'src> {
    match &node.variant {
        AstNodeVariant::Literal { lit, .. } => lit,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::{
        test_util::{assert_expr_is_err, assert_expr_type},
        typ::{IntType, Type},
    };

    #[test]
    fn array_literal() {
        assert_expr_type(
            "[int 1, 2, 3];",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Int(IntType::Int)),
            },
        );
    }

    #[test]
    fn empty_array_literal() {
        assert_expr_type(
            "[int];",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Int(IntType::Int)),
            },
        );
    }

    #[test]
    fn array_literal_with_math_expression() {
        assert_expr_type(
            "[int 8 * 2048];",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Int(IntType::Int)),
            },
        );
    }

    #[test]
    fn array_literal_with_incompatible_type() {
        let symbols = [("x", Type::Int(IntType::Short))];
        assert_expr_is_err("[int 1, x, 5];", &symbols);
    }

    #[test]
    fn nested_array_literals() {
        assert_expr_type(
            "[[]int [int 1, 3, 5], [int 2, 6, 3], [int 9, 1, 1]];",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Array {
                    elem_t: Box::new(Type::Int(IntType::Int)),
                }),
            },
        );
    }

    #[test]
    fn array_literal_with_different_element_types() {
        assert_expr_is_err("[int 1, 5.0];", &[]);
    }

    #[test]
    fn array_literal_with_different_nested_element_sizes() {
        assert_expr_type(
            "[[]int [int], [int 1]];",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Array {
                    elem_t: Box::new(Type::Int(IntType::Int)),
                }),
            },
        );
    }
}
