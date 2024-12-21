use super::{
    error::Result,
    expression::ExpressionAnalyzer,
    state::SharedState,
    types::{assert, Type},
};
use crate::ast::{AstNode, Literal};

/// Analyzer for a literal expressions, such as numbers or arrays.
pub struct LiteralAnalyzer<'a> {
    lit: &'a Literal,
    state: &'a SharedState,
}

impl<'a> LiteralAnalyzer<'a> {
    pub const fn new(lit: &'a Literal, state: &'a SharedState) -> Self {
        Self { lit, state }
    }
}

impl LiteralAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        match self.lit {
            Literal::Void => Ok(Type::Void),
            Literal::Bool(_) => Ok(Type::Bool),

            Literal::Int(_) => Ok(Type::UnboundedInt),
            Literal::Float(_) => Ok(Type::Float),

            Literal::Array(_) => self.analyze_array(),
        }
    }

    fn analyze_array(&self) -> Result<Type> {
        let mut elem_t = None;

        for elem in self.array() {
            let t = ExpressionAnalyzer::new(elem, self.state).analyze()?;

            // value with type `[]{unknown}` can't contribute to the inferring
            // process of the array literal, so we skip this.
            if t.is_array_with_unknown_elem_t() {
                continue;
            }

            if elem_t.is_none() {
                elem_t = Some(t);
                continue;
            }

            if let Some(current_t) = &elem_t {
                assert::is_compatible(current_t, &t, || elem.span().clone())?;
                // temporary
                if current_t.is_number() {
                    elem_t = Some(Type::largest_numeric_t_between(current_t, &t).clone());
                } else {
                    elem_t = Some(t);
                }
            }
        }

        Ok(Type::Array {
            elem_t: elem_t.map(Box::new),
        })
    }

    fn array(&self) -> &[AstNode] {
        if let Literal::Array(arr) = self.lit {
            arr
        } else {
            unreachable!()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::{
        test_util::{assert_expr_is_err, assert_expr_type},
        types::Type,
    };

    #[test]
    fn array_literal() {
        assert_expr_type(
            "[1, 2, 3];",
            &[],
            Type::Array {
                elem_t: Some(Box::new(Type::UnboundedInt)),
            },
        );
    }

    #[test]
    fn empty_array_literal() {
        assert_expr_type("[];", &[], Type::Array { elem_t: None });
    }

    #[test]
    fn array_literal_with_math_expression() {
        assert_expr_type(
            "[8 * 2048];",
            &[],
            Type::Array {
                elem_t: Some(Box::new(Type::UnboundedInt)),
            },
        );
    }

    #[test]
    fn array_literal_with_literal_after_variable_element() {
        let symbols = [("x", Type::Int)];
        assert_expr_type(
            "[1, x, 5];",
            &symbols,
            Type::Array {
                elem_t: Some(Box::new(Type::Int)),
            },
        );
    }

    #[test]
    fn nested_array_literals() {
        assert_expr_type(
            "[[1, 3, 5], [2, 6, 3], [9, 1, 1]];",
            &[],
            Type::Array {
                elem_t: Some(Box::new(Type::Array {
                    elem_t: Some(Box::new(Type::UnboundedInt)),
                })),
            },
        );
    }

    #[test]
    fn array_literal_with_different_element_types() {
        assert_expr_is_err("[1, 5.0];");
    }

    #[test]
    fn array_literal_with_different_nested_element_sizes() {
        assert_expr_type(
            "[[], [1]];",
            &[],
            Type::Array {
                elem_t: Some(Box::new(Type::Array {
                    elem_t: Some(Box::new(Type::UnboundedInt)),
                })),
            },
        );
    }
}
