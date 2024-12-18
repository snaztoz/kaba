use super::{
    error::Result,
    expression::ExpressionAnalyzer,
    state::SharedState,
    types::{assert, Type},
};
use crate::ast::Literal;

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
        let arr = if let Literal::Array(arr) = self.lit {
            arr
        } else {
            unreachable!()
        };

        // Perform double analyzing.
        //
        // For example, in the case of a nested array (let's call it A) like:
        //
        //      [[], [1]]
        //
        // The array type can't be inferred based on the first element only,
        // which is an empty array.
        //
        // The solution is to iterate through all elements and stop on the
        // first non-empty array, whose type can be inferred (let's call it T).
        //
        // Then the type of A will be set to `[]T`.
        //
        // Lastly, we re-run the analyzing process against the type of T for
        // every element inside the array.

        let mut elem_t = None;

        for elem in arr {
            let t = ExpressionAnalyzer::new(elem, self.state).analyze()?;
            if !t.is_array_with_unknown_elem_t() {
                elem_t = Some(t);
                break;
            }
        }

        if elem_t.is_none() {
            return Ok(Type::Array { elem_t: None });
        }

        for elem in arr {
            let t = ExpressionAnalyzer::new(elem, self.state).analyze()?;
            assert::is_assignable(&t, elem_t.as_ref().unwrap(), || elem.span().clone())?;
        }

        Ok(Type::Array {
            elem_t: elem_t.map(Box::new),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::{
        test_util::{assert_expression_is_err, assert_expression_type},
        types::Type,
    };

    #[test]
    fn array_literal() {
        assert_expression_type(
            "[1, 2, 3];",
            Type::Array {
                elem_t: Some(Box::new(Type::UnboundedInt)),
            },
        );
    }

    #[test]
    fn empty_array_literal() {
        assert_expression_type("[];", Type::Array { elem_t: None });
    }

    #[test]
    fn array_literal_with_math_expression() {
        assert_expression_type(
            "[8 * 2048];",
            Type::Array {
                elem_t: Some(Box::new(Type::UnboundedInt)),
            },
        );
    }

    #[test]
    fn nested_array_literals() {
        assert_expression_type(
            "[[1, 3, 5], [2, 6, 3], [9, 1, 1]];",
            Type::Array {
                elem_t: Some(Box::new(Type::Array {
                    elem_t: Some(Box::new(Type::UnboundedInt)),
                })),
            },
        );
    }

    #[test]
    fn array_literal_with_different_element_types() {
        assert_expression_is_err("[1, 5.0];");
    }

    #[test]
    fn array_literal_with_different_nested_element_sizes() {
        assert_expression_type(
            "[[], [1]];",
            Type::Array {
                elem_t: Some(Box::new(Type::Array {
                    elem_t: Some(Box::new(Type::UnboundedInt)),
                })),
            },
        );
    }
}
