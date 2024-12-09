use super::{error::Result, expression::ExpressionChecker, scope::ScopeStack, types::Type};
use crate::ast::Literal;

/// Checker for a literal expressions, such as numbers or arrays.
pub struct LiteralChecker<'a> {
    ss: &'a ScopeStack,
    lit: &'a Literal,
}

impl<'a> LiteralChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, lit: &'a Literal) -> Self {
        Self { ss, lit }
    }
}

impl LiteralChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        match self.lit {
            Literal::Void => Ok(Type::Void),

            Literal::Integer(_) => Ok(Type::UIntLiteral),
            Literal::Float(_) => Ok(Type::Float),
            Literal::Boolean(_) => Ok(Type::Bool),

            Literal::Array(_) => self.check_array(),
        }
    }

    fn check_array(&self) -> Result<Type> {
        let arr = if let Literal::Array(arr) = self.lit {
            arr
        } else {
            unreachable!()
        };

        // Doing double checking.
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
        // Lastly, we re-run the checking process against the type of T for
        // every element inside the array.

        let mut elem_t = None;

        for elem in arr {
            let t = ExpressionChecker::new(self.ss, elem).check()?;
            if !t.is_array_with_unknown_elem_t() {
                elem_t = Some(t);
                break;
            }
        }

        if elem_t.is_none() {
            return Ok(Type::Array { elem_t: None });
        }

        for elem in arr {
            let t = ExpressionChecker::new(self.ss, elem).check()?;
            Type::assert_assignable(&t, elem_t.as_ref().unwrap(), || elem.span().clone())?;
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
                elem_t: Some(Box::new(Type::UIntLiteral)),
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
                elem_t: Some(Box::new(Type::UIntLiteral)),
            },
        );
    }

    #[test]
    fn nested_array_literals() {
        assert_expression_type(
            "[[1, 3, 5], [2, 6, 3], [9, 1, 1]];",
            Type::Array {
                elem_t: Some(Box::new(Type::Array {
                    elem_t: Some(Box::new(Type::UIntLiteral)),
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
                    elem_t: Some(Box::new(Type::UIntLiteral)),
                })),
            },
        );
    }
}
