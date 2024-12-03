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
            Literal::Void => Ok(Type::new("Void")),
            Literal::Integer(_) => Ok(Type::new("Int")),
            Literal::Float(_) => Ok(Type::new("Float")),
            Literal::Boolean(_) => Ok(Type::new("Bool")),

            Literal::Array(_) => self.check_array(),
        }
    }

    fn check_array(&self) -> Result<Type> {
        let arr = if let Literal::Array(arr) = self.lit {
            arr
        } else {
            unreachable!()
        };

        let mut elem_t = None;
        for elem in arr {
            let t = ExpressionChecker::new(self.ss, elem).check()?;

            if elem_t.is_none() {
                elem_t = Some(t);
                continue;
            }

            Type::assert_assignable(&t, elem_t.as_ref().unwrap(), || elem.span().clone())?;
        }

        Ok(Type::Array {
            size: Some(arr.len()),
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
                size: Some(3),
                elem_t: Some(Box::new(Type::new("Int"))),
            },
        );
    }

    #[test]
    fn empty_array_literal() {
        assert_expression_type(
            "[];",
            Type::Array {
                size: Some(0),
                elem_t: None,
            },
        );
    }

    #[test]
    fn array_literal_with_math_expression() {
        assert_expression_type(
            "[8 * 2048];",
            Type::Array {
                size: Some(1),
                elem_t: Some(Box::new(Type::new("Int"))),
            },
        );
    }

    #[test]
    fn nested_array_literals() {
        assert_expression_type(
            "[[1, 3, 5], [2, 6, 3], [9, 1, 1]];",
            Type::Array {
                size: Some(3),
                elem_t: Some(Box::new(Type::Array {
                    size: Some(3),
                    elem_t: Some(Box::new(Type::new("Int"))),
                })),
            },
        );
    }

    #[test]
    fn array_literal_with_different_element_types() {
        assert_expression_is_err("[1, 5.0];");
    }

    #[test]
    fn array_literal_with_different_nested_element_types() {
        assert_expression_is_err("[[1], []];");
    }
}
