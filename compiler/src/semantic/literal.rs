use super::{
    error::Result,
    expression::ExpressionAnalyzer,
    state::SharedState,
    tn::TypeNotationAnalyzer,
    types::{assert, FloatType, IntType, Type},
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
            Literal::Int(n) => Ok(Type::Int(IntType::Unbounded(*n))),
            Literal::Float(n) => Ok(Type::Float(FloatType::Unbounded(*n))),
            Literal::Char(_) => Ok(Type::Char),
            Literal::String(_) => Ok(Type::String),

            Literal::Array { .. } => self.analyze_array(),
        }
    }

    fn analyze_array(&self) -> Result<Type> {
        let (elem_t, elems) = if let Literal::Array { elem_tn, elems } = self.lit {
            let elem_t = TypeNotationAnalyzer::new(elem_tn, self.state).analyze()?;
            (elem_t, elems)
        } else {
            unreachable!()
        };

        for elem in elems {
            let t = ExpressionAnalyzer::new(elem, self.state).analyze()?;
            assert::is_compatible(&elem_t, &t, || elem.span().clone())?;
        }

        Ok(Type::Array {
            elem_t: Box::new(elem_t),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::{
        test_util::{assert_expr_is_err, assert_expr_type},
        types::{IntType, Type},
    };

    #[test]
    fn array_literal() {
        assert_expr_type(
            "[]int{ 1, 2, 3 };",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Int(IntType::Int)),
            },
        );
    }

    #[test]
    fn empty_array_literal() {
        assert_expr_type(
            "[]int{};",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Int(IntType::Int)),
            },
        );
    }

    #[test]
    fn array_literal_with_math_expression() {
        assert_expr_type(
            "[]int{ 8 * 2048 };",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Int(IntType::Int)),
            },
        );
    }

    #[test]
    fn array_literal_with_incompatible_type() {
        let symbols = [("x", Type::Int(IntType::Short))];
        assert_expr_is_err("[]int{ 1, x, 5 };", &symbols);
    }

    #[test]
    fn nested_array_literals() {
        assert_expr_type(
            "[][]int{ []int{1, 3, 5}, []int{2, 6, 3}, []int{9, 1, 1} };",
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
        assert_expr_is_err("[]int{ 1, 5.0 };", &[]);
    }

    #[test]
    fn array_literal_with_different_nested_element_sizes() {
        assert_expr_type(
            "[][]int{ []int{}, []int{1} };",
            &[],
            Type::Array {
                elem_t: Box::new(Type::Array {
                    elem_t: Box::new(Type::Int(IntType::Int)),
                }),
            },
        );
    }
}
