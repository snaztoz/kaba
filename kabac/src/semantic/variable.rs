use super::{
    error::{Result, SemanticError, SemanticErrorVariant},
    expression::ExpressionAnalyzer,
    state::AnalyzerState,
    tn::TypeNotationAnalyzer,
    types::{assert, FloatType, IntType, Type},
};
use crate::ast::{AstNode, SymbolId};
use logos::Span;

/// Analyzer for variable declaration statement.
///
/// ### ✅ Valid Examples
///
/// * Below is the example of a valid, full-form variable declaration:
///
/// ```text
/// var x: int = 5;
/// ```
///
/// * The type can also be inferred:
///
/// ```text
/// var x = 99;
/// ```
///
/// ### ❌ Invalid Examples
///
/// * Variable can't be created without providing the initial value:
///
/// ```text
/// var x: int;
/// ```
///
/// * If type notation presents, the provided initial value must also be
///   assignable to the type:
///
/// ```text
/// var x: int = 5.0;
/// ```
pub struct VariableDeclarationAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> VariableDeclarationAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
        Self { node, state }
    }
}

impl VariableDeclarationAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let val_t = ExpressionAnalyzer::new(self.val(), self.state).analyze()?;

        let var_t = match self.tn() {
            Some(tn) => {
                let t = TypeNotationAnalyzer::new(tn, self.state).analyze()?;
                assert::is_assignable(&val_t, &t, || self.span().clone())?;
                t
            }

            None if val_t.is_unbounded_int() => Type::Int(IntType::Int),
            None if val_t.is_unbounded_float() => Type::Float(FloatType::Double),
            None => val_t,
        };

        self.save_symbol(var_t)?;

        Ok(Type::Void)
    }

    fn sym_string(&self) -> String {
        if let AstNode::VariableDeclaration { sym, .. } = self.node {
            sym.unwrap_symbol().0
        } else {
            unreachable!()
        }
    }

    fn sym_id(&self) -> SymbolId {
        if let AstNode::VariableDeclaration { sym_id, .. } = self.node {
            *sym_id
        } else {
            unreachable!()
        }
    }

    fn sym_span(&self) -> Span {
        if let AstNode::VariableDeclaration { sym, .. } = self.node {
            sym.unwrap_symbol().1
        } else {
            unreachable!()
        }
    }

    fn tn(&self) -> Option<&AstNode> {
        if let AstNode::VariableDeclaration { tn, .. } = self.node {
            tn.as_deref()
        } else {
            unreachable!()
        }
    }

    fn val(&self) -> &AstNode {
        if let AstNode::VariableDeclaration { val, .. } = self.node {
            val
        } else {
            unreachable!()
        }
    }

    fn span(&self) -> &Span {
        if let AstNode::VariableDeclaration { span, .. } = self.node {
            span
        } else {
            unreachable!()
        }
    }

    fn save_symbol(&self, t: Type) -> Result<()> {
        self.state
            .save_entity_or_else(self.sym_id(), &self.sym_string(), t, || SemanticError {
                variant: SemanticErrorVariant::SymbolAlreadyExist(self.sym_string()),
                span: self.sym_span().clone(),
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn declaring_variable_with_type_annotation_and_initial_value() {
        assert_is_ok(indoc! {"
                fn main() {
                    var x: int = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_type_inferring() {
        assert_is_ok(indoc! {"
                fn main() {
                    var x = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_builtin_type_symbol() {
        assert_is_err(indoc! {"
                fn main() {
                    var int = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_float_literal() {
        assert_is_ok(indoc! {"
                fn main() {
                    var x: float = -0.5;
                    var y: double = 9.99;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_different_int_types() {
        assert_is_err(indoc! {"
                fn main() {
                    var a: sbyte = 10;
                    var b: int = a;
                }
            "});
    }

    #[test]
    fn declaring_sbyte_variable_with_overflow_constant() {
        assert_is_err(indoc! {"
                fn main() {
                    var a: sbyte = 127 + 1;
                }
            "});
    }

    #[test]
    fn declaring_short_variable_with_overflow_constant() {
        assert_is_err(indoc! {"
                fn main() {
                    var a: short = -32768 - 1;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_bool_literal() {
        assert_is_ok(indoc! {"
                fn main() {
                    var x = true;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_char_literal() {
        assert_is_ok(indoc! {"
                fn main() {
                    var x = 'a';
                    var y: char = 'b';
                }
            "});
    }

    #[test]
    fn declaring_variable_with_string_literal() {
        assert_is_ok(indoc! {r#"
                fn main() {
                    var x = "abc def \n 123\t";
                    var y: string = "hello, world!";
                }
            "#});
    }

    #[test]
    fn declaring_variable_with_void_type() {
        assert_is_err(indoc! {"
                fn main() {
                    var x: Void = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_type() {
        assert_is_err(indoc! {"
                fn main() {
                    var x: int = 5.0;
                }
            "})
    }

    #[test]
    fn declaring_variable_with_non_existing_type() {
        assert_is_err(indoc! {"
                fn main() {
                    var x: NonExistingType = 10;
                }
            "})
    }

    #[test]
    fn redeclaring_variable_in_the_same_scope() {
        assert_is_err(indoc! {"
                fn main() {
                    var x = 5;
                    var x = 10;
                }
            "})
    }

    //
    // Functions
    //

    #[test]
    fn declaring_variable_with_function_pointer_as_value() {
        assert_is_ok(indoc! {"
                fn main() {
                    var x: () -> int = produce;

                    debug x();
                }

                fn produce(): int {
                    return 5;
                }
            "});
    }

    #[test]
    fn declaring_callable_type_variable_with_non_existing_param_type() {
        assert_is_err(indoc! {"
                fn main() {}

                fn produce(f: (NotExist) -> Void) {}
            "});
    }

    #[test]
    fn declaring_callable_type_variable_with_non_existing_return_type() {
        assert_is_err(indoc! {"
                fn main() {}

                fn produce(f: () -> NotExist) {}
            "});
    }

    //
    // Arrays
    //

    #[test]
    fn declaring_variable_with_array_literal() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr = [int 1];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_empty_array_literal() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr = [int];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_array_type_notation() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr: []int = [int 5, 9, 10];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_empty_array_literal_and_type_notation() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr: []int = [int];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_array_literal() {
        assert_is_err(indoc! {"
                fn main() {
                    var arr: []int = [short 5];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_nested_arrays_and_type_notation() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr: [][]int = [[]int [int 5, 9, 10], [int 1, 2, 3]];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_nested_empty_arrays_and_type_notation() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr: [][][]int = [[][]int [[]int], [[]int]];
                }
            "});
    }

    #[test]
    fn declaring_variables_of_sbyte_array() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr: []sbyte = [sbyte];

                    var arr2: []sbyte = [sbyte 1, 2, 3];
                }
            "});
    }

    #[test]
    fn declaring_variables_of_long_array_with_math_expr_in_literal() {
        assert_is_ok(indoc! {"
                fn main() {
                    var x: long = 10;
                    var arr: []long = [long 1, 2, 3 + x];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_array_literal_of_function_types() {
        assert_is_ok(indoc! {"
                fn main() {
                    var arr = [()->int five, six];
                }

                fn five(): int {
                    return 5;
                }

                fn six(): int {
                    return 6;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_element_types() {
        assert_is_err(indoc! {"
                fn main() {
                    var arr = [int 1, 0.5];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_non_existing_array_type() {
        assert_is_err(indoc! {"
                fn main() {
                    var arr = [NotExist];
                }
            "})
    }
}
