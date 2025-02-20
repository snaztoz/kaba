use super::{
    error::{Result, SemanticError, SemanticErrorVariant},
    expression,
    state::AnalyzerState,
    tn,
    typ::{assert, FloatType, IntType, Type},
};
use crate::{ast::AstNode, AstNodeVariant};

/// Analyze variable declaration statement.
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
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let val_t = expression::analyze(state, unwrap_val(node))?;

    let var_t = match unwrap_tn(node) {
        Some(var_tn) => {
            let t = match tn::analyze(state, var_tn, false)? {
                Type::Symbol(sym_name) => state
                    .get_sym_variant(&sym_name)
                    .unwrap()
                    .clone()
                    .into_type_t(),

                t => t,
            };

            assert::is_assignable(&val_t, &t, || node.span.clone())?;

            t
        }

        None if val_t.is_unbounded_int() => Type::Int(IntType::Int),
        None if val_t.is_unbounded_float() => Type::Float(FloatType::Double),
        None => val_t,
    };

    save_symbol(state, node, var_t)
}

fn save_symbol(state: &AnalyzerState, node: &AstNode, t: Type) -> Result<()> {
    let sym = node.variant.as_sym();
    let sym_name = sym.variant.as_sym_name();
    let sym_span = node.span.clone();

    if !state.can_save_sym(sym_name) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(sym_name)),
            span: sym_span,
        });
    }

    state.save_entity(sym.id, sym_name, t);

    Ok(())
}

fn unwrap_tn<'src, 'a>(node: &'a AstNode<'src>) -> Option<&'a AstNode<'src>> {
    if let AstNodeVariant::VariableDeclaration { tn, .. } = &node.variant {
        tn.as_deref()
    } else {
        unreachable!()
    }
}

fn unwrap_val<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    if let AstNodeVariant::VariableDeclaration { val, .. } = &node.variant {
        val
    } else {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn declaring_variable_with_type_annotation_and_initial_value() {
        assert_is_ok(indoc! {"
                def main {
                    var x: int = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_type_inferring() {
        assert_is_ok(indoc! {"
                def main {
                    var x = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_builtin_type_symbol() {
        assert_is_err(indoc! {"
                def main {
                    var int = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_float_literal() {
        assert_is_ok(indoc! {"
                def main {
                    var x: float = -0.5;
                    var y: double = 9.99;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_different_int_types() {
        assert_is_err(indoc! {"
                def main {
                    var a: sbyte = 10;
                    var b: int = a;
                }
            "});
    }

    #[test]
    fn declaring_sbyte_variable_with_overflow_constant() {
        assert_is_err(indoc! {"
                def main {
                    var a: sbyte = 127 + 1;
                }
            "});
    }

    #[test]
    fn declaring_short_variable_with_overflow_constant() {
        assert_is_err(indoc! {"
                def main {
                    var a: short = -32768 - 1;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_bool_literal() {
        assert_is_ok(indoc! {"
                def main {
                    var x = true;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_char_literal() {
        assert_is_ok(indoc! {"
                def main {
                    var x = 'a';
                    var y: char = 'b';
                }
            "});
    }

    #[test]
    fn declaring_variable_with_string_literal() {
        assert_is_ok(indoc! {r#"
                def main {
                    var x = "abc def \n 123\t";
                    var y: string = "hello, world!";
                }
            "#});
    }

    #[test]
    fn declaring_variable_with_void_type() {
        assert_is_err(indoc! {"
                def main {
                    var x: Void = 5;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_type() {
        assert_is_err(indoc! {"
                def main {
                    var x: int = 5.0;
                }
            "})
    }

    #[test]
    fn declaring_variable_with_non_existing_type() {
        assert_is_err(indoc! {"
                def main {
                    var x: NonExistingType = 10;
                }
            "})
    }

    #[test]
    fn redeclaring_variable_in_the_same_scope() {
        assert_is_err(indoc! {"
                def main {
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
                def main {
                    var x: () -> int = produce;

                    debug x();
                }

                def produce: int {
                    return 5;
                }
            "});
    }

    #[test]
    fn declaring_callable_type_variable_with_non_existing_param_type() {
        assert_is_err(indoc! {"
                def main {}

                def produce(f: (NotExist) -> Void) {}
            "});
    }

    #[test]
    fn declaring_callable_type_variable_with_non_existing_return_type() {
        assert_is_err(indoc! {"
                def main {}

                def produce(f: () -> NotExist) {}
            "});
    }

    //
    // Arrays
    //

    #[test]
    fn declaring_variable_with_array_literal() {
        assert_is_ok(indoc! {"
                def main {
                    var arr = [int 1];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_empty_array_literal() {
        assert_is_ok(indoc! {"
                def main {
                    var arr = [int];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_array_type_notation() {
        assert_is_ok(indoc! {"
                def main {
                    var arr: []int = [int 5, 9, 10];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_empty_array_literal_and_type_notation() {
        assert_is_ok(indoc! {"
                def main {
                    var arr: []int = [int];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_array_literal() {
        assert_is_err(indoc! {"
                def main {
                    var arr: []int = [short 5];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_nested_arrays_and_type_notation() {
        assert_is_ok(indoc! {"
                def main {
                    var arr: [][]int = [[]int [int 5, 9, 10], [int 1, 2, 3]];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_nested_empty_arrays_and_type_notation() {
        assert_is_ok(indoc! {"
                def main {
                    var arr: [][][]int = [[][]int [[]int], [[]int]];
                }
            "});
    }

    #[test]
    fn declaring_variables_of_sbyte_array() {
        assert_is_ok(indoc! {"
                def main {
                    var arr: []sbyte = [sbyte];

                    var arr2: []sbyte = [sbyte 1, 2, 3];
                }
            "});
    }

    #[test]
    fn declaring_variables_of_long_array_with_math_expr_in_literal() {
        assert_is_ok(indoc! {"
                def main {
                    var x: long = 10;
                    var arr: []long = [long 1, 2, 3 + x];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_array_literal_of_function_types() {
        assert_is_ok(indoc! {"
                def main {
                    var arr = [()->int five, six];
                }

                def five: int {
                    return 5;
                }

                def six: int {
                    return 6;
                }
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_element_types() {
        assert_is_err(indoc! {"
                def main {
                    var arr = [int 1, 0.5];
                }
            "});
    }

    #[test]
    fn declaring_variable_with_non_existing_array_type() {
        assert_is_err(indoc! {"
                def main {
                    var arr = [NotExist];
                }
            "})
    }
}
