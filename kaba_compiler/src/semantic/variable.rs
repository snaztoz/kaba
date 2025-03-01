use super::{
    error::{Result, SemanticError, SemanticErrorVariant},
    expression,
    state::AnalyzerState,
    tn,
    typ::{assert, FloatType, IntType, Type},
};
use crate::ast::AstNode;
use std::borrow::Cow;

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
pub fn analyze(state: &mut AnalyzerState, node: &AstNode) -> Result<()> {
    let val = node.variant.as_variable_declaration_val();
    let val_t = expression::analyze(state, val)?;

    let var_t = match node.variant.as_variable_declaration_tn() {
        Some(var_tn) => {
            let t = match tn::analyze(state, var_tn, false)? {
                Type::Symbol(sym_name) => {
                    Cow::Borrowed(state.get_sym_variant(&sym_name).unwrap().as_type_t())
                }

                t => Cow::Owned(t),
            };

            assert::is_assignable(&val_t, &t, state, || node.span.clone())?;

            t
        }

        None if val_t.is_unbounded_int() => Cow::Owned(Type::Int(IntType::Int)),
        None if val_t.is_unbounded_float() => Cow::Owned(Type::Float(FloatType::Double)),
        None => val_t,
    };

    save_symbol(state, node, var_t.into_owned())
}

fn save_symbol(state: &mut AnalyzerState, node: &AstNode, t: Type) -> Result<()> {
    let sym = node.variant.as_sym();
    let sym_name = sym.variant.as_sym_name();

    if !state.can_save_sym(sym_name) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(sym_name)),
            span: node.span.clone(),
        });
    }

    state.save_entity(sym.id, sym_name, t);

    Ok(())
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
                    var x: void = 5;
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
    // Records
    //

    #[test]
    fn declaring_variable_with_record_literal() {
        assert_is_ok(indoc! {"
                def main {
                    var u = { name: \"snaztoz\" };
                }
            "});
    }

    #[test]
    fn declaring_variable_with_record_literal_and_type_notation() {
        assert_is_ok(indoc! {"
                record User {
                    name: string,
                }

                def main {
                    var u: User = { name: \"snaztoz\" };
                }
            "});
    }

    #[test]
    fn declaring_variable_with_record_type_notation() {
        assert_is_ok(indoc! {"
            def main {
                var u: {
                    name: string,
                    age: int,
                } = {
                    name: \"snaztoz\",
                    age: 23,
                };
            }
        "});
    }

    #[test]
    fn declaring_variable_with_record_type_notation_and_non_existing_type() {
        assert_is_err(indoc! {"
            def main {
                var u: {
                    name: NotExist,
                } = {
                    name: \"snaztoz\",
                };
            }
        "});
    }

    #[test]
    fn declaring_variable_with_nested_records() {
        assert_is_ok(indoc! {"
                record User {
                    name: string,
                    occupation: Occupation,
                }

                record Occupation {
                    name: string,
                }

                def main {
                    var u: User = {
                        name: \"snaztoz\",
                        occupation: { name: \"programmer\" },
                    };
                }
            "});
    }

    #[test]
    fn accessing_variable_field() {
        assert_is_ok(indoc! {"
                record User {
                    name: string,
                    occupation: Occupation,
                }

                record Occupation {
                    name: string,
                }

                def main {
                    var u: User = {
                        name: \"snaztoz\",
                        occupation: { name: \"programmer\" },
                    };

                    debug u.name;
                    debug u.occupation.name;
                }
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
