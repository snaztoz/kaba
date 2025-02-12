pub mod declaration;
pub mod definition;

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn defining_function_without_parameter_or_return_type() {
        assert_is_ok(indoc! {"
                def add {}
            "});
    }

    #[test]
    fn defining_duplicated_functions() {
        assert_is_err(indoc! {"
                def print_sum_of(a: int, b: int) {
                    debug a + b;
                }

                def print_sum_of(a: float, b: float) {
                    debug a + b;
                }
            "});
    }

    #[test]
    fn defining_functions_both_with_parameters_and_return_type() {
        assert_is_ok(indoc! {"
                def sum(x: int, y: int): int {
                    return x + y;
                }
            "});
    }

    #[test]
    fn recursive_fibonacci_function() {
        assert_is_ok(indoc! {"
                def fibonacci(n: int): int {
                    if n == 0 {
                        return 0;
                    } else if n == 1 || n == 2 {
                        return 1;
                    }
                    return fibonacci(n-1) + fibonacci(n-2);
                }
            "});
    }

    #[test]
    fn recursive_functions_with_void_return_type() {
        assert_is_ok(indoc! {"
                def count_to_zero(n: int) {
                    debug n;
                    if n == 0 {
                        return;
                    }
                    count_to_zero(n-1);
                }

                def main {
                    count_to_zero(10);
                }
            "});
    }

    #[test]
    fn returning_from_functions_with_conditional_and_loop_statements() {
        assert_is_ok(indoc! {"
                def first: int {
                    return 5;
                }

                def second: int {
                    if false {
                        return 0;
                    } else {
                        return 1;
                    }
                }

                def third: int {
                    if false {
                        return 0;
                    }
                    return 1;
                }

                def fourth: int {
                    while false {
                        return 0;
                    }
                    return 1;
                }

                def fifth: int {
                    return 1;

                    if false {
                        return 0;
                    }
                }
            "});
    }

    #[test]
    fn defining_functions_not_in_order() {
        assert_is_ok(indoc! {"
                def main {
                    call_foo();
                }

                def call_foo {
                    call_bar();
                }

                def call_bar {
                    debug true;
                }
            "})
    }

    #[test]
    fn prevent_incompatible_int_type_on_function_return_value() {
        assert_is_err(indoc! {"
                def main {
                    var x: int = foo();
                }

                def foo: sbyte {
                    return 5;
                }
            "});
    }

    #[test]
    fn prevent_calling_function_with_missing_args() {
        assert_is_err(indoc! {"
                def main {
                    foo();
                }

                def foo(x: sbyte): sbyte {
                    return x;
                }
            "});
    }

    #[test]
    fn prevent_calling_function_with_too_many_args() {
        assert_is_err(indoc! {"
                def main {
                    foo(5, 7);
                }

                def foo(x: sbyte): sbyte {
                    return x;
                }
            "});
    }

    #[test]
    fn aliasing_function_identifier() {
        assert_is_ok(indoc! {"
                def main {
                    var aliased = return_two;

                    debug aliased();
                }

                def return_two: int {
                    return 2;
                }
            "});
    }

    #[test]
    fn using_function_type_as_function_parameter() {
        assert_is_ok(indoc! {"
                def main {
                    debug get_num(produce);
                }

                def get_num(producer: () -> int): int {
                    return producer() + 5;
                }

                def produce: int {
                    return 10;
                }
            "});
    }

    #[test]
    fn calling_function_returned_by_another_function_call() {
        assert_is_ok(indoc! {"
                def main {
                    debug foo()();
                }

                def foo: () -> int {
                    return bar;
                }

                def bar: int {
                    return 25;
                }
            "});
    }

    #[test]
    fn calling_function_with_array_parameter() {
        assert_is_ok(indoc! {"
                def main {
                    foo([int 1, 2, 3]);
                }

                def foo(arr: []int) {}
            "});
    }

    #[test]
    fn calling_function_with_array_parameter_using_different_array_sizes() {
        assert_is_ok(indoc! {"
                def main {
                    foo([int 1, 2, 3]);

                    foo([int]);

                    foo([int 1]);
                }

                def foo(arr: []int) {}
            "});
    }

    #[test]
    fn returning_array_from_a_function() {
        assert_is_ok(indoc! {"
                def main {
                    var arr_1: []int = foo();
                    var arr_2: []int = foo();

                    arr_1[0] = 10;
                }

                def foo: []int {
                    return [int 1, 2, 3];
                }
            "});
    }

    #[test]
    fn defining_function_not_in_global_scope() {
        assert_is_err(indoc! {"
                def main {
                    if true {
                        def foo {}
                    }
                }
            "});
    }

    #[test]
    fn defining_function_with_a_non_existing_return_type() {
        assert_is_err(indoc! {"
                def foo: NonExistingType { }
            "});
    }

    #[test]
    fn defining_function_with_duplicated_parameter_name() {
        assert_is_err(indoc! {"
                def add_sum_of(x: int, x: int) { }
            "});
    }

    #[test]
    fn defining_function_with_a_non_existing_parameter_type() {
        assert_is_err(indoc! {"
                def foo(x: NonExistingType) { }
            "});
    }

    #[test]
    fn defining_function_with_void_parameter_type() {
        assert_is_err(indoc! {"
                def foo(x: Void) { }
            "});
    }

    #[test]
    fn returning_value_from_function_with_void_return_type() {
        assert_is_err(indoc! {"
                def foo {
                    return 5;
                }
            "});
    }

    #[test]
    fn returning_value_from_function_with_mismatched_return_type() {
        assert_is_err(indoc! {"
                def sum(x: int, y: int): int {
                    return 5.0;
                }
            "});
    }

    #[test]
    fn invalid_statement_after_return() {
        assert_is_err(indoc! {"
                def get_five: int {
                    return 5;
                    1 + true; // should be error
                }
            "});
    }

    #[test]
    fn returning_non_existing_variable() {
        assert_is_err(indoc! {"
                def foo: int {
                    return not_exist;
                }
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_other_branches() {
        assert_is_err(indoc! {"
                def foo: int {
                    if false {
                        return 5;
                    }
                }
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_else_branch_or_outer_scope() {
        assert_is_err(indoc! {"
                def foo: int {
                    if false {
                        return 0;
                    } else if !true {
                        return 0;
                    }
                }
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_outer_scope_of_while_statement() {
        assert_is_err(indoc! {"
                def foo: int {
                    while false {
                        return 0;
                    }
                }
            "});
    }
}
