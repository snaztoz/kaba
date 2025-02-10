use super::{
    body,
    error::{Result, SemanticError, SemanticErrorVariant},
    state::AnalyzerState,
    tn,
    types::Type,
};
use crate::ast::{AstNode, FunctionParam};

pub fn analyze_declaration(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    analyze_params_tn(state, node)?;
    analyze_return_tn(state, node)?;

    save_function_t(state, node)
}

pub fn analyze_definition(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let scope_id = node.scope_id();

    let (params_t, return_t) = get_function_t(state, node).unwrap_callable();

    state.with_function_scope(scope_id, return_t.clone(), || {
        let params = node.params().iter().zip(params_t);

        for (param, t) in params {
            let sym_id = param.sym_id;
            let (sym, sym_span) = &param.sym.unwrap_symbol();

            if !state.can_save_sym(sym) {
                return Err(SemanticError {
                    variant: SemanticErrorVariant::SymbolAlreadyExist(sym.to_string()),
                    span: sym_span.clone(),
                });
            }

            state.save_entity(sym_id, sym, t);
        }

        // Analyze function body
        //
        // We do this last in order to accommodate features such as
        // recursive function call.

        let body_t = body::analyze(state, node)?;

        if !return_t.is_void() && body_t.is_void() {
            return Err(SemanticError {
                variant: SemanticErrorVariant::ReturnTypeMismatch {
                    expected: return_t,
                    get: Type::Void,
                },
                span: node.sym().span().clone(),
            });
        }

        Ok(())
    })?;

    Ok(Type::Void)
}

fn analyze_params_tn(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    for FunctionParam { tn: param_tn, .. } in node.params() {
        tn::analyze(state, param_tn, false)?;
    }
    Ok(())
}

fn analyze_return_tn(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    if let Some(return_tn) = node.return_tn() {
        tn::analyze(state, return_tn, true)?;
    }
    Ok(())
}

fn save_function_t(state: &AnalyzerState, node: &AstNode) -> Result<()> {
    let (sym, sym_span) = node.sym().unwrap_symbol();

    let params_t = node.params().iter().map(|p| Type::from(&p.tn)).collect();
    let return_t = Box::new(node.return_tn().map_or(Type::Void, Type::from));
    let function_t = Type::Callable { params_t, return_t };

    if !state.can_save_sym(sym) {
        return Err(SemanticError {
            variant: SemanticErrorVariant::SymbolAlreadyExist(String::from(sym)),
            span: sym_span,
        });
    }

    state.save_entity(node.sym_id(), sym, function_t);

    Ok(())
}

fn get_function_t(state: &AnalyzerState, node: &AstNode) -> Type {
    if let AstNode::FunctionDefinition { sym, .. } = node {
        let sym_string = &sym.unwrap_symbol().0;
        state.get_sym_t(sym_string).unwrap().unwrap_entity()
    } else {
        unreachable!()
    }
}

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
