//! This module contains the implementation for type checking stage of the
//! compiler.

use self::{
    error::{Error, Result},
    types::Type,
};
use crate::ast::AstNode;
use function::{FunctionDeclarationChecker, FunctionDefinitionChecker};
use scope::ScopeStack;

mod body;
mod error;
mod expression;
mod function;
mod scope;
mod statement;
mod tn;
mod types;

/// Provides a quick way to run semantic analysis on a Kaba AST.
pub fn check(program: &AstNode) -> Result<Type> {
    ProgramChecker::new(program).check()
}

struct ProgramChecker<'a> {
    ss: ScopeStack,
    program: &'a AstNode,
}

impl<'a> ProgramChecker<'a> {
    fn new(program: &'a AstNode) -> Self {
        Self {
            ss: ScopeStack::default(),
            program,
        }
    }
}

impl ProgramChecker<'_> {
    fn check(&self) -> Result<Type> {
        for stmt in self.body() {
            self.ensure_global_statement(stmt)?;
            FunctionDeclarationChecker::new(&self.ss, stmt).check()?;
        }

        for stmt in self.body() {
            FunctionDefinitionChecker::new(&self.ss, stmt).check()?;
        }

        Ok(Type::new("Void"))
    }

    fn ensure_global_statement(&self, stmt: &AstNode) -> Result<()> {
        // We are expecting that in global scope, statements (currently) are
        // only consisted of function definitions, while other statements are
        // rejected in this scope.
        //
        // TODO: review other statements for possibilities to be applied here

        match stmt {
            AstNode::FunctionDefinition { .. } => Ok(()),

            _ => Err(Error::UnexpectedStatementInGlobal {
                span: stmt.span().clone(),
            }),
        }
    }

    fn body(&self) -> &[AstNode] {
        if let AstNode::Program { body } = self.program {
            body
        } else {
            unreachable!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};
    use expression::ExpressionChecker;
    use indoc::indoc;

    fn check_and_assert_is_ok(input: &str) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let result = ProgramChecker::new(&ast).check();

        assert!(result.is_ok());
    }

    fn check_and_assert_is_err(input: &str) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let result = ProgramChecker::new(&ast).check();

        assert!(result.is_err());
    }

    //
    // Test variable declarations
    //

    #[test]
    fn test_check_variable_declaration_with_type_annotation_and_initial_value() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x: Int = 5;
                end
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_initial_value_only() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x = 5;
                end
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_float_literal() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x = -0.5;
                end
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_boolean_literal() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x = true;
                end
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_function_object_as_value() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x: () -> Int = produce;

                    debug x();
                end

                fn produce(): Int do
                    return 5;
                end
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_void_type() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: Void = 5;
                end
            "});
    }

    #[test]
    fn test_using_incompatible_types_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: Int = 5.0;
                end
            "})
    }

    #[test]
    fn test_using_non_existing_type_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: NonExistingType = 10;
                end
            "})
    }

    #[test]
    fn test_redeclaring_variable_in_the_same_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x = 5;
                    var x = 10;
                end
            "})
    }

    //
    // Test value assignments
    //

    #[test]
    fn test_check_value_assignments() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x = 0;
                    x = 10;

                    var y: Float = 0.0;
                    y = 5.0;

                    var z = false;
                    z = true;
                end
            "})
    }

    #[test]
    fn test_check_shorthand_value_assignments() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var i = 0;
                    i += 1;
                    i -= 2;
                    i *= 3;
                    i /= 4;
                    i %= 5;
                end
            "})
    }

    #[test]
    fn test_check_mod_assign_with_float_types() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var i = 5.0;
                    i %= 2.5;
                end
            "})
    }

    #[test]
    fn test_assigning_value_with_non_existing_variable() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: Float = 5.0;
                    x = y;
                end
            "})
    }

    #[test]
    fn test_using_math_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    1 + 1 = 5;
                end
            "})
    }

    #[test]
    fn test_using_boolean_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    true || false = false;
                end
            "})
    }

    #[test]
    fn test_using_integer_grouped_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    (50) = true;
                end
            "})
    }

    #[test]
    fn test_using_boolean_type_in_shorthand_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    true += true;
                end
            "})
    }

    //
    // Test conditional branches
    //

    #[test]
    fn test_check_if_else_statements() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var condition1 = 5 < 10;
                    var condition2 = 0.5 < 0.75;

                    if condition1 do
                        debug condition1;
                        debug 1;
                    else if condition2 do
                        debug condition2;
                        debug 2;
                    else do
                        debug 0;
                    end
                end
            "})
    }

    #[test]
    fn test_check_nested_if_statements() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    if 1 + 1 == 2 do
                        if 2 + 2 == 4 do
                            if 3 + 3 == 6 do
                                debug true;
                            end
                        end
                    end
                end
            "})
    }

    #[test]
    fn test_using_variable_after_out_of_conditional_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    if true do
                        var x = 50;
                        debug x;
                    end

                    debug x;
                end
            "})
    }

    #[test]
    fn test_using_math_expression_as_condition_in_if_statement() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    if 1 + 1 do
                        debug 1;
                    end
                end
            "})
    }

    #[test]
    fn test_using_variable_declared_in_sibling_conditional_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    if true do
                        var x = 50;
                    else do
                        debug x;
                    end
                end
            "})
    }

    //
    // Test loops
    //

    #[test]
    fn test_check_loop_statements() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    while 2 > 5 do
                        debug 1;
                    end

                    var a = 5;
                    while true do
                        if a == 5 do
                            break;
                        end
                        debug 0;
                    end
                end
            "})
    }

    #[test]
    fn test_using_math_expression_as_condition_in_while_statement() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    while 5 + 5 do end
                end
            "})
    }

    #[test]
    fn test_using_break_statement_not_in_loop_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    if true do
                        break;
                    end
                end
            "})
    }

    #[test]
    fn test_using_invalid_statement_after_loop_control() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    while true do
                        break;
                        1 + true; # this should be error
                    end
                end
            "})
    }

    //
    // Test function definitions
    //

    #[test]
    fn test_defining_function_without_parameter_or_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn add() do end
            "});
    }

    #[test]
    fn test_defining_duplicated_function() {
        check_and_assert_is_err(indoc! {"
                fn print_sum_of(a: Int, b: Int,) do
                    debug a + b;
                end

                fn print_sum_of(a: Float, b: Float) do
                    debug a + b;
                end
            "});
    }

    #[test]
    fn test_defining_functions_both_with_parameters_and_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn sum(x: Int, y: Int): Int do
                    return x + y;
                end
            "});
    }

    #[test]
    fn test_recursive_fibonacci_function() {
        check_and_assert_is_ok(indoc! {"
                fn fibonacci(n: Int): Int do
                    if n == 0 do
                        return 0;
                    else if n == 1 || n == 2 do
                        return 1;
                    end
                    return fibonacci(n-1) + fibonacci(n-2);
                end
            "});
    }

    #[test]
    fn test_recursive_functions_with_void_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn count_to_zero(n: Int) do
                    debug n;
                    if n == 0 do
                        return;
                    end
                    count_to_zero(n-1);
                end

                fn main() do
                    count_to_zero(10);
                end
            "});
    }

    #[test]
    fn test_function_returning_branches() {
        check_and_assert_is_ok(indoc! {"
                fn first(): Int do
                    return 5;
                end

                fn second(): Int do
                    if false do
                        return 0;
                    else do
                        return 1;
                    end
                end

                fn third(): Int do
                    if false do
                        return 0;
                    end
                    return 1;
                end

                fn fourth(): Int do
                    while false do
                        return 0;
                    end
                    return 1;
                end

                fn fifth(): Int do
                    return 1;

                    if false do
                        return 0;
                    end
                end
            "});
    }

    #[test]
    fn test_defining_functions_not_in_order() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    call_foo();
                end

                fn call_foo() do
                    call_bar();
                end

                fn call_bar() do
                    debug true;
                end
            "})
    }

    #[test]
    fn test_aliasing_function_identifier() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var aliased = return_two;

                    debug aliased();
                end

                fn return_two(): Int do
                    return 2;
                end
            "});
    }

    #[test]
    fn test_function_type_as_function_parameter() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    debug get_num(produce);
                end

                fn get_num(producer: () -> Int): Int do
                    return producer() + 5;
                end

                fn produce(): Int do
                    return 10;
                end
            "});
    }

    #[test]
    fn test_calling_function_returned_from_another_function_call() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    debug foo()();
                end

                fn foo(): () -> Int do
                    return bar;
                end

                fn bar(): Int do
                    return 25;
                end
            "});
    }

    #[test]
    fn test_defining_function_not_in_global_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    if true do
                        fn foo() do end
                    end
                end
            "});
    }

    #[test]
    fn test_defining_function_with_an_invalid_return_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(): NonExistingType do end
            "});
    }

    #[test]
    fn test_defining_function_with_duplicated_parameter_name() {
        check_and_assert_is_err(indoc! {"
                fn add_sum_of(x: Int, x: Int) do end
            "});
    }

    #[test]
    fn test_defining_function_with_an_invalid_parameter_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(x: NonExistingType) do end
            "});
    }

    #[test]
    fn test_defining_function_with_void_parameter_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(x: Void) do end
            "});
    }

    #[test]
    fn test_returning_value_from_function_with_void_return_type() {
        check_and_assert_is_err(indoc! {"
                fn foo() do
                    return 5;
                end
            "});
    }

    #[test]
    fn test_returning_value_from_function_with_mismatched_return_type() {
        check_and_assert_is_err(indoc! {"
                fn sum(x: Int, y: Int): Int do
                    return 5.0;
                end
            "});
    }

    #[test]
    fn test_using_invalid_statement_after_return() {
        check_and_assert_is_err(indoc! {"
                fn get_five(): Int do
                    return 5;
                    1 + true; # should be error
                end
            "});
    }

    #[test]
    fn test_returning_non_existing_variable() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int do
                    return not_exist;
                end
            "});
    }

    #[test]
    fn test_defining_function_with_missing_return_in_other_branches() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int do
                    if false do
                        return 5;
                    end
                end
            "});
    }

    #[test]
    fn test_defining_function_with_missing_return_in_else_or_outer_branches() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int do
                    if false do
                        return 0;
                    else if !true do
                        return 0;
                    end
                end
            "});
    }

    #[test]
    fn test_defining_function_with_missing_return_in_outer_branch_of_while_statement() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int do
                    while false do
                        return 0;
                    end
                end
            "});
    }

    //
    // Test debug statement
    //

    #[test]
    fn test_debug_expression() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    debug 17 * 5;
                end
            "});
    }

    #[test]
    fn test_debug_expression_that_returns_void() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    debug this_is_void();
                end

                fn this_is_void() do
                end
            "});
    }

    //
    // Test expressions
    //

    #[test]
    fn test_check_expressions_returned_types() {
        let cases = [
            ("-5 + 50 * 200 / 7 - 999;", Type::new("Int")),
            ("-5 + -0.25;", Type::new("Float")),
            ("99.9 % 0.1;", Type::new("Float")),
            ("767 >= 900 == (45 < 67);", Type::new("Bool")),
            ("false || !false && 50 > 0;", Type::new("Bool")),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let scopes = ScopeStack::default();
            let result = if let AstNode::Program { body } = &ast {
                ExpressionChecker::new(&scopes, &body[0]).check()
            } else {
                unreachable!();
            };

            assert!(result.is_ok());
            assert_eq!(result.unwrap(), expected);
        }
    }

    #[test]
    fn test_invalid_expression() {
        let cases = [
            "100 - not_exist;",
            "-(print);",
            "-true;",
            "true > false;",
            "93 != 93.0;",
            "!5;",
            "false || !false && 50;",
        ];

        for input in cases {
            check_and_assert_is_err(&format!("fn main() do {input} end"));
        }
    }
}
