use super::{
    body::BodyChecker,
    error::{Error, Result},
    scope::{Scope, ScopeStack},
    tn::TypeNotationChecker,
    types::Type,
};
use crate::ast::{AstNode, FunctionParam};
use logos::Span;

/// Checker for function declarations.
///
/// This checker assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the ScopeStack.
pub struct FunctionDeclarationChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionDeclarationChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionDeclarationChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        self.check_params_tn()?;
        self.check_return_tn()?;

        let params_t = self.params_t();
        let return_t = Box::new(self.return_t());

        let fn_t = Type::Callable { params_t, return_t };
        self.save_fn_t_to_stack(fn_t.clone())?;

        Ok(fn_t)
    }

    fn check_params_tn(&self) -> Result<()> {
        for FunctionParam { tn, .. } in self.params() {
            TypeNotationChecker::new(self.ss, tn).check()?;
        }

        Ok(())
    }

    fn check_return_tn(&self) -> Result<()> {
        if let Some(tn) = self.return_tn() {
            TypeNotationChecker::new_with_void_allowed(self.ss, tn).check()?;
        }

        Ok(())
    }

    fn params_t(&self) -> Vec<Type> {
        let mut params_t = vec![];
        for FunctionParam { tn, .. } in self.params() {
            params_t.push(Type::from(tn));
        }

        params_t
    }

    fn return_t(&self) -> Type {
        self.return_tn().map_or(Type::new("Void"), Type::from)
    }

    // Save function information to the ScopeStack.
    fn save_fn_t_to_stack(&self, fn_t: Type) -> Result<()> {
        let (id, id_span) = self.id().unwrap_identifier();
        self.ss
            .save_symbol_or_else(&id, fn_t.clone(), || Error::SymbolAlreadyExist {
                id: id.clone(),
                span: id_span,
            })
    }

    fn id(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn params(&self) -> &[FunctionParam] {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
        } else {
            unreachable!()
        }
    }

    fn return_tn(&self) -> Option<&AstNode> {
        if let AstNode::FunctionDefinition { return_tn, .. } = self.node {
            return_tn.as_deref()
        } else {
            unreachable!()
        }
    }
}

/// Checker for function definition.
///
/// This checker assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the ScopeStack.
pub struct FunctionDefinitionChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionDefinitionChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionDefinitionChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let return_t = self.fn_t().unwrap_callable().1;
        self.ss
            .with_scope(Scope::new_function_scope(return_t.clone()), || {
                self.save_params_to_stack(&self.params())?;

                // Check function body
                //
                // We do this last in order to accommodate features such as
                // recursive function call.

                let body_t = BodyChecker::new(self.ss, self.node).check()?;

                if !return_t.is_void() && body_t.is_void() {
                    return Err(Error::ReturnTypeMismatch {
                        expected: return_t,
                        get: Type::new("Void"),
                        span: self.id().span().clone(),
                    });
                }

                Ok(())
            })?;

        Ok(Type::new("Void"))
    }

    fn save_params_to_stack(&self, params: &[((String, Span), Type)]) -> Result<()> {
        for ((id, id_span), t) in params {
            self.ss
                .save_symbol_or_else(id, t.clone(), || Error::SymbolAlreadyExist {
                    id: id.clone(),
                    span: id_span.clone(),
                })?;
        }

        Ok(())
    }

    fn params(&self) -> Vec<((String, Span), Type)> {
        let params_t = self.fn_t().unwrap_callable().0;
        let params_id = self.params_id();

        params_id
            .iter()
            .cloned()
            .zip(params_t.iter().cloned())
            .collect::<Vec<_>>()
    }

    fn id(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn fn_t(&self) -> Type {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            let id_str = &id.unwrap_identifier().0;
            self.ss.get_symbol_type(id_str).unwrap()
        } else {
            unreachable!()
        }
    }

    fn params_id(&self) -> Vec<(String, Span)> {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
                .iter()
                .map(|p| p.id.unwrap_identifier())
                .collect::<Vec<_>>()
        } else {
            unreachable!()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{check_and_assert_is_err, check_and_assert_is_ok};
    use indoc::indoc;

    #[test]
    fn defining_function_without_parameter_or_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn add() do end
            "});
    }

    #[test]
    fn defining_duplicated_functions() {
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
    fn defining_functions_both_with_parameters_and_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn sum(x: Int, y: Int): Int do
                    return x + y;
                end
            "});
    }

    #[test]
    fn recursive_fibonacci_function() {
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
    fn recursive_functions_with_void_return_type() {
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
    fn returning_from_functions_with_conditional_and_loop_statements() {
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
    fn defining_functions_not_in_order() {
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
    fn aliasing_function_identifier() {
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
    fn using_function_type_as_function_parameter() {
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
    fn calling_function_returned_by_another_function_call() {
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
    fn calling_function_with_array_parameter() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    foo([1, 2, 3]);
                end

                fn foo(arr: [3]Int) do
                end
            "});
    }

    #[test]
    fn calling_function_with_auto_array_parameter() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    foo([1, 2, 3]);

                    foo([]);

                    foo([1,]);
                end

                fn foo(arr: [_]Int) do
                end
            "});
    }

    #[test]
    fn defining_function_not_in_global_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    if true do
                        fn foo() do end
                    end
                end
            "});
    }

    #[test]
    fn defining_function_with_a_non_existing_return_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(): NonExistingType do end
            "});
    }

    #[test]
    fn defining_function_with_duplicated_parameter_name() {
        check_and_assert_is_err(indoc! {"
                fn add_sum_of(x: Int, x: Int) do end
            "});
    }

    #[test]
    fn defining_function_with_a_non_existing_parameter_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(x: NonExistingType) do end
            "});
    }

    #[test]
    fn defining_function_with_void_parameter_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(x: Void) do end
            "});
    }

    #[test]
    fn returning_value_from_function_with_void_return_type() {
        check_and_assert_is_err(indoc! {"
                fn foo() do
                    return 5;
                end
            "});
    }

    #[test]
    fn returning_value_from_function_with_mismatched_return_type() {
        check_and_assert_is_err(indoc! {"
                fn sum(x: Int, y: Int): Int do
                    return 5.0;
                end
            "});
    }

    #[test]
    fn invalid_statement_after_return() {
        check_and_assert_is_err(indoc! {"
                fn get_five(): Int do
                    return 5;
                    1 + true; # should be error
                end
            "});
    }

    #[test]
    fn returning_non_existing_variable() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int do
                    return not_exist;
                end
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_other_branches() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int do
                    if false do
                        return 5;
                    end
                end
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_else_branch_or_outer_scope() {
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
    fn defining_function_with_missing_return_in_outer_scope_of_while_statement() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int do
                    while false do
                        return 0;
                    end
                end
            "});
    }
}
