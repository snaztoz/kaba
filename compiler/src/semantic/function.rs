use super::{
    body::BodyAnalyzer,
    error::{Error, Result},
    state::{scope::ScopeVariant, SharedState},
    tn::TypeNotationAnalyzer,
    types::Type,
};
use crate::ast::{AstNode, FunctionParam};
use logos::Span;

/// Analyzer for function declarations.
///
/// This analyzer assumes that the data from function declarations (i.e.
/// function signature informations) are already stored in the [`SharedState`].
pub struct FunctionDeclarationAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> FunctionDeclarationAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl FunctionDeclarationAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        self.analyze_params_tn()?;
        self.analyze_return_tn()?;

        let params_t = self.params_t();
        let return_t = Box::new(self.return_t());

        let fn_t = Type::Callable { params_t, return_t };
        self.save_fn_t_to_stack(fn_t.clone())?;

        Ok(fn_t)
    }

    fn analyze_params_tn(&self) -> Result<()> {
        for FunctionParam { tn, .. } in self.params() {
            TypeNotationAnalyzer::new(tn, self.state).analyze()?;
        }

        Ok(())
    }

    fn analyze_return_tn(&self) -> Result<()> {
        if let Some(tn) = self.return_tn() {
            TypeNotationAnalyzer::new(tn, self.state)
                .allow_void()
                .analyze()?;
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
        self.return_tn().map_or(Type::void(), Type::from)
    }

    // Save function information to the ScopeStack.
    fn save_fn_t_to_stack(&self, fn_t: Type) -> Result<()> {
        let (id, id_span) = self.id().unwrap_identifier();
        self.state
            .save_sym_or_else(&id, fn_t.clone(), || Error::SymbolAlreadyExist {
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

/// Analyzer for function definition.
///
/// This analyzer assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the ScopeStack.
pub struct FunctionDefinitionAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> FunctionDefinitionAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl FunctionDefinitionAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let return_t = self.fn_t().unwrap_callable().1;
        self.state.with_scope(
            ScopeVariant::Function {
                return_t: return_t.clone(),
            },
            || {
                self.save_params_to_stack(&self.params())?;

                // Analyze function body
                //
                // We do this last in order to accommodate features such as
                // recursive function call.

                let body_t = BodyAnalyzer::new(self.node, self.state).analyze()?;

                if !return_t.is_void() && body_t.is_void() {
                    return Err(Error::ReturnTypeMismatch {
                        expected: return_t,
                        get: Type::void(),
                        span: self.id().span().clone(),
                    });
                }

                Ok(())
            },
        )?;

        Ok(Type::void())
    }

    fn save_params_to_stack(&self, params: &[((String, Span), Type)]) -> Result<()> {
        for ((id, id_span), t) in params {
            self.state
                .save_sym_or_else(id, t.clone(), || Error::SymbolAlreadyExist {
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
            self.state.get_sym_t(id_str).unwrap()
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
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn defining_function_without_parameter_or_return_type() {
        assert_is_ok(indoc! {"
                fn add() do end
            "});
    }

    #[test]
    fn defining_duplicated_functions() {
        assert_is_err(indoc! {"
                fn print_sum_of(a: int, b: int,) do
                    debug a + b;
                end

                fn print_sum_of(a: float, b: float) do
                    debug a + b;
                end
            "});
    }

    #[test]
    fn defining_functions_both_with_parameters_and_return_type() {
        assert_is_ok(indoc! {"
                fn sum(x: int, y: int): int do
                    return x + y;
                end
            "});
    }

    #[test]
    fn recursive_fibonacci_function() {
        assert_is_ok(indoc! {"
                fn fibonacci(n: int): int do
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
        assert_is_ok(indoc! {"
                fn count_to_zero(n: int) do
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
        assert_is_ok(indoc! {"
                fn first(): int do
                    return 5;
                end

                fn second(): int do
                    if false do
                        return 0;
                    else do
                        return 1;
                    end
                end

                fn third(): int do
                    if false do
                        return 0;
                    end
                    return 1;
                end

                fn fourth(): int do
                    while false do
                        return 0;
                    end
                    return 1;
                end

                fn fifth(): int do
                    return 1;

                    if false do
                        return 0;
                    end
                end
            "});
    }

    #[test]
    fn defining_functions_not_in_order() {
        assert_is_ok(indoc! {"
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
        assert_is_ok(indoc! {"
                fn main() do
                    var aliased = return_two;

                    debug aliased();
                end

                fn return_two(): int do
                    return 2;
                end
            "});
    }

    #[test]
    fn using_function_type_as_function_parameter() {
        assert_is_ok(indoc! {"
                fn main() do
                    debug get_num(produce);
                end

                fn get_num(producer: () -> int): int do
                    return producer() + 5;
                end

                fn produce(): int do
                    return 10;
                end
            "});
    }

    #[test]
    fn calling_function_returned_by_another_function_call() {
        assert_is_ok(indoc! {"
                fn main() do
                    debug foo()();
                end

                fn foo(): () -> int do
                    return bar;
                end

                fn bar(): int do
                    return 25;
                end
            "});
    }

    #[test]
    fn calling_function_with_array_parameter() {
        assert_is_ok(indoc! {"
                fn main() do
                    foo([1, 2, 3]);
                end

                fn foo(arr: []int) do
                end
            "});
    }

    #[test]
    fn calling_function_with_array_parameter_using_different_array_sizes() {
        assert_is_ok(indoc! {"
                fn main() do
                    foo([1, 2, 3]);

                    foo([]);

                    foo([1,]);
                end

                fn foo(arr: []int) do
                end
            "});
    }

    #[test]
    fn returning_array_from_a_function() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr_1: []int = foo();
                    var arr_2: []int = foo();

                    arr_1[0] = 10;
                end

                fn foo(): []int do
                    return [1, 2, 3];
                end
            "});
    }

    #[test]
    fn defining_function_not_in_global_scope() {
        assert_is_err(indoc! {"
                fn main() do
                    if true do
                        fn foo() do end
                    end
                end
            "});
    }

    #[test]
    fn defining_function_with_a_non_existing_return_type() {
        assert_is_err(indoc! {"
                fn foo(): NonExistingType do end
            "});
    }

    #[test]
    fn defining_function_with_duplicated_parameter_name() {
        assert_is_err(indoc! {"
                fn add_sum_of(x: int, x: int) do end
            "});
    }

    #[test]
    fn defining_function_with_a_non_existing_parameter_type() {
        assert_is_err(indoc! {"
                fn foo(x: NonExistingType) do end
            "});
    }

    #[test]
    fn defining_function_with_void_parameter_type() {
        assert_is_err(indoc! {"
                fn foo(x: Void) do end
            "});
    }

    #[test]
    fn returning_value_from_function_with_void_return_type() {
        assert_is_err(indoc! {"
                fn foo() do
                    return 5;
                end
            "});
    }

    #[test]
    fn returning_value_from_function_with_mismatched_return_type() {
        assert_is_err(indoc! {"
                fn sum(x: int, y: int): int do
                    return 5.0;
                end
            "});
    }

    #[test]
    fn invalid_statement_after_return() {
        assert_is_err(indoc! {"
                fn get_five(): int do
                    return 5;
                    1 + true; # should be error
                end
            "});
    }

    #[test]
    fn returning_non_existing_variable() {
        assert_is_err(indoc! {"
                fn foo(): int do
                    return not_exist;
                end
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_other_branches() {
        assert_is_err(indoc! {"
                fn foo(): int do
                    if false do
                        return 5;
                    end
                end
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_else_branch_or_outer_scope() {
        assert_is_err(indoc! {"
                fn foo(): int do
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
        assert_is_err(indoc! {"
                fn foo(): int do
                    while false do
                        return 0;
                    end
                end
            "});
    }
}
