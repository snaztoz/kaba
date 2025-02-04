use super::{
    body::BodyAnalyzer,
    error::{Result, SemanticError, SemanticErrorVariant},
    state::{AnalyzerState, ScopeVariant},
    tn::TypeNotationAnalyzer,
    types::Type,
};
use crate::ast::{AstNode, FunctionParam, SymbolId};
use logos::Span;

/// Analyzer for function declarations.
///
/// This analyzer assumes that the data from function declarations (i.e.
/// function signature informations) are already stored in the [`AnalyzerState`].
pub struct FunctionDeclarationAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> FunctionDeclarationAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
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
        self.save_fn_t(fn_t.clone())?;

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
        self.return_tn().map_or(Type::Void, Type::from)
    }

    // Save function information to the scope.
    fn save_fn_t(&self, fn_t: Type) -> Result<()> {
        let (sym, sym_span) = self.sym().unwrap_symbol();
        self.state
            .save_entity_or_else(self.sym_id(), &sym, fn_t.clone(), || SemanticError {
                variant: SemanticErrorVariant::SymbolAlreadyExist(sym.clone()),
                span: sym_span,
            })
    }

    fn sym(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { sym, .. } = self.node {
            sym
        } else {
            unreachable!()
        }
    }

    fn sym_id(&self) -> SymbolId {
        if let AstNode::FunctionDefinition { sym_id, .. } = self.node {
            *sym_id
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
/// This analyzer assumes that the data from function declarations (i.e.
/// function signature informations) are already stored in the scope table.
pub struct FunctionDefinitionAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a AnalyzerState,
}

impl<'a> FunctionDefinitionAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a AnalyzerState) -> Self {
        Self { node, state }
    }
}

impl FunctionDefinitionAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let return_t = self.fn_t().unwrap_callable().1;
        self.state.with_scope(
            self.scope_id(),
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

                if return_t != Type::Void && body_t == Type::Void {
                    return Err(SemanticError {
                        variant: SemanticErrorVariant::ReturnTypeMismatch {
                            expected: return_t,
                            get: Type::Void,
                        },
                        span: self.sym().span().clone(),
                    });
                }

                Ok(())
            },
        )?;

        Ok(Type::Void)
    }

    fn save_params_to_stack(&self, params: &[((SymbolId, String, Span), Type)]) -> Result<()> {
        for ((sym_id, sym, sym_span), t) in params {
            self.state
                .save_entity_or_else(*sym_id, sym, t.clone(), || SemanticError {
                    variant: SemanticErrorVariant::SymbolAlreadyExist(sym.clone()),
                    span: sym_span.clone(),
                })?;
        }

        Ok(())
    }

    fn params(&self) -> Vec<((SymbolId, String, Span), Type)> {
        let params_sym = self.params_sym();
        let params_t = self.fn_t().unwrap_callable().0;

        params_sym
            .iter()
            .cloned()
            .zip(params_t.iter().cloned())
            .collect::<Vec<_>>()
    }

    fn sym(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { sym, .. } = self.node {
            sym
        } else {
            unreachable!()
        }
    }

    fn scope_id(&self) -> SymbolId {
        if let AstNode::FunctionDefinition { scope_id, .. } = self.node {
            *scope_id
        } else {
            unreachable!()
        }
    }

    fn fn_t(&self) -> Type {
        if let AstNode::FunctionDefinition { sym, .. } = self.node {
            let sym_string = &sym.unwrap_symbol().0;
            self.state.get_sym_t(sym_string).unwrap().unwrap_entity()
        } else {
            unreachable!()
        }
    }

    fn params_sym(&self) -> Vec<(SymbolId, String, Span)> {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
                .iter()
                .map(|p| {
                    let id = p.sym_id;
                    let (sym, span) = p.sym.unwrap_symbol();
                    (id, sym, span)
                })
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
                def add() {}
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

                def main() {
                    count_to_zero(10);
                }
            "});
    }

    #[test]
    fn returning_from_functions_with_conditional_and_loop_statements() {
        assert_is_ok(indoc! {"
                def first(): int {
                    return 5;
                }

                def second(): int {
                    if false {
                        return 0;
                    } else {
                        return 1;
                    }
                }

                def third(): int {
                    if false {
                        return 0;
                    }
                    return 1;
                }

                def fourth(): int {
                    while false {
                        return 0;
                    }
                    return 1;
                }

                def fifth(): int {
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
                def main() {
                    call_foo();
                }

                def call_foo() {
                    call_bar();
                }

                def call_bar() {
                    debug true;
                }
            "})
    }

    #[test]
    fn prevent_incompatible_int_type_on_function_return_value() {
        assert_is_err(indoc! {"
                def main() {
                    var x: int = foo();
                }

                def foo(): sbyte {
                    return 5;
                }
            "});
    }

    #[test]
    fn prevent_calling_function_with_missing_args() {
        assert_is_err(indoc! {"
                def main() {
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
                def main() {
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
                def main() {
                    var aliased = return_two;

                    debug aliased();
                }

                def return_two(): int {
                    return 2;
                }
            "});
    }

    #[test]
    fn using_function_type_as_function_parameter() {
        assert_is_ok(indoc! {"
                def main() {
                    debug get_num(produce);
                }

                def get_num(producer: () -> int): int {
                    return producer() + 5;
                }

                def produce(): int {
                    return 10;
                }
            "});
    }

    #[test]
    fn calling_function_returned_by_another_function_call() {
        assert_is_ok(indoc! {"
                def main() {
                    debug foo()();
                }

                def foo(): () -> int {
                    return bar;
                }

                def bar(): int {
                    return 25;
                }
            "});
    }

    #[test]
    fn calling_function_with_array_parameter() {
        assert_is_ok(indoc! {"
                def main() {
                    foo([int 1, 2, 3]);
                }

                def foo(arr: []int) {}
            "});
    }

    #[test]
    fn calling_function_with_array_parameter_using_different_array_sizes() {
        assert_is_ok(indoc! {"
                def main() {
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
                def main() {
                    var arr_1: []int = foo();
                    var arr_2: []int = foo();

                    arr_1[0] = 10;
                }

                def foo(): []int {
                    return [int 1, 2, 3];
                }
            "});
    }

    #[test]
    fn defining_function_not_in_global_scope() {
        assert_is_err(indoc! {"
                def main() {
                    if true {
                        def foo() {}
                    }
                }
            "});
    }

    #[test]
    fn defining_function_with_a_non_existing_return_type() {
        assert_is_err(indoc! {"
                def foo(): NonExistingType { }
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
                def foo() {
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
                def get_five(): int {
                    return 5;
                    1 + true; // should be error
                }
            "});
    }

    #[test]
    fn returning_non_existing_variable() {
        assert_is_err(indoc! {"
                def foo(): int {
                    return not_exist;
                }
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_other_branches() {
        assert_is_err(indoc! {"
                def foo(): int {
                    if false {
                        return 5;
                    }
                }
            "});
    }

    #[test]
    fn defining_function_with_missing_return_in_else_branch_or_outer_scope() {
        assert_is_err(indoc! {"
                def foo(): int {
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
                def foo(): int {
                    while false {
                        return 0;
                    }
                }
            "});
    }
}
