// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the implementation for semantic analysis
//! stage of the compiler.

use self::{
    context::Context,
    error::{Error, Result},
    scope::Scope,
    types::{CallableParameters, Type},
};
use crate::ast::{AstNode, Program as ProgramAst};
use logos::Span;
use types::CallableSignature;

mod context;
mod error;
mod scope;
mod types;

/// Provides a quick way to run semantic analysis on a Kaba AST.
pub fn check(ast: &ProgramAst) -> Result<()> {
    let checker = SemanticChecker::default();
    checker.check_program(ast)?;
    Ok(())
}

#[derive(Default)]
struct SemanticChecker {
    ctx: Context,
}

impl SemanticChecker {
    fn check_program(&self, program_ast: &ProgramAst) -> Result<()> {
        // We are expecting that in global scope, statements (currently) are
        // consisted of function definitions only. So other statements are
        // rejected in this scope.
        //
        // TODO: review other statements for possibilities to be applied here

        let mut functions = vec![];

        for stmt in &program_ast.stmts {
            match stmt {
                AstNode::FunctionDefinition { .. } => {
                    let f = self.check_function_declaration(stmt)?;
                    functions.push(f);
                }

                node => {
                    return Err(Error::NotExpectingStatementInGlobal {
                        span: node.span().clone(),
                    });
                }
            }
        }

        for (i, stmt) in program_ast.stmts.iter().enumerate() {
            if let AstNode::FunctionDefinition { id, body, .. } = stmt {
                self.check_function_definition(id, &functions[i], body)?;
            }
        }

        Ok(())
    }

    fn check_body(&self, body: &[AstNode]) -> Result<Option<Type>> {
        // Body may have a type if it contains a "return" statement
        let mut body_t = None;

        for statement in body {
            match statement {
                AstNode::VariableDeclaration { id, tn, val, span } => {
                    self.check_variable_declaration(id, &tn.as_deref(), &val.as_deref(), span)?
                }

                AstNode::If {
                    cond,
                    body,
                    or_else,
                    ..
                } => {
                    let t = self.check_conditional_branch(cond, body, &or_else.as_deref())?;

                    if body_t.is_none() {
                        body_t = t;
                    }
                }

                AstNode::While { cond, body, .. } => {
                    self.check_while(cond, body)?;
                }

                AstNode::Break { span } | AstNode::Continue { span } => {
                    self.check_loop_control(span)?;
                }

                AstNode::FunctionDefinition { id, .. } => {
                    return Err(Error::FunctionDefinitionNotInGlobal {
                        span: id.span().clone(),
                    })
                }

                AstNode::Return { expr, span } => {
                    let t = self.check_return(expr, span)?;
                    body_t = Some(t);
                }

                AstNode::Debug { expr, .. } => {
                    self.check_debug(expr)?;
                }

                expr => {
                    self.check_expression(expr)?;
                }
            }
        }

        Ok(body_t)
    }

    fn check_variable_declaration(
        &self,
        id: &AstNode,
        tn: &Option<&AstNode>,
        val: &Option<&AstNode>,
        span: &Span,
    ) -> Result<()> {
        let (id, _) = id.unwrap_identifier();
        let var_t = tn.map(Type::from_type_notation_node).transpose()?;
        let val_t = val
            .map(|expression| self.check_expression(expression))
            .transpose()?;

        // Either variable's or value's type must be present
        if var_t.is_none() && val_t.is_none() {
            return Err(Error::UnableToInferVariableType {
                id,
                span: span.clone(),
            });
        }

        // If both present, check if value's type is compatible with
        // the variable's
        if let (Some(var_t), Some(val_t)) = (&var_t, &val_t) {
            Type::assert_assignable(val_t, var_t, || span.clone())?;
        }

        let t = var_t.or(val_t).unwrap();
        self.ctx
            .save_symbol_or_else(&id, t, || Error::VariableAlreadyExist {
                id: id.clone(),
                span: span.clone(),
            })?;

        Ok(())
    }

    fn check_assignment(&self, lhs: &AstNode, rhs: &AstNode, span: &Span) -> Result<Type> {
        if !lhs.is_assignable() {
            return Err(Error::InvalidAssignmentLhs {
                lhs: lhs.to_string(),
                span: lhs.span().clone(),
            });
        }

        let lhs_t = self.check_expression(lhs)?;
        let rhs_t = self.check_expression(rhs)?;

        Type::assert_assignable(&rhs_t, &lhs_t, || span.clone())?;

        Ok(Type::Void)
    }

    fn check_shorthand_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type> {
        let lhs_t = self.check_expression(lhs)?;
        let rhs_t = self.check_expression(rhs)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        self.check_assignment(lhs, rhs, span)
    }

    fn check_conditional_branch(
        &self,
        cond: &AstNode,
        body: &[AstNode],
        or_else: &Option<&AstNode>,
    ) -> Result<Option<Type>> {
        // Expecting boolean type for the condition

        let cond_t = self.check_expression(cond)?;
        Type::assert_boolean(&cond_t, || cond.span().clone())?;

        // Check all statements inside the body with a new scope

        let return_t = self
            .ctx
            .with_scope(Scope::new_conditional_scope(), || self.check_body(body))?;

        if or_else.is_none() {
            return Ok(None);
        }

        match or_else.unwrap() {
            AstNode::If {
                cond,
                body,
                or_else,
                ..
            } => {
                // All conditional branches must returning a value for this whole
                // statement to be considered as returning value

                let branch_return_t =
                    self.check_conditional_branch(cond, body, &or_else.as_deref())?;

                if return_t.is_some() && branch_return_t.is_some() {
                    Ok(return_t)
                } else {
                    Ok(None)
                }
            }

            AstNode::Else { body, .. } => {
                // Check all statements inside the body with a new scope

                let branch_return_t = self
                    .ctx
                    .with_scope(Scope::new_conditional_scope(), || self.check_body(body))?;

                if return_t.is_some() && branch_return_t.is_some() {
                    Ok(return_t)
                } else {
                    Ok(None)
                }
            }

            _ => unreachable!(),
        }
    }

    fn check_while(&self, condition: &AstNode, body: &[AstNode]) -> Result<Option<Type>> {
        // Expecting boolean type for the condition

        let cond_t = self.check_expression(condition)?;
        Type::assert_boolean(&cond_t, || condition.span().clone())?;

        // Check all statements inside the body with a new scope

        self.ctx
            .with_scope(Scope::new_loop_scope(), || self.check_body(body))
    }

    fn check_loop_control(&self, span: &Span) -> Result<()> {
        if self.ctx.is_inside_loop() {
            Ok(())
        } else {
            Err(Error::LoopControlNotInLoopScope { span: span.clone() })
        }
    }

    fn check_function_declaration(&self, node: &AstNode) -> Result<CallableSignature> {
        if let AstNode::FunctionDefinition {
            id,
            params,
            return_t,
            ..
        } = node
        {
            let params = CallableParameters::from_ast_node_pairs(params)?;
            let return_t = return_t
                .as_ref()
                .map_or(Ok(Type::Void), |tn| Type::from_type_notation_node(tn))?;

            let (id, id_span) = id.unwrap_identifier();
            self.ctx.save_symbol_or_else(
                &id,
                Type::Callable {
                    params: params.clone(),
                    return_t: Box::new(return_t.clone()),
                },
                || Error::FunctionAlreadyExist {
                    id: id.clone(),
                    span: id_span.clone(),
                },
            )?;

            return Ok((params, return_t));
        }

        unreachable!()
    }

    fn check_function_definition(
        &self,
        id: &AstNode,
        signature: &CallableSignature,
        body: &[AstNode],
    ) -> Result<()> {
        let (params, return_t) = signature;

        // Entering new scope
        self.ctx
            .with_scope(Scope::new_function_scope(return_t.clone()), || {
                for (i, t) in params.pairs() {
                    self.ctx
                        .save_symbol_or_else(&i, t.clone(), || unreachable!())
                        .unwrap();
                }

                // Check function body
                //
                // We do this last in order to accommodate features such as
                // recursive function call.

                let body_t = self.check_body(body)?;

                if !return_t.is_void() && body_t.is_none() {
                    return Err(Error::FunctionNotReturningValue {
                        expect: return_t.clone(),
                        span: id.span().clone(),
                    });
                }

                Ok(())
            })
    }

    fn check_return(&self, expr: &Option<Box<AstNode>>, span: &Span) -> Result<Type> {
        let expr_t = expr
            .as_ref()
            .map(|expr| self.check_expression(expr).unwrap())
            .unwrap_or(Type::Void);

        let return_t = self
            .ctx
            .get_current_function_return_type()
            .ok_or_else(|| Error::ReturnNotInFunctionScope { span: span.clone() })?;

        Type::assert_assignable(&expr_t, &return_t, || span.clone())
            .map_err(|err| Error::ReturnTypeMismatch {
                expect: return_t.clone(),
                get: expr_t,
                span: err.span().clone(),
            })
            .map(|_| return_t)
    }

    fn check_debug(&self, expr: &AstNode) -> Result<()> {
        self.check_expression(expr)?;
        Ok(())
    }

    fn check_expression(&self, expr: &AstNode) -> Result<Type> {
        match expr {
            AstNode::Assign { lhs, rhs, span } => self.check_assignment(lhs, rhs, span),

            AstNode::AddAssign { lhs, rhs, span }
            | AstNode::SubAssign { lhs, rhs, span }
            | AstNode::MulAssign { lhs, rhs, span }
            | AstNode::DivAssign { lhs, rhs, span }
            | AstNode::ModAssign { lhs, rhs, span } => {
                self.check_shorthand_assignment(lhs, rhs, span)
            }

            AstNode::Eq { lhs, rhs, .. } | AstNode::Neq { lhs, rhs, .. } => {
                self.check_equality_operation(lhs, rhs)
            }

            AstNode::Or { lhs, rhs, .. } | AstNode::And { lhs, rhs, .. } => {
                self.check_logical_and_or_operation(lhs, rhs)
            }

            AstNode::Gt { lhs, rhs, .. }
            | AstNode::Gte { lhs, rhs, .. }
            | AstNode::Lt { lhs, rhs, .. }
            | AstNode::Lte { lhs, rhs, .. } => self.check_comparison_operation(lhs, rhs),

            AstNode::Add { lhs, rhs, .. }
            | AstNode::Sub { lhs, rhs, .. }
            | AstNode::Mul { lhs, rhs, .. }
            | AstNode::Div { lhs, rhs, .. }
            | AstNode::Mod { lhs, rhs, .. } => self.check_math_binary_operation(lhs, rhs),

            AstNode::Not { child, .. } => self.check_logical_not_operation(child),
            AstNode::Neg { child, .. } => self.check_neg_operation(child),

            AstNode::Identifier { name, span } => {
                self.ctx
                    .get_symbol_type(name)
                    .ok_or_else(|| Error::VariableNotExist {
                        id: String::from(name),
                        span: span.clone(),
                    })
            }

            AstNode::Literal { value, .. } => Type::from_value(value),

            AstNode::FunctionCall { callee, args, span } => {
                self.check_function_call(callee, args, span)
            }

            _ => unreachable!(),
        }
    }

    fn check_logical_and_or_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check_expression(lhs)?;
        let rhs_t = self.check_expression(rhs)?;

        Type::assert_boolean(&lhs_t, || lhs.span().clone())?;
        Type::assert_boolean(&rhs_t, || rhs.span().clone())?;

        Ok(Type::Bool)
    }

    fn check_equality_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check_expression(lhs)?;
        let rhs_t = self.check_expression(rhs)?;

        Type::assert_comparable(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Ok(Type::Bool)
    }

    fn check_comparison_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check_expression(lhs)?;
        let rhs_t = self.check_expression(rhs)?;

        Type::assert_comparable(&lhs_t, &rhs_t, || lhs.span().start..rhs.span().end)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        Ok(Type::Bool)
    }

    fn check_math_binary_operation(&self, lhs: &AstNode, rhs: &AstNode) -> Result<Type> {
        let lhs_t = self.check_expression(lhs)?;
        let rhs_t = self.check_expression(rhs)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        if lhs_t == Type::Int && rhs_t == Type::Int {
            Ok(Type::Int)
        } else {
            Ok(Type::Float)
        }
    }

    fn check_logical_not_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = self.check_expression(child)?;
        Type::assert_boolean(&child_t, || child.span().clone())?;
        Ok(Type::Bool)
    }

    fn check_neg_operation(&self, child: &AstNode) -> Result<Type> {
        let child_t = self.check_expression(child)?;
        Type::assert_number(&child_t, || child.span().clone())?;
        Ok(child_t)
    }

    fn check_function_call(&self, callee: &AstNode, args: &[AstNode], span: &Span) -> Result<Type> {
        // transform the arguments into their respective type
        let mut args_t = vec![];
        for arg in args {
            args_t.push(self.check_expression(arg)?)
        }

        let t = match callee {
            AstNode::Identifier { name, span } => {
                self.ctx
                    .get_symbol_type(name)
                    .ok_or_else(|| Error::VariableNotExist {
                        id: String::from(name),
                        span: span.clone(),
                    })?
            }

            _ => todo!(),
        };

        if let Type::Callable { params, return_t } = &t {
            if params.types() != args_t {
                return Err(Error::InvalidFunctionCallArgument {
                    args: args_t,
                    span: span.clone(),
                });
            }
            Ok(*return_t.clone())
        } else {
            Err(Error::NotAFunction {
                span: callee.span().clone(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};
    use indoc::indoc;

    fn check_and_assert_is_ok(input: &str) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let checker = SemanticChecker::default();
        let result = checker.check_program(&ast);

        assert!(result.is_ok());
    }

    fn check_and_assert_is_err(input: &str) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let checker = SemanticChecker::default();
        let result = checker.check_program(&ast);

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
    fn test_check_variable_declaration_with_type_annotation_only() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    var x: Int;
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
    fn test_using_incompatible_types_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: Int = 5.0;
                end
            "})
    }

    #[test]
    fn test_using_no_type_annotation_or_initial_value_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x;
                end
            "})
    }

    #[test]
    fn test_using_non_existing_type_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: NonExistingType;
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
                    var x: Int;
                    x = 10;

                    var y: Float;
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
    fn test_assigning_value_with_mismatched_types() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: Int;
                    x = 5.0;
                end
            "})
    }

    #[test]
    fn test_assigning_value_with_non_existing_variable() {
        check_and_assert_is_err(indoc! {"
                fn main() do
                    var x: Float;
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
    fn test_debug_statement() {
        check_and_assert_is_ok(indoc! {"
                fn main() do
                    debug 17 * 5;
                end
            "});
    }

    //
    // Test expressions
    //

    #[test]
    fn test_check_expressions_returned_types() {
        let cases = [
            ("-5 + 50 * 200 / 7 - 999;", Type::Int),
            ("-5 + -0.25;", Type::Float),
            ("99.9 % 0.1;", Type::Float),
            ("767 >= 900 == (45 < 67);", Type::Bool),
            ("false || !false && 50 > 0;", Type::Bool),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let checker = SemanticChecker::default();
            let result = checker.check_expression(&ast.stmts[0]);

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
