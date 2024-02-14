// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the implementation for semantic analysis
//! stage of the compiler.

use self::{context::Context, scope::Scope, types::Type};
use crate::ast::{AstNode, Program as ProgramAst, Value};
use logos::Span;
use std::{collections::BTreeMap, fmt::Display, str::FromStr};

mod builtin_functions;
mod context;
mod scope;
mod types;

type FunctionParameterList = BTreeMap<String, Type>;
type FunctionReturnType = Type;

/// Provides a quick way to run semantic analysis on a Kaba AST.
pub fn check(ast: &ProgramAst) -> Result<(), SemanticError> {
    let checker = SemanticChecker::default();
    checker.check_program(ast)?;
    Ok(())
}

#[derive(Default)]
struct SemanticChecker {
    context: Context,
}

impl SemanticChecker {
    fn check_program(&self, program_ast: &ProgramAst) -> Result<(), SemanticError> {
        let mut functions = vec![];

        for statement in &program_ast.statements {
            match statement {
                AstNode::FunctionDefinition {
                    name,
                    parameters,
                    return_type,
                    ..
                } => {
                    let function =
                        self.check_function_declaration(name, parameters, return_type)?;
                    functions.push(function);
                }

                node => {
                    return Err(SemanticError::NotExpectingStatementInGlobal {
                        span: node.get_span().clone(),
                    });
                }
            }
        }

        for (i, statement) in program_ast.statements.iter().enumerate() {
            if let AstNode::FunctionDefinition { name, body, .. } = statement {
                let (parameters, return_type) = &functions[i];
                self.check_function_definition(name, parameters, return_type, body)?;
            } else {
                unreachable!();
            }
        }

        Ok(())
    }

    fn check_body(&self, body: &[AstNode]) -> Result<Option<Type>, SemanticError> {
        let mut body_returned_type = None;

        for statement in body {
            match statement {
                AstNode::VariableDeclaration {
                    identifier,
                    var_type,
                    value,
                    span,
                } => self.check_variable_declaration(
                    identifier,
                    &var_type.as_deref(),
                    &value.as_deref(),
                    span,
                )?,

                AstNode::If {
                    condition,
                    body,
                    or_else,
                    ..
                } => {
                    let returned_type =
                        self.check_conditional_branch(condition, body, &or_else.as_deref())?;

                    if body_returned_type.is_none() {
                        body_returned_type = returned_type;
                    }
                }

                AstNode::While {
                    condition, body, ..
                } => {
                    self.check_while(condition, body)?;
                }

                AstNode::Break { span } | AstNode::Continue { span } => {
                    self.check_loop_control(span)?;
                }

                AstNode::FunctionDefinition { name, .. } => {
                    return Err(SemanticError::FunctionDefinitionNotInGlobal {
                        span: name.get_span().clone(),
                    })
                }

                AstNode::Return { expression, span } => {
                    let returned_type = self.check_return(expression, span)?;
                    body_returned_type = Some(returned_type);
                }

                expression => {
                    self.check_expression(expression)?;
                }
            }
        }

        Ok(body_returned_type)
    }

    fn check_variable_declaration(
        &self,
        identifier: &AstNode,
        var_type: &Option<&AstNode>,
        value: &Option<&AstNode>,
        span: &Span,
    ) -> Result<(), SemanticError> {
        let (var_name, _) = identifier.unwrap_identifier();

        // Can't have the same variable be declared multiple times
        // in the same scope
        self.assert_not_exist_in_current_scope(identifier)?;

        let var_type = var_type
            .map(|vt| self.convert_type_notation_to_type(vt))
            .transpose()?;

        let value_type = value
            .map(|expression| self.check_expression(expression))
            .transpose()?;

        // Either variable's or value's type must be present
        if var_type.is_none() && value_type.is_none() {
            return Err(SemanticError::UnableToInferVariableType {
                name: var_name,
                span: span.clone(),
            });
        }

        // If both present, check if value's type is compatible with
        // the variable's
        if let (Some(var_type), Some(value_type)) = (&var_type, &value_type) {
            self.assert_is_assignable(value_type, var_type, span)?;
        }

        // Save variable information
        self.context
            .save_symbol_to_current_scope(var_name, var_type.or(value_type).unwrap());

        Ok(())
    }

    fn check_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.check_expression(lhs)?;
        let rhs_type = self.check_expression(rhs)?;

        self.assert_can_act_as_assignment_lhs(lhs)?;
        self.assert_is_assignable(&rhs_type, &lhs_type, span)?;

        Ok(Type::Void)
    }

    fn check_shorthand_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.check_expression(lhs)?;
        let rhs_type = self.check_expression(rhs)?;

        self.assert_is_number(&lhs_type, lhs.get_span())?;
        self.assert_is_number(&rhs_type, rhs.get_span())?;

        self.check_assignment(lhs, rhs, span)
    }

    fn check_conditional_branch(
        &self,
        condition: &AstNode,
        body: &[AstNode],
        or_else: &Option<&AstNode>,
    ) -> Result<Option<Type>, SemanticError> {
        // Expecting boolean type for the condition

        let condition_type = self.check_expression(condition)?;
        self.assert_is_boolean(&condition_type, condition.get_span())?;

        // Check all statements inside the body with a new scope

        let this_branch_returned_type = self
            .context
            .with_scope(Scope::new_conditional_scope(), || self.check_body(body))?;

        if or_else.is_none() {
            return Ok(None);
        }

        match or_else.unwrap() {
            AstNode::If {
                condition,
                body,
                or_else,
                ..
            } => {
                // All conditional branches must returning a value for this whole
                // statement to be considered as returning value

                let sibling_branch_returned_type =
                    self.check_conditional_branch(condition, body, &or_else.as_deref())?;

                if this_branch_returned_type.is_some() && sibling_branch_returned_type.is_some() {
                    Ok(this_branch_returned_type)
                } else {
                    Ok(None)
                }
            }

            AstNode::Else { body, .. } => {
                // Check all statements inside the body with a new scope

                let else_branch_returned_type = self
                    .context
                    .with_scope(Scope::new_conditional_scope(), || self.check_body(body))?;

                if this_branch_returned_type.is_some() && else_branch_returned_type.is_some() {
                    Ok(this_branch_returned_type)
                } else {
                    Ok(None)
                }
            }

            _ => unreachable!(),
        }
    }

    fn check_while(
        &self,
        condition: &AstNode,
        body: &[AstNode],
    ) -> Result<Option<Type>, SemanticError> {
        // Expecting boolean type for the condition

        let condition_type = self.check_expression(condition)?;
        self.assert_is_boolean(&condition_type, condition.get_span())?;

        // Check all statements inside the body with a new scope

        self.context
            .with_scope(Scope::new_loop_scope(), || self.check_body(body))
    }

    fn check_loop_control(&self, span: &Span) -> Result<(), SemanticError> {
        if self.context.current_scope_is_inside_loop() {
            Ok(())
        } else {
            Err(SemanticError::LoopControlNotInLoopScope { span: span.clone() })
        }
    }

    fn check_function_declaration(
        &self,
        name: &AstNode,
        parameters: &[(AstNode, AstNode)],
        return_type: &Option<Box<AstNode>>,
    ) -> Result<(FunctionParameterList, FunctionReturnType), SemanticError> {
        // Can't use the name if it's already used by another data type.
        //
        // But if it's also a function, then it should be no problem
        // (function overloading).
        self.assert_not_exist_in_current_scope(name)?;

        let mut params = BTreeMap::new();
        for (parameter, parameter_type) in parameters {
            let (name, span) = parameter.unwrap_identifier();
            let parameter_type = self.convert_type_notation_to_type(parameter_type)?;

            // Parameters can't have duplicated name
            if params.contains_key(&name) {
                return Err(SemanticError::VariableAlreadyExist { name, span });
            }

            params.insert(name, parameter_type);
        }

        let parameter_types = params.values().cloned().collect::<Vec<_>>();
        let return_type = return_type
            .as_ref()
            .map_or(Ok(Type::Void), |tn| self.convert_type_notation_to_type(tn))?;

        // Save function information

        let function_type = Type::Callable {
            parameter_types: parameter_types.clone(),
            return_type: Box::new(return_type.clone()),
        };
        let (name, name_span) = name.unwrap_identifier();
        let function_signature = format!("{name}{function_type}");

        if self.context.global_scope_has_symbol(&function_signature) {
            return Err(SemanticError::FunctionVariantAlreadyExist {
                name,
                args: parameter_types,
                span: name_span,
            });
        }

        self.context
            .save_symbol_to_global_scope(function_signature, function_type);

        Ok((params, return_type))
    }

    fn check_function_definition(
        &self,
        name: &AstNode,
        parameters: &FunctionParameterList,
        return_type: &FunctionReturnType,
        body: &[AstNode],
    ) -> Result<(), SemanticError> {
        // Entering new scope
        self.context
            .with_scope(Scope::new_function_scope(return_type.clone()), || {
                for (parameter_name, parameter_type) in parameters {
                    self.context.save_symbol_to_current_scope(
                        parameter_name.to_string(),
                        parameter_type.clone(),
                    );
                }

                // Check function body
                //
                // We do this last in order to accommodate features such as
                // recursive function call.

                let body_returned_type = self.check_body(body)?;

                if !return_type.is_void() && body_returned_type.is_none() {
                    return Err(SemanticError::FunctionNotReturningValue {
                        expecting_type: return_type.clone(),
                        span: name.get_span().clone(),
                    });
                }

                Ok(())
            })
    }

    fn check_return(
        &self,
        expression: &Option<Box<AstNode>>,
        span: &Span,
    ) -> Result<Type, SemanticError> {
        let expression_type = expression
            .as_ref()
            .map(|expr| self.check_expression(expr).unwrap())
            .unwrap_or(Type::Void);

        let return_type = self
            .context
            .get_current_function_return_type()
            .ok_or_else(|| SemanticError::ReturnNotInFunctionScope { span: span.clone() })?;

        self.assert_is_assignable(&expression_type, &return_type, span)
            .map_err(|err| SemanticError::ReturnTypeMismatch {
                expecting: return_type.clone(),
                get: expression_type,
                span: err.get_span().clone(),
            })
            .map(|_| return_type)
    }

    fn check_expression(&self, expression: &AstNode) -> Result<Type, SemanticError> {
        match expression {
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

            AstNode::Identifier { name, span } => self.get_identifier_type(name, span),

            AstNode::Literal { value, .. } => self.get_literal_type(value),

            AstNode::FunctionCall { callee, args, span } => {
                self.check_function_call(callee, args, span)
            }

            _ => unreachable!(),
        }
    }

    fn check_logical_and_or_operation(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.check_expression(lhs)?;
        let rhs_type = self.check_expression(rhs)?;

        self.assert_is_boolean(&lhs_type, lhs.get_span())?;
        self.assert_is_boolean(&rhs_type, rhs.get_span())?;

        Ok(Type::Bool)
    }

    fn check_equality_operation(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.check_expression(lhs)?;
        let rhs_type = self.check_expression(rhs)?;

        let span = lhs.get_span().start..rhs.get_span().end;
        self.assert_is_comparable_between(&lhs_type, &rhs_type, &span)?;

        Ok(Type::Bool)
    }

    fn check_comparison_operation(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.check_expression(lhs)?;
        let rhs_type = self.check_expression(rhs)?;

        let span = lhs.get_span().start..rhs.get_span().end;
        self.assert_is_comparable_between(&lhs_type, &rhs_type, &span)?;

        self.assert_is_number(&lhs_type, lhs.get_span())?;
        self.assert_is_number(&rhs_type, rhs.get_span())?;

        Ok(Type::Bool)
    }

    fn check_math_binary_operation(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.check_expression(lhs)?;
        let rhs_type = self.check_expression(rhs)?;

        self.assert_is_number(&lhs_type, lhs.get_span())?;
        self.assert_is_number(&rhs_type, rhs.get_span())?;

        if lhs_type == Type::Int && rhs_type == Type::Int {
            Ok(Type::Int)
        } else {
            Ok(Type::Float)
        }
    }

    fn check_logical_not_operation(&self, child: &AstNode) -> Result<Type, SemanticError> {
        let child_type = self.check_expression(child)?;
        self.assert_is_boolean(&child_type, child.get_span())?;
        Ok(Type::Bool)
    }

    fn check_neg_operation(&self, child: &AstNode) -> Result<Type, SemanticError> {
        let child_type = self.check_expression(child)?;
        self.assert_is_number(&child_type, child.get_span())?;
        Ok(child_type)
    }

    fn check_function_call(
        &self,
        callee: &AstNode,
        args: &[AstNode],
        span: &Span,
    ) -> Result<Type, SemanticError> {
        // transform the arguments into their respective type
        let mut arg_types = vec![];
        for arg in args {
            arg_types.push(self.check_expression(arg)?)
        }

        let t = match callee {
            AstNode::Identifier { name, span } => {
                let arg_types: Vec<_> = arg_types.iter().map(|arg| arg.to_string()).collect();
                let function_signature = format!("{name}/{}", arg_types.join(","));

                self.get_identifier_type(&function_signature, span)?
            }

            _ => todo!(),
        };

        if let Type::Callable {
            parameter_types,
            return_type,
        } = &t
        {
            if parameter_types != &arg_types {
                return Err(SemanticError::FunctionVariantNotExist {
                    args: arg_types,
                    span: span.clone(),
                });
            }

            Ok(*return_type.clone())
        } else {
            Err(SemanticError::NotAFunction {
                span: callee.get_span().clone(),
            })
        }
    }

    fn get_identifier_type(&self, name: &str, err_span: &Span) -> Result<Type, SemanticError> {
        if self.context.has_symbol(name) {
            Ok(self.context.get_symbol_type(name))
        } else {
            Err(SemanticError::VariableNotExist {
                name: String::from(name),
                span: err_span.clone(),
            })
        }
    }

    fn get_literal_type(&self, literal_value: &Value) -> Result<Type, SemanticError> {
        match literal_value {
            Value::Void => Ok(Type::Void),
            Value::Integer(_) => Ok(Type::Int),
            Value::Float(_) => Ok(Type::Float),
            Value::Boolean(_) => Ok(Type::Bool),
        }
    }

    fn convert_type_notation_to_type(
        &self,
        type_notation: &AstNode,
    ) -> Result<Type, SemanticError> {
        let (type_name, type_span) = type_notation.unwrap_type_notation();
        Type::from_str(&type_name).map_err(|_| SemanticError::TypeNotExist {
            name: type_name,
            span: type_span,
        })
    }

    fn assert_not_exist_in_current_scope(&self, identifier: &AstNode) -> Result<(), SemanticError> {
        let (name, span) = identifier.unwrap_identifier();

        if !self.context.current_scope_has_symbol(&name) {
            Ok(())
        } else {
            Err(SemanticError::VariableAlreadyExist { name, span })
        }
    }

    fn assert_can_act_as_assignment_lhs(&self, node: &AstNode) -> Result<(), SemanticError> {
        if node.can_be_assignment_lhs() {
            Ok(())
        } else {
            Err(SemanticError::InvalidAssignmentLhs {
                lhs: node.to_string(),
                span: node.get_span().clone(),
            })
        }
    }

    fn assert_is_assignable(
        &self,
        from: &Type,
        to: &Type,
        err_span: &Span,
    ) -> Result<(), SemanticError> {
        if from.is_assignable_to(to) {
            Ok(())
        } else {
            Err(SemanticError::UnableToAssignValueType {
                var_type: to.to_string(),
                value_type: from.to_string(),
                span: err_span.clone(),
            })
        }
    }

    fn assert_is_comparable_between(
        &self,
        type_a: &Type,
        type_b: &Type,
        err_span: &Span,
    ) -> Result<(), SemanticError> {
        if type_a == type_b {
            Ok(())
        } else {
            Err(SemanticError::UnableToCompareTypeAWithTypeB {
                type_a: type_a.clone(),
                type_b: type_b.clone(),
                span: err_span.clone(),
            })
        }
    }

    fn assert_is_number(&self, t: &Type, err_span: &Span) -> Result<(), SemanticError> {
        if t.is_number() {
            Ok(())
        } else {
            Err(SemanticError::NotANumber {
                span: err_span.clone(),
            })
        }
    }

    fn assert_is_boolean(&self, t: &Type, err_span: &Span) -> Result<(), SemanticError> {
        if t.is_boolean() {
            Ok(())
        } else {
            Err(SemanticError::NotABoolean {
                span: err_span.clone(),
            })
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SemanticError {
    UnableToInferVariableType {
        name: String,
        span: Span,
    },

    UnableToAssignValueType {
        var_type: String,
        value_type: String,
        span: Span,
    },

    InvalidAssignmentLhs {
        lhs: String,
        span: Span,
    },

    UnableToCompareTypeAWithTypeB {
        type_a: Type,
        type_b: Type,
        span: Span,
    },

    VariableAlreadyExist {
        name: String,
        span: Span,
    },

    VariableNotExist {
        name: String,
        span: Span,
    },

    TypeNotExist {
        name: String,
        span: Span,
    },

    NotANumber {
        span: Span,
    },

    NotABoolean {
        span: Span,
    },

    NotAFunction {
        span: Span,
    },

    NotExpectingStatementInGlobal {
        span: Span,
    },

    FunctionDefinitionNotInGlobal {
        span: Span,
    },

    FunctionVariantAlreadyExist {
        name: String,
        args: Vec<Type>,
        span: Span,
    },

    FunctionVariantNotExist {
        args: Vec<Type>,
        span: Span,
    },

    FunctionNotReturningValue {
        expecting_type: Type,
        span: Span,
    },

    ReturnNotInFunctionScope {
        span: Span,
    },

    ReturnTypeMismatch {
        expecting: Type,
        get: Type,
        span: Span,
    },

    LoopControlNotInLoopScope {
        span: Span,
    },
}

impl SemanticError {
    pub fn get_span(&self) -> &Span {
        match self {
            Self::UnableToInferVariableType { span, .. }
            | Self::UnableToAssignValueType { span, .. }
            | Self::InvalidAssignmentLhs { span, .. }
            | Self::UnableToCompareTypeAWithTypeB { span, .. }
            | Self::VariableNotExist { span, .. }
            | Self::VariableAlreadyExist { span, .. }
            | Self::TypeNotExist { span, .. }
            | Self::NotANumber { span, .. }
            | Self::NotABoolean { span, .. }
            | Self::NotAFunction { span, .. }
            | Self::NotExpectingStatementInGlobal { span }
            | Self::FunctionDefinitionNotInGlobal { span, .. }
            | Self::FunctionVariantAlreadyExist { span, .. }
            | Self::FunctionVariantNotExist { span, .. }
            | Self::FunctionNotReturningValue { span, .. }
            | Self::ReturnNotInFunctionScope { span, .. }
            | Self::ReturnTypeMismatch { span, .. }
            | Self::LoopControlNotInLoopScope { span } => span,
        }
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnableToInferVariableType { name, .. } => {
                write!(f, "unable to infer the type of variable `{name}` because of no type or initial value were provided")
            }
            Self::UnableToAssignValueType {
                var_type,
                value_type,
                ..
            } => {
                write!(
                    f,
                    "unable to assign value of type `{value_type}` to type `{var_type}`"
                )
            }
            Self::InvalidAssignmentLhs { lhs, .. } => {
                write!(f, "{lhs} can not be an assignment's lhs")
            }
            Self::UnableToCompareTypeAWithTypeB { type_a, type_b, .. } => {
                write!(
                    f,
                    "unable to compare the value of type `{type_a}` with type `{type_b}`"
                )
            }
            Self::VariableAlreadyExist { name, .. } => {
                write!(f, "variable `{name}` already exists in current scope")
            }
            Self::VariableNotExist { name, .. } => {
                write!(f, "variable `{name}` is not exist in current scope")
            }
            Self::TypeNotExist { name, .. } => {
                write!(f, "type `{name}` is not exist")
            }
            Self::NotANumber { .. } => {
                write!(f, "not a number")
            }
            Self::NotABoolean { .. } => {
                write!(f, "not a boolean")
            }
            Self::NotAFunction { .. } => {
                write!(f, "not a function")
            }
            Self::NotExpectingStatementInGlobal { .. } => {
                write!(f, "expecting only function definitions in global scope")
            }
            Self::FunctionDefinitionNotInGlobal { .. } => {
                write!(f, "unable to define functions in non-global scope")
            }
            Self::FunctionVariantAlreadyExist { name, args, .. } => {
                write!(
                    f,
                    "function `{name}` with variant of `{:?}` is already exists",
                    args
                )
            }
            Self::FunctionVariantNotExist { args, .. } => {
                write!(
                    f,
                    "unable to call function with argument(s) of type {:?}",
                    args
                )
            }
            Self::FunctionNotReturningValue { expecting_type, .. } => {
                write!(
                    f,
                    "expecting function to return a value of type {expecting_type}, but none was returned",
                )
            }
            Self::ReturnNotInFunctionScope { .. } => {
                write!(
                    f,
                    "the usage of `return` statement must be within a function body",
                )
            }
            Self::ReturnTypeMismatch { expecting, get, .. } => {
                write!(
                    f,
                    "expecting to returning value of type `{expecting}`, but get `{get}` instead",
                )
            }
            Self::LoopControlNotInLoopScope { .. } => {
                write!(
                    f,
                    "the usage of `break` and `continue` statements must be within a loop"
                )
            }
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
                fn main() {
                    var x: Int = 5;
                }
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_type_annotation_only() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var x: Int;
                }
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_initial_value_only() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var x = 5;
                }
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_float_literal() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var x = -0.5;
                }
            "});
    }

    #[test]
    fn test_check_variable_declaration_with_boolean_literal() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var x = true;
                }
            "});
    }

    #[test]
    fn test_using_incompatible_types_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    var x: Int = 5.0;
                }
            "})
    }

    #[test]
    fn test_using_no_type_annotation_or_initial_value_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    var x;
                }
            "})
    }

    #[test]
    fn test_using_non_existing_type_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    var x: NonExistingType;
                }
            "})
    }

    #[test]
    fn test_redeclaring_variable_in_the_same_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    var x = 5;
                    var x = 10;
                }
            "})
    }

    //
    // Test value assignments
    //

    #[test]
    fn test_check_value_assignments() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var x: Int;
                    x = 10;

                    var y: Float;
                    y = 5.0;

                    var z = false;
                    z = true;
                }
            "})
    }

    #[test]
    fn test_check_shorthand_value_assignments() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var i = 0;
                    i += 1;
                    i -= 2;
                    i *= 3;
                    i /= 4;
                    i %= 5;
                }
            "})
    }

    #[test]
    fn test_check_mod_assign_with_float_types() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var i = 5.0;
                    i %= 2.5;
                }
            "})
    }

    #[test]
    fn test_assigning_value_with_mismatched_types() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    var x: Int;
                    x = 5.0;
                }
            "})
    }

    #[test]
    fn test_assigning_value_with_non_existing_variable() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    var x: Float;
                    x = y;
                }
            "})
    }

    #[test]
    fn test_using_math_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    1 + 1 = 5;
                }
            "})
    }

    #[test]
    fn test_using_boolean_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    true || false = false;
                }
            "})
    }

    #[test]
    fn test_using_integer_grouped_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    (50) = true;
                }
            "})
    }

    #[test]
    fn test_using_boolean_type_in_shorthand_value_assignment() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    true += true;
                }
            "})
    }

    //
    // Test conditional branches
    //

    #[test]
    fn test_check_if_else_statements() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    var condition1 = 5 < 10;
                    var condition2 = 0.5 < 0.75;

                    if condition1 {
                        print(condition1);
                        print(1);
                    } else if condition2 {
                        print(condition2);
                        print(2);
                    } else {
                        print(0);
                    }
                }
            "})
    }

    #[test]
    fn test_check_nested_if_statements() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    if 1 + 1 == 2 {
                        if 2 + 2 == 4 {
                            if 3 + 3 == 6 {
                                print(true);
                            }
                        }
                    }
                }
            "})
    }

    #[test]
    fn test_using_variable_after_out_of_conditional_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    if true {
                        var x = 50;
                        print(x);
                    }

                    print(x);
                }
            "})
    }

    #[test]
    fn test_using_math_expression_as_condition_in_if_statement() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    if 1 + 1 {
                        print(1);
                    }
                }
            "})
    }

    #[test]
    fn test_using_variable_declared_in_sibling_conditional_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    if true {
                        var x = 50;
                    } else {
                        print(x);
                    }
                }
            "})
    }

    //
    // Test loops
    //

    #[test]
    fn test_check_loop_statements() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    while 2 > 5 {
                        print(1);
                    }

                    var a = 5;
                    while true {
                        if a == 5 {
                            break;
                        }
                        print(0);
                    }
                }
            "})
    }

    #[test]
    fn test_using_math_expression_as_condition_in_while_statement() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    while 5 + 5 {}
                }
            "})
    }

    #[test]
    fn test_using_break_statement_not_in_loop_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    if true {
                        break;
                    }
                }
            "})
    }

    #[test]
    fn test_using_invalid_statement_after_loop_control() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    while true {
                        break;
                        1 + true; // this should be error
                    }
                }
            "})
    }

    //
    // Test function definitions
    //

    #[test]
    fn test_defining_function_without_parameter_or_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn add() {}
            "});
    }

    #[test]
    fn test_defining_function_overloading() {
        check_and_assert_is_ok(indoc! {"
                fn printSumOf(a: Int, b: Int,) {
                    print(a + b);
                }

                fn printSumOf(a: Float, b: Float) {
                    print(a + b);
                }
            "});
    }

    #[test]
    fn test_defining_functions_both_with_parameters_and_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn sum(x: Int, y: Int): Int {
                    return x + y;
                }
            "});
    }

    #[test]
    fn test_calling_overloaded_functions_with_one_of_its_variants() {
        check_and_assert_is_ok(indoc! {"
                fn sum(x: Int, y: Int): Int {
                    return x + y;
                }

                fn sum(x: Float, y: Float): Float {
                    return x + y;
                }

                fn main() {
                    var result = sum(5, 7);
                    print(result);
                }
            "});
    }

    #[test]
    fn test_recursive_fibonacci_function() {
        check_and_assert_is_ok(indoc! {"
                fn fibonacci(n: Int): Int {
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
    fn test_recursive_functions_with_void_return_type() {
        check_and_assert_is_ok(indoc! {"
                fn countToZero(n: Int) {
                    print(n);
                    if n == 0 {
                        return;
                    }
                    countToZero(n-1);
                }

                fn main() {
                    countToZero(10);
                }
            "});
    }

    #[test]
    fn test_function_returning_branches() {
        check_and_assert_is_ok(indoc! {"
                fn first(): Int {
                    return 5;
                }

                fn second(): Int {
                    if false {
                        return 0;
                    } else {
                        return 1;
                    }
                }

                fn third(): Int {
                    if false {
                        return 0;
                    }
                    return 1;
                }

                fn fourth(): Int {
                    while false {
                        return 0;
                    }
                    return 1;
                }

                fn fifth(): Int {
                    return 1;

                    if false {
                        return 0;
                    }
                }
            "});
    }

    #[test]
    fn test_defining_functions_not_in_order() {
        check_and_assert_is_ok(indoc! {"
                fn main() {
                    callFoo();
                }

                fn callFoo() {
                    callBar();
                }

                fn callBar() {
                    print(true);
                }
            "})
    }

    #[test]
    fn test_defining_function_not_in_global_scope() {
        check_and_assert_is_err(indoc! {"
                fn main() {
                    if true {
                        fn foo() {}
                    }
                }
            "});
    }

    // #[test]
    // fn test_defining_function_with_an_already_taken_name_in_the_same_scope() {
    //     check_and_assert_is_err(indoc! {"
    //             var foo: Int = 0;

    //             fn foo() {}
    //         "});
    // }

    #[test]
    fn test_defining_function_with_an_invalid_return_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(): NonExistingType {}
            "});
    }

    #[test]
    fn test_defining_function_with_duplicated_parameter_name() {
        check_and_assert_is_err(indoc! {"
                fn addSumOf(x: Int, x: Int) {}
            "});
    }

    #[test]
    fn test_defining_function_with_an_invalid_parameter_type() {
        check_and_assert_is_err(indoc! {"
                fn foo(x: NonExistingType) {}
            "});
    }

    #[test]
    fn test_returning_value_from_function_with_void_return_type() {
        check_and_assert_is_err(indoc! {"
                fn foo() {
                    return 5;
                }
            "});
    }

    #[test]
    fn test_returning_value_from_function_with_mismatched_return_type() {
        check_and_assert_is_err(indoc! {"
                fn sum(x: Int, y: Int): Int {
                    return 5.0;
                }
            "});
    }

    #[test]
    fn test_using_invalid_statement_after_return() {
        check_and_assert_is_err(indoc! {"
                fn getFive(): Int {
                    return 5;
                    1 + true; // should be error
                }
            "});
    }

    #[test]
    fn test_defining_overloaded_functions_with_duplicated_parameters() {
        check_and_assert_is_err(indoc! {"
                fn sum(x: Int, y: Int): Int {
                    return x + y;
                }

                fn sum(x: Int, y: Int): Float {
                    return x + y;
                }
            "});
    }

    #[test]
    fn test_calling_overloaded_function_with_a_non_existing_variant() {
        check_and_assert_is_err(indoc! {"
                fn foo(x: Int) {}
                fn foo(x: Float) {}

                fn main() {
                    foo(true);
                }
            "});
    }

    // #[test]
    // fn test_using_return_statement_not_in_function_scope() {
    //     check_and_assert_is_err(indoc! {"
    //             if true {
    //                 return;
    //             }
    //         "});
    // }

    #[test]
    fn test_defining_function_with_missing_return_in_other_branches() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int {
                    if false {
                        return 5;
                    }
                }
            "});
    }

    #[test]
    fn test_defining_function_with_missing_return_in_else_or_outer_branches() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int {
                    if false {
                        return 0;
                    } else if !true {
                        return 0;
                    }
                }
            "});
    }

    #[test]
    fn test_defining_function_with_missing_return_in_outer_branch_of_while_statement() {
        check_and_assert_is_err(indoc! {"
                fn foo(): Int {
                    while false {
                        return 0;
                    }
                }
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
            ("print(703 + 5 - 90 * 100 / 86 * 0.5);", Type::Void),
            ("767 >= 900 == (45 < 67);", Type::Bool),
            ("false || !false && 50 > 0;", Type::Bool),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let checker = SemanticChecker::default();
            let result = checker.check_expression(&ast.statements[0]);

            assert!(result.is_ok());
            assert_eq!(result.unwrap(), expected);
        }
    }

    #[test]
    fn test_invalid_expression() {
        let cases = [
            "50 * print(73);",
            "print(50 + print(90));",
            "100 - notExist;",
            "-(print);",
            "-true;",
            "true > false;",
            "93 != 93.0;",
            "!5;",
            "false || !false && 50;",
        ];

        for input in cases {
            check_and_assert_is_err(&format!("fn main() {{ {} }}", input));
        }
    }
}
