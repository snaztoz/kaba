// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the implementation for semantic analysis
//! stage of the compiler.

use self::{
    scope::{Scope, ScopeType},
    types::Type,
};
use crate::ast::{AstNode, Program as ProgramAst, Value};
use logos::Span;
use std::{fmt::Display, str::FromStr};

mod builtin_functions;
mod scope;
mod types;

/// Provides a quick way to run semantic analysis on a Kaba AST.
pub fn check(ast: &ProgramAst) -> Result<(), SemanticError> {
    let mut checker = SemanticChecker::new();
    checker.check(&ast.statements)?;
    Ok(())
}

struct SemanticChecker {
    scopes: Vec<Scope>,
}

impl SemanticChecker {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new_builtin_scope(), Scope::new_global_scope()],
        }
    }

    fn check(&mut self, statements: &[AstNode]) -> Result<(), SemanticError> {
        for statement in statements {
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
                } => self.check_conditional_branch(condition, body, &or_else.as_deref())?,

                AstNode::While {
                    condition, body, ..
                } => self.check_while(condition, body)?,

                AstNode::Break { span } | AstNode::Continue { span } => {
                    self.check_loop_control(span)?
                }

                AstNode::FunctionDefinition {
                    name,
                    parameters,
                    return_type,
                    body,
                    ..
                } => self.check_function_definition(name, parameters, return_type, body)?,

                AstNode::Return { expression, span } => self.check_return(expression, span)?,

                expression => {
                    self.get_expression_type(expression)?;
                }
            }
        }

        Ok(())
    }

    fn check_variable_declaration(
        &mut self,
        identifier: &AstNode,
        var_type: &Option<&AstNode>,
        value: &Option<&AstNode>,
        span: &Span,
    ) -> Result<(), SemanticError> {
        let (identifier_name, identifier_span) = identifier.unwrap_identifier();

        // Can't have the same variable be declared multiple times
        // in the same scope

        if self
            .get_current_scope()
            .symbols
            .contains_key(&identifier_name)
        {
            return Err(SemanticError::VariableAlreadyExist {
                name: identifier_name,
                span: identifier_span,
            });
        }

        // Get variable type

        let var_type = if let Some(vt) = var_type {
            let (type_name, type_span) = vt.unwrap_type_notation();
            let vt = Type::from_str(&type_name).map_err(|_| SemanticError::TypeNotExist {
                name: type_name,
                span: type_span,
            })?;
            Some(vt)
        } else {
            None
        };

        // Get value type

        let value_type = if let Some(expression) = value {
            Some(self.get_expression_type(expression)?)
        } else {
            None
        };

        // Either variable's or value's type must be present

        if var_type.is_none() && value_type.is_none() {
            return Err(SemanticError::UnableToInferVariableType {
                name: identifier_name,
                span: span.clone(),
            });
        }

        // If both present, check if value's type is compatible with
        // the variable's

        if let Some(var_type) = &var_type {
            if let Some(value_type) = &value_type {
                if !value_type.is_assignable_to(var_type) {
                    return Err(SemanticError::UnableToAssignValueType {
                        var_type: var_type.to_string(),
                        value_type: value_type.to_string(),
                        span: span.clone(),
                    });
                }
            }
        }

        // Save variable information

        self.insert_to_current_scope_symbols(&identifier_name, var_type.or(value_type).unwrap());

        Ok(())
    }

    fn check_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let rhs_type = self.get_expression_type(rhs)?;

        if !lhs.can_be_assignment_lhs() {
            return Err(SemanticError::InvalidAssignmentLhs {
                lhs: lhs.to_string(),
                span: lhs.get_span(),
            });
        }

        if !rhs_type.is_assignable_to(&lhs_type) {
            return Err(SemanticError::UnableToAssignValueType {
                var_type: lhs_type.to_string(),
                value_type: rhs_type.to_string(),
                span: span.clone(),
            });
        }

        Ok(Type::Void)
    }

    fn check_shorthand_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let rhs_type = self.get_expression_type(rhs)?;

        if !lhs_type.is_number() {
            return Err(SemanticError::NotANumber {
                span: lhs.get_span(),
            });
        } else if !rhs_type.is_number() {
            return Err(SemanticError::NotANumber {
                span: rhs.get_span(),
            });
        }

        self.check_assignment(lhs, rhs, span)
    }

    fn check_conditional_branch(
        &mut self,
        condition: &AstNode,
        body: &[AstNode],
        or_else: &Option<&AstNode>,
    ) -> Result<(), SemanticError> {
        // Expecting boolean type for the condition

        if self.get_expression_type(condition)? != Type::Bool {
            return Err(SemanticError::NotABoolean {
                span: condition.get_span().clone(),
            });
        }

        // Check all statements inside the body with a new scope

        self.scopes.push(Scope::new_conditional_scope());
        self.check(body)?;
        self.scopes.pop();

        if or_else.is_none() {
            return Ok(());
        }

        match or_else.unwrap() {
            AstNode::If {
                condition,
                body,
                or_else,
                ..
            } => {
                self.check_conditional_branch(condition, body, &or_else.as_deref())?;
            }

            AstNode::Else { body, .. } => {
                // Check all statements inside the body with a new scope

                self.scopes.push(Scope::new_conditional_scope());
                self.check(body)?;
                self.scopes.pop();
            }

            _ => unreachable!(),
        }

        Ok(())
    }

    fn check_while(&mut self, condition: &AstNode, body: &[AstNode]) -> Result<(), SemanticError> {
        // Expecting boolean type for the condition

        if self.get_expression_type(condition)? != Type::Bool {
            return Err(SemanticError::NotABoolean {
                span: condition.get_span().clone(),
            });
        }

        // Check all statements inside the body with a new scope

        self.scopes.push(Scope::new_loop_scope());
        self.check(body)?;
        self.scopes.pop();

        Ok(())
    }

    fn check_loop_control(&mut self, span: &Span) -> Result<(), SemanticError> {
        for scope in self.scopes.iter().rev() {
            if scope.scope_type == ScopeType::Loop {
                return Ok(());
            }
        }

        Err(SemanticError::LoopControlNotInLoopScope { span: span.clone() })
    }

    fn check_function_definition(
        &mut self,
        name: &AstNode,
        parameters: &[(AstNode, AstNode)],
        return_type: &Option<Box<AstNode>>,
        body: &[AstNode],
    ) -> Result<(), SemanticError> {
        // Can't have function definition to be declared in scope other
        // than global

        if self.get_current_scope().scope_type != ScopeType::Global {
            return Err(SemanticError::FunctionDefinitionNotInGlobal {
                span: name.get_span(),
            });
        }

        // Can't use the name if it's already used by another data type.
        //
        // But if it's also a function, then it should be no problem
        // (function overloading).

        let (name, name_span) = name.unwrap_identifier();

        if self.get_current_scope().symbols.contains_key(&name) {
            return Err(SemanticError::VariableAlreadyExist {
                name,
                span: name_span,
            });
        }

        // Check its return type

        let return_type = if let Some(type_notation) = return_type {
            let (type_name, type_span) = type_notation.unwrap_type_notation();
            Type::from_str(&type_name).map_err(|_| SemanticError::TypeNotExist {
                name: type_name,
                span: type_span.clone(),
            })?
        } else {
            Type::Void
        };

        // Entering new scope

        self.scopes
            .push(Scope::new_function_scope(return_type.clone()));

        let mut parameter_types = vec![];
        for (parameter_name, parameter_type) in parameters {
            // Parameters can't have duplicated name

            let (parameter_name, parameter_name_span) = parameter_name.unwrap_identifier();
            if self
                .get_current_scope()
                .symbols
                .contains_key(&parameter_name)
            {
                return Err(SemanticError::VariableAlreadyExist {
                    name: parameter_name,
                    span: parameter_name_span,
                });
            }

            // Check if parameter type is valid

            let (type_name, type_span) = parameter_type.unwrap_type_notation();
            let parameter_type =
                Type::from_str(&type_name).map_err(|_| SemanticError::TypeNotExist {
                    name: type_name,
                    span: type_span,
                })?;

            // Save parameter information

            self.insert_to_current_scope_symbols(&parameter_name, parameter_type.clone());
            parameter_types.push(parameter_type);
        }

        // Create function type

        let function_type = Type::Callable {
            parameter_types: parameter_types.clone(),
            return_type: Box::new(return_type),
        };

        // Save function information

        let global_scope = self
            .scopes
            .iter_mut()
            .rev()
            .find(|scope| scope.scope_type == ScopeType::Global)
            .unwrap();

        let function_signature = format!("{name}{function_type}");

        if global_scope.symbols.contains_key(&function_signature) {
            return Err(SemanticError::FunctionVariantAlreadyExist {
                name,
                args: parameter_types,
                span: name_span,
            });
        }

        global_scope
            .symbols
            .insert(function_signature, function_type);

        // Check function body
        //
        // We do this last in order to accommodate features such as
        // recursive function call.

        self.check(body)?;
        self.scopes.pop();

        Ok(())
    }

    fn check_return(
        &self,
        expression: &Option<Box<AstNode>>,
        span: &Span,
    ) -> Result<(), SemanticError> {
        let expression_type = expression
            .as_ref()
            .map(|expr| self.get_expression_type(expr).unwrap())
            .unwrap_or(Type::Void);

        for scope in self.scopes.iter().rev() {
            if let ScopeType::Function { return_type } = &scope.scope_type {
                if !expression_type.is_assignable_to(return_type) {
                    return Err(SemanticError::ReturnTypeMismatch {
                        expecting: return_type.clone(),
                        get: expression_type,
                        span: span.clone(),
                    });
                }

                return Ok(());
            }
        }

        Err(SemanticError::ReturnNotInFunctionScope { span: span.clone() })
    }

    fn get_expression_type(&self, expression: &AstNode) -> Result<Type, SemanticError> {
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
                self.get_equality_operation_type(lhs, rhs)
            }

            AstNode::Or { lhs, rhs, .. } | AstNode::And { lhs, rhs, .. } => {
                self.get_logical_and_or_operation_type(lhs, rhs)
            }

            AstNode::Gt { lhs, rhs, .. }
            | AstNode::Gte { lhs, rhs, .. }
            | AstNode::Lt { lhs, rhs, .. }
            | AstNode::Lte { lhs, rhs, .. } => self.get_comparison_operation_type(lhs, rhs),

            AstNode::Add { lhs, rhs, .. }
            | AstNode::Sub { lhs, rhs, .. }
            | AstNode::Mul { lhs, rhs, .. }
            | AstNode::Div { lhs, rhs, .. }
            | AstNode::Mod { lhs, rhs, .. } => self.get_math_binary_operation_type(lhs, rhs),

            AstNode::Not { child, .. } => self.get_logical_not_operation_type(child),
            AstNode::Neg { child, .. } => self.get_neg_operation_type(child),

            AstNode::Identifier { name, span } => self.get_identifier_type(name, span),

            AstNode::Literal { value, .. } => self.get_literal_type(value),

            AstNode::FunctionCall { callee, args, span } => {
                self.get_function_call_return_type(callee, args, span)
            }

            _ => unreachable!(),
        }
    }

    fn get_logical_and_or_operation_type(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let rhs_type = self.get_expression_type(rhs)?;

        if !lhs_type.is_boolean() {
            return Err(SemanticError::NotABoolean {
                span: lhs.get_span().clone(),
            });
        } else if !rhs_type.is_boolean() {
            return Err(SemanticError::NotABoolean {
                span: rhs.get_span().clone(),
            });
        }

        Ok(Type::Bool)
    }

    fn get_equality_operation_type(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let rhs_type = self.get_expression_type(rhs)?;

        if lhs_type != rhs_type {
            return Err(SemanticError::UnableToCompareTypeAWithTypeB {
                type_a: lhs_type,
                type_b: rhs_type,
                span: lhs.get_span().clone().start..rhs.get_span().clone().end,
            });
        }

        Ok(Type::Bool)
    }

    fn get_comparison_operation_type(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let rhs_type = self.get_expression_type(rhs)?;

        if lhs_type != rhs_type {
            return Err(SemanticError::UnableToCompareTypeAWithTypeB {
                type_a: lhs_type,
                type_b: rhs_type,
                span: lhs.get_span().clone().start..rhs.get_span().clone().end,
            });
        } else if !lhs_type.is_number() {
            return Err(SemanticError::NotANumber {
                span: lhs.get_span().clone(),
            });
        } else if !rhs_type.is_number() {
            return Err(SemanticError::NotANumber {
                span: rhs.get_span().clone(),
            });
        }

        Ok(Type::Bool)
    }

    fn get_math_binary_operation_type(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<Type, SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let rhs_type = self.get_expression_type(rhs)?;

        if !lhs_type.is_number() {
            return Err(SemanticError::NotANumber {
                span: lhs.get_span().clone(),
            });
        } else if !rhs_type.is_number() {
            return Err(SemanticError::NotANumber {
                span: rhs.get_span().clone(),
            });
        }

        if lhs_type == Type::Int && rhs_type == Type::Int {
            Ok(Type::Int)
        } else {
            Ok(Type::Float)
        }
    }

    fn get_logical_not_operation_type(&self, child: &AstNode) -> Result<Type, SemanticError> {
        let child_type = self.get_expression_type(child)?;
        if !child_type.is_boolean() {
            return Err(SemanticError::NotABoolean {
                span: child.get_span(),
            });
        }
        Ok(Type::Bool)
    }

    fn get_neg_operation_type(&self, child: &AstNode) -> Result<Type, SemanticError> {
        let child_type = self.get_expression_type(child)?;
        if !child_type.is_number() {
            return Err(SemanticError::NotANumber {
                span: child.get_span(),
            });
        }
        Ok(child_type)
    }

    fn get_function_call_return_type(
        &self,
        callee: &AstNode,
        args: &[AstNode],
        span: &Span,
    ) -> Result<Type, SemanticError> {
        // transform the arguments into their respective type
        let mut arg_types = vec![];
        for arg in args {
            arg_types.push(self.get_expression_type(arg)?)
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

    fn get_identifier_type(&self, name: &str, span: &Span) -> Result<Type, SemanticError> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.symbols.get(name) {
                return Ok(t.clone());
            }
        }

        Err(SemanticError::VariableNotExist {
            name: String::from(name),
            span: span.clone(),
        })
    }

    fn get_literal_type(&self, literal_value: &Value) -> Result<Type, SemanticError> {
        match literal_value {
            Value::Void => Ok(Type::Void),
            Value::Integer(_) => Ok(Type::Int),
            Value::Float(_) => Ok(Type::Float),
            Value::Boolean(_) => Ok(Type::Bool),
        }
    }

    fn get_current_scope(&self) -> &Scope {
        &self.scopes[self.scopes.len() - 1]
    }

    fn insert_to_current_scope_symbols(&mut self, name: &str, symbol_type: Type) {
        let last_index = self.scopes.len() - 1;
        self.scopes[last_index]
            .symbols
            .insert(String::from(name), symbol_type);
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
    pub fn get_span(&self) -> Option<Span> {
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
            | Self::FunctionDefinitionNotInGlobal { span, .. }
            | Self::FunctionVariantAlreadyExist { span, .. }
            | Self::FunctionVariantNotExist { span, .. }
            | Self::ReturnNotInFunctionScope { span, .. }
            | Self::ReturnTypeMismatch { span, .. }
            | Self::LoopControlNotInLoopScope { span } => Some(span.clone()),
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
            Self::FunctionDefinitionNotInGlobal { .. } => {
                write!(f, "unable to define functions in non-global scope",)
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

        let mut checker = SemanticChecker::new();
        let result = checker.check(&ast.statements);

        assert!(result.is_ok());
    }

    fn check_and_assert_is_err(input: &str) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let mut checker = SemanticChecker::new();
        let result = checker.check(&ast.statements);

        assert!(result.is_err());
    }

    //
    // Test variable declarations
    //

    fn assert_the_type_of_x_is(input: &str, expected: Type) {
        let tokens = lexer::lex(input).unwrap();
        let ast = parser::parse(tokens).unwrap();

        let mut checker = SemanticChecker::new();
        let result = checker.check(&ast.statements);

        assert!(result.is_ok());
        assert_eq!(
            *checker.get_current_scope().symbols.get("x").unwrap(),
            expected
        );
    }

    #[test]
    fn test_check_variable_declaration_with_type_annotation_and_initial_value() {
        assert_the_type_of_x_is(
            indoc! {"
                var x: Int = 5;
            "},
            Type::Int,
        );
    }

    #[test]
    fn test_check_variable_declaration_with_type_annotation_only() {
        assert_the_type_of_x_is(
            indoc! {"
                var x: Int;
            "},
            Type::Int,
        );
    }

    #[test]
    fn test_check_variable_declaration_with_initial_value_only() {
        assert_the_type_of_x_is(
            indoc! {"
                var x = 5;
            "},
            Type::Int,
        );
    }

    #[test]
    fn test_check_variable_declaration_with_float_literal() {
        assert_the_type_of_x_is(
            indoc! {"
                var x = -0.5;
            "},
            Type::Float,
        );
    }

    #[test]
    fn test_check_variable_declaration_with_boolean_literal() {
        assert_the_type_of_x_is(
            indoc! {"
                var x = true;
            "},
            Type::Bool,
        );
    }

    #[test]
    fn test_using_incompatible_types_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                var x: Int = 5.0;
            "})
    }

    #[test]
    fn test_using_no_type_annotation_or_initial_value_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                var x;
            "})
    }

    #[test]
    fn test_using_non_existing_type_in_variable_declaration() {
        check_and_assert_is_err(indoc! {"
                var x: NonExistingType;
            "})
    }

    #[test]
    fn test_redeclaring_variable_in_the_same_scope() {
        check_and_assert_is_err(indoc! {"
                var x = 5;
                var x = 10;
            "})
    }

    //
    // Test value assignments
    //

    #[test]
    fn test_check_value_assignments() {
        check_and_assert_is_ok(indoc! {"
                var x: Int;
                x = 10;

                var y: Float;
                y = 5.0;

                var z = false;
                z = true;
            "})
    }

    #[test]
    fn test_check_shorthand_value_assignments() {
        check_and_assert_is_ok(indoc! {"
                var i = 0;
                i += 1;
                i -= 2;
                i *= 3;
                i /= 4;
                i %= 5;
            "})
    }

    #[test]
    fn test_check_mod_assign_with_float_types() {
        check_and_assert_is_ok(indoc! {"
                var i = 5.0;
                i %= 2.5;
            "})
    }

    #[test]
    fn test_assigning_value_with_mismatched_types() {
        check_and_assert_is_err(indoc! {"
                var x: Int;
                x = 5.0;
            "})
    }

    #[test]
    fn test_assigning_value_with_non_existing_variable() {
        check_and_assert_is_err(indoc! {"
                var x: Float;
                x = y;
            "})
    }

    #[test]
    fn test_using_math_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                1 + 1 = 5;
            "})
    }

    #[test]
    fn test_using_boolean_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                true || false = false;
            "})
    }

    #[test]
    fn test_using_integer_grouped_expression_as_lhs_in_value_assignment() {
        check_and_assert_is_err(indoc! {"
                (50) = true;
            "})
    }

    #[test]
    fn test_using_boolean_type_in_shorthand_value_assignment() {
        check_and_assert_is_err(indoc! {"
                true += true;
            "})
    }

    //
    // Test conditional branches
    //

    #[test]
    fn test_check_if_else_statements() {
        check_and_assert_is_ok(indoc! {"
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
            "})
    }

    #[test]
    fn test_check_nested_if_statements() {
        check_and_assert_is_ok(indoc! {"
                if 1 + 1 == 2 {
                    if 2 + 2 == 4 {
                        if 3 + 3 == 6 {
                            print(true);
                        }
                    }
                }
            "})
    }

    #[test]
    fn test_using_variable_after_out_of_conditional_scope() {
        check_and_assert_is_err(indoc! {"
                if true {
                    var x = 50;
                    print(x);
                }

                print(x);
            "})
    }

    #[test]
    fn test_using_math_expression_as_condition_in_if_statement() {
        check_and_assert_is_err(indoc! {"
                if 1 + 1 {
                    print(1);
                }
            "})
    }

    #[test]
    fn test_using_variable_declared_in_sibling_conditional_scope() {
        check_and_assert_is_err(indoc! {"
                if true {
                    var x = 50;
                } else {
                    print(x);
                }
            "})
    }

    //
    // Test loops
    //

    #[test]
    fn test_check_loop_statements() {
        check_and_assert_is_ok(indoc! {"
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
            "})
    }

    #[test]
    fn test_using_math_expression_as_condition_in_while_statement() {
        check_and_assert_is_err(indoc! {"
                while 5 + 5 {}
            "})
    }

    #[test]
    fn test_using_break_statement_not_in_loop_scope() {
        check_and_assert_is_err(indoc! {"
                if true {
                    break;
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

                var result = sum(5, 7);

                print(result);
            "});
    }

    #[test]
    fn test_defining_function_not_in_global_scope() {
        check_and_assert_is_err(indoc! {"
                if true {
                    fn foo() {}
                }
            "});
    }

    #[test]
    fn test_defining_function_with_an_already_taken_name_in_the_same_scope() {
        check_and_assert_is_err(indoc! {"
                var foo: Int = 0;

                fn foo() {}
            "});
    }

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

                foo(true);
            "});
    }

    #[test]
    fn test_using_return_statement_not_in_function_scope() {
        check_and_assert_is_err(indoc! {"
                if true {
                    return;
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

            let checker = SemanticChecker::new();
            let result = checker.get_expression_type(&ast.statements[0]);

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
            check_and_assert_is_err(input);
        }
    }
}
