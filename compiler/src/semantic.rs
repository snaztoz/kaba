// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the implementation for semantic analysis
//! stage of the compiler.

use self::scope::{Scope, ScopeType};
use crate::ast::{AstNode, Program as ProgramAst, Value};
use builtin::{self, types::Types as BuiltinTypes};
use logos::Span;
use std::{fmt::Display, str::FromStr};

mod scope;

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
            let vt =
                BuiltinTypes::from_str(&type_name).map_err(|_| SemanticError::TypeNotExist {
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
    ) -> Result<BuiltinTypes, SemanticError> {
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

        Ok(BuiltinTypes::Void)
    }

    fn check_conditional_branch(
        &mut self,
        condition: &AstNode,
        body: &[AstNode],
        or_else: &Option<&AstNode>,
    ) -> Result<(), SemanticError> {
        // Expecting boolean type for the condition

        if self.get_expression_type(condition)? != BuiltinTypes::Bool {
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

        if self.get_expression_type(condition)? != BuiltinTypes::Bool {
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

    fn get_expression_type(&self, expression: &AstNode) -> Result<BuiltinTypes, SemanticError> {
        match expression {
            AstNode::Assign { lhs, rhs, span } => self.check_assignment(lhs, rhs, span),

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
    ) -> Result<BuiltinTypes, SemanticError> {
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

        Ok(BuiltinTypes::Bool)
    }

    fn get_equality_operation_type(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<BuiltinTypes, SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let rhs_type = self.get_expression_type(rhs)?;

        if lhs_type != rhs_type {
            return Err(SemanticError::UnableToCompareTypeAWithTypeB {
                type_a: lhs_type,
                type_b: rhs_type,
                span: lhs.get_span().clone().start..rhs.get_span().clone().end,
            });
        }

        Ok(BuiltinTypes::Bool)
    }

    fn get_comparison_operation_type(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<BuiltinTypes, SemanticError> {
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

        Ok(BuiltinTypes::Bool)
    }

    fn get_math_binary_operation_type(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
    ) -> Result<BuiltinTypes, SemanticError> {
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

        if lhs_type == BuiltinTypes::Int && rhs_type == BuiltinTypes::Int {
            Ok(BuiltinTypes::Int)
        } else {
            Ok(BuiltinTypes::Float)
        }
    }

    fn get_logical_not_operation_type(
        &self,
        child: &AstNode,
    ) -> Result<BuiltinTypes, SemanticError> {
        let child_type = self.get_expression_type(child)?;
        if !child_type.is_boolean() {
            return Err(SemanticError::NotABoolean {
                span: child.get_span(),
            });
        }
        Ok(BuiltinTypes::Bool)
    }

    fn get_neg_operation_type(&self, child: &AstNode) -> Result<BuiltinTypes, SemanticError> {
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
    ) -> Result<BuiltinTypes, SemanticError> {
        let callee_type = self.get_expression_type(callee)?;

        if let BuiltinTypes::Callable {
            parameters,
            return_type,
        } = &callee_type
        {
            if args.len() != parameters.len() {
                return Err(SemanticError::FunctionArgumentCountMismatch {
                    expecting: parameters.len(),
                    get: args.len(),
                    span: span.clone(),
                });
            }

            for i in 0..parameters.len() {
                let arg_type = self.get_expression_type(&args[i])?;
                if !arg_type.is_assignable_to(&parameters[i]) {
                    return Err(SemanticError::UnableToAssignValueType {
                        var_type: parameters[i].to_string(),
                        value_type: arg_type.to_string(),
                        span: args[i].get_span(),
                    });
                }
            }

            Ok(*return_type.clone())
        } else {
            Err(SemanticError::NotAFunction {
                span: callee.get_span().clone(),
            })
        }
    }

    fn get_identifier_type(&self, name: &str, span: &Span) -> Result<BuiltinTypes, SemanticError> {
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

    fn get_literal_type(&self, literal_value: &Value) -> Result<BuiltinTypes, SemanticError> {
        match literal_value {
            Value::Integer(_) => Ok(BuiltinTypes::Int),
            Value::Float(_) => Ok(BuiltinTypes::Float),
            Value::Boolean(_) => Ok(BuiltinTypes::Bool),
        }
    }

    fn get_current_scope(&self) -> &Scope {
        &self.scopes[self.scopes.len() - 1]
    }

    fn insert_to_current_scope_symbols(&mut self, name: &str, symbol_type: BuiltinTypes) {
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
        type_a: BuiltinTypes,
        type_b: BuiltinTypes,
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

    FunctionArgumentCountMismatch {
        expecting: usize,
        get: usize,
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
            | Self::FunctionArgumentCountMismatch { span, .. }
            | Self::LoopControlNotInLoopScope { span } => Some(span.clone()),
        }
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::UnableToInferVariableType { name, .. } => {
                    format!("unable to infer the type of variable `{name}` because of no type or initial value were provided")
                }
                Self::UnableToAssignValueType {
                    var_type,
                    value_type,
                    ..
                } => {
                    format!("unable to assign value of type `{value_type}` to type `{var_type}`")
                }
                Self::InvalidAssignmentLhs { lhs, .. } => {
                    format!("{lhs} can not be an assignment's lhs")
                }
                Self::UnableToCompareTypeAWithTypeB { type_a, type_b, .. } => {
                    format!("unable to compare the value of type `{type_a}` with type `{type_b}`")
                }
                Self::VariableAlreadyExist { name, .. } => {
                    format!("variable `{name}` already exists in current scope")
                }
                Self::VariableNotExist { name, .. } => {
                    format!("variable `{name}` is not exist in current scope")
                }
                Self::TypeNotExist { name, .. } => {
                    format!("type `{name}` is not exist")
                }
                Self::NotANumber { .. } => {
                    "not a number".to_string()
                }
                Self::NotABoolean { .. } => {
                    "not a boolean".to_string()
                }
                Self::NotAFunction { .. } => {
                    "not a function".to_string()
                }
                Self::FunctionArgumentCountMismatch { expecting, get, .. } => {
                    format!("expecting {expecting} argument(s) but get {get} instead")
                }
                Self::LoopControlNotInLoopScope { .. } => {
                    "the usage of `break` and `continue` statements must be within a loop"
                        .to_string()
                }
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};
    use indoc::indoc;

    #[test]
    fn test_variable_declaration_semantic() {
        let cases = [
            ("var x: Int = 5;", BuiltinTypes::Int),
            ("var x: Int;", BuiltinTypes::Int),
            ("var x = -0.5;", BuiltinTypes::Float),
            ("var x = true;", BuiltinTypes::Bool),
        ];

        for (input, expected) in cases {
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
    }

    #[test]
    fn test_invalid_variable_declaration_semantic() {
        let cases = [
            "var x: Int = 5.0;",
            "var x;",
            "var x: NonExistingType;",
            "var x = 5; var x = 10;", // declare twice
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_value_assignment() {
        let cases = [
            indoc! {"
                var x: Int;
                x = 50;
            "},
            indoc! {"
                var x: Float;
                x = 50;
            "},
            indoc! {"
                var b: Bool = false;
                b = true;
            "},
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_invalid_value_assignment() {
        let cases = [
            indoc! {"
                var x: Int;
                x = 5.0;
            "},
            indoc! {"
                var x: Float;
                x = y;
            "},
            indoc! {"
                1 + 1 = 5;
            "},
            indoc! {"
                true = false;
            "},
            indoc! {"
                (50) = true;
            "},
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_conditional_branch() {
        let cases = [
            indoc! {"
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
            "},
            indoc! {"
                if 1 + 1 == 2 {
                    if 2 + 2 == 4 {
                        if 3 + 3 == 6 {
                            print(true);
                        }
                    }
                }
            "},
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_invalid_conditional_branch() {
        let cases = [
            indoc! {"
                if true {
                    var x = 50;
                    print(x);
                }

                print(x);
            "},
            indoc! {"
                if 1 + 1 {
                    print(1);
                }
            "},
            indoc! {"
                if true {
                    var x = 50;
                } else {
                    print(x);
                }
            "},
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_loop() {
        let cases = [
            indoc! {"
                while 2 > 5 {
                    print(1);
                }
            "},
            indoc! {"
                var a = 5;

                while true {
                    if a == 5 {
                        break;
                    }
                    print(0);
                }
            "},
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_invalid_loop() {
        let cases = [
            indoc! {"
                while 5 + 5 {}
            "},
            indoc! {"
                if true {
                    break;
                }
            "},
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_expression_type() {
        let cases = [
            ("-5 + 50 * 200 / 7 - 999;", BuiltinTypes::Int),
            ("-5 + -0.25;", BuiltinTypes::Float),
            ("99.9 % 0.1;", BuiltinTypes::Float),
            ("print(703 + 5 - 90 * 100 / 86 * 0.5);", BuiltinTypes::Void),
            ("767 >= 900 == (45 < 67);", BuiltinTypes::Bool),
            ("false || !false && 50 > 0;", BuiltinTypes::Bool),
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
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast.statements);

            assert!(result.is_err());
        }
    }
}
