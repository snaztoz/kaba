// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the implementation for semantic analysis
//! stage of the compiler.

use crate::ast::{AstNode, Program as ProgramAst, Value};
use builtin::{self, types::Types as BuiltinTypes};
use logos::Span;
use std::{collections::HashMap, fmt::Display, str::FromStr};

pub fn check(ast: ProgramAst) -> Result<ProgramAst, SemanticError> {
    let mut checker = SemanticChecker::new();
    checker.check(&ast)?;
    Ok(ast)
}

struct SemanticChecker {
    identifiers: HashMap<String, BuiltinTypes>,
}

impl SemanticChecker {
    fn new() -> Self {
        let mut identifiers = HashMap::new();

        identifiers.extend(builtin::functions::get_types());

        Self { identifiers }
    }

    fn check(&mut self, ast: &ProgramAst) -> Result<(), SemanticError> {
        for statement in &ast.statements {
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

                AstNode::ValueAssignment { lhs, value, span } => {
                    self.check_value_assignment(lhs, value, span)?
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

        if self.identifiers.contains_key(&identifier_name) {
            return Err(SemanticError::VariableAlreadyExist {
                name: identifier_name,
                span: identifier_span,
            });
        }

        // Get var_type

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

        // Either var_type or value_type must be present

        if var_type.is_none() && value_type.is_none() {
            return Err(SemanticError::UnableToInferVariableType {
                name: identifier_name,
                span: span.clone(),
            });
        }

        // If both present, check if value can be assigned to
        // the variable

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

        self.identifiers
            .insert(identifier_name, var_type.or(value_type).unwrap());

        Ok(())
    }

    fn check_value_assignment(
        &self,
        lhs: &AstNode,
        value: &AstNode,
        span: &Span,
    ) -> Result<(), SemanticError> {
        let lhs_type = self.get_expression_type(lhs)?;
        let value_type = self.get_expression_type(value)?;

        if !value_type.is_assignable_to(&lhs_type) {
            return Err(SemanticError::UnableToAssignValueType {
                var_type: lhs_type.to_string(),
                value_type: value_type.to_string(),
                span: span.clone(),
            });
        }

        Ok(())
    }

    fn get_expression_type(&self, expression: &AstNode) -> Result<BuiltinTypes, SemanticError> {
        match expression {
            AstNode::Eq { lhs, rhs, .. } | AstNode::Neq { lhs, rhs, .. } => {
                self.get_equality_operation_type(lhs, rhs)
            }

            AstNode::Gt { lhs, rhs, .. }
            | AstNode::Gte { lhs, rhs, .. }
            | AstNode::Lt { lhs, rhs, .. }
            | AstNode::Lte { lhs, rhs, .. } => self.get_comparison_operation_type(lhs, rhs),

            AstNode::Add { lhs, rhs, .. }
            | AstNode::Sub { lhs, rhs, .. }
            | AstNode::Mul { lhs, rhs, .. }
            | AstNode::Div { lhs, rhs, .. } => self.get_math_binary_operation_type(lhs, rhs),

            AstNode::Neg { child, .. } => self.get_neg_operation_type(child),

            AstNode::Identifier { name, span } => self.get_identifier_type(name, span),

            AstNode::Literal { value, .. } => self.get_literal_type(value),

            AstNode::FunctionCall { callee, args, span } => {
                self.get_function_call_return_type(callee, args, span)
            }

            _ => unreachable!(),
        }
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
                span: lhs.get_span().clone(),
            });
        }

        Ok(BuiltinTypes::Boolean)
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
                span: lhs.get_span().clone(),
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

        Ok(BuiltinTypes::Boolean)
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
        self.identifiers
            .get(name)
            .cloned()
            .ok_or(SemanticError::VariableNotExist {
                name: String::from(name),
                span: span.clone(),
            })
    }

    fn get_literal_type(&self, literal_value: &Value) -> Result<BuiltinTypes, SemanticError> {
        match literal_value {
            Value::Integer(_) => Ok(BuiltinTypes::Int),
            Value::Float(_) => Ok(BuiltinTypes::Float),
            Value::Boolean(_) => Ok(BuiltinTypes::Boolean),
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

    NotAFunction {
        span: Span,
    },

    FunctionArgumentCountMismatch {
        expecting: usize,
        get: usize,
        span: Span,
    },
}

impl SemanticError {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Self::UnableToInferVariableType { span, .. }
            | Self::UnableToAssignValueType { span, .. }
            | Self::UnableToCompareTypeAWithTypeB { span, .. }
            | Self::VariableNotExist { span, .. }
            | Self::VariableAlreadyExist { span, .. }
            | Self::TypeNotExist { span, .. }
            | Self::NotANumber { span, .. }
            | Self::NotAFunction { span, .. }
            | Self::FunctionArgumentCountMismatch { span, .. } => Some(span.clone()),
        }
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnableToInferVariableType { name, .. } => {
                write!(
                    f,
                    "unable to infer the type of variable `{name}`. Please provide a type or initial value",
                )
            }
            Self::UnableToAssignValueType {
                var_type,
                value_type,
                ..
            } => {
                write!(
                    f,
                    "unable to assign value of type `{value_type}` to type `{}`",
                    var_type
                )
            }
            Self::UnableToCompareTypeAWithTypeB { type_a, type_b, .. } => {
                write!(
                    f,
                    "unable to compare the value of type `{type_a}` with type `{type_b}`",
                )
            }

            Self::VariableAlreadyExist { name, .. } => {
                write!(f, "variable `{name}` already exists")
            }
            Self::VariableNotExist { name, .. } => write!(f, "variable `{name}` is not exists"),
            Self::TypeNotExist { name, .. } => write!(f, "type `{name}` is not exists"),

            Self::NotANumber { .. } => write!(f, "not a number"),
            Self::NotAFunction { .. } => write!(f, "not a function"),

            Self::FunctionArgumentCountMismatch { expecting, get, .. } => {
                write!(f, "expecting {expecting} argument(s) but get {get} instead")
            }
        }
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
            ("var x = true;", BuiltinTypes::Boolean),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast);

            assert!(result.is_ok());
            assert_eq!(*checker.identifiers.get("x").unwrap(), expected);
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
            let result = checker.check(&ast);

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
            let result = checker.check(&ast);

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
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_expression_type() {
        let cases = [
            ("-5 + 50 * 200 / 7 - 999;", BuiltinTypes::Int),
            ("-5 + -0.25;", BuiltinTypes::Float),
            ("print(703 + 5 - 90 * 100 / 86 * 0.5);", BuiltinTypes::Void),
            ("767 >= 900 == (45 < 67);", BuiltinTypes::Boolean),
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
        ];

        for input in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast);

            assert!(result.is_err());
        }
    }
}
