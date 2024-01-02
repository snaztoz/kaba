// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the implementation for semantic analysis
//! stage of the compiler.

use crate::ast::{AstNode, Program as ProgramAst, Value};
use logos::Span;
use std::{collections::HashMap, fmt::Display};

pub fn check(ast: ProgramAst) -> Result<ProgramAst, SemanticError> {
    let mut checker = SemanticChecker::new();
    checker.check(&ast)?;
    Ok(ast)
}

struct SemanticChecker {
    variables: HashMap<String, Variable>,
}

impl SemanticChecker {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn check(&mut self, ast: &ProgramAst) -> Result<(), SemanticError> {
        for statement in &ast.statements {
            match statement {
                AstNode::VariableDeclaration {
                    identifier,
                    r#type,
                    value,
                    span,
                } => {
                    self.check_variable_declaration(identifier, r#type, &value.as_deref(), span)?
                }

                AstNode::ValueAssignment { lhs, value, span } => {
                    self.check_value_assignment(lhs, value, span)?
                }

                _ => (),
            }
        }

        Ok(())
    }

    fn check_variable_declaration(
        &mut self,
        identifier: &str,
        r#type: &Option<String>,
        value: &Option<&AstNode>,
        span: &Span,
    ) -> Result<(), SemanticError> {
        // Can't have the same variable be declared multiple times

        if self.variables.contains_key(identifier) {
            return Err(SemanticError::VariableAlreadyExist {
                name: String::from(identifier),
                span: span.clone(),
            });
        }

        // Get r#type

        let r#type = if let Some(t) = r#type {
            if !self.is_type_exist(t) {
                return Err(SemanticError::TypeNotExist {
                    name: String::from(t),
                    span: span.clone(),
                });
            }
            Some(String::from(t))
        } else {
            None
        };

        // Get value type

        let value_type = if let Some(expression) = value {
            Some(self.get_expression_type(expression)?)
        } else {
            None
        };

        // Either r#type or value must be present

        if r#type.is_none() && value_type.is_none() {
            return Err(SemanticError::UnableToInferVariableType {
                name: String::from(identifier),
                span: span.clone(),
            });
        }

        // If both present, check if value can be assigned to
        // the variable

        if let Some(t) = &r#type {
            if let Some(vt) = &value_type {
                if !self.can_assign_type(vt, t) {
                    return Err(SemanticError::UnableToAssignValueType {
                        r#type: t.clone(),
                        value_type: vt.clone(),
                        span: span.clone(),
                    });
                }
            }
        }

        // Save variable information

        self.variables.insert(
            String::from(identifier),
            Variable {
                r#type: r#type.or(value_type).unwrap(),
            },
        );

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

        if !self.can_assign_type(&value_type, &lhs_type) {
            return Err(SemanticError::UnableToAssignValueType {
                r#type: lhs_type.clone(),
                value_type: value_type.clone(),
                span: span.clone(),
            });
        }

        Ok(())
    }

    fn can_assign_type(&self, from: &str, to: &str) -> bool {
        if from == to {
            return true;
        }
        from == "Int" && to == "Float"
    }

    fn is_type_exist(&self, r#type: &str) -> bool {
        r#type == "Int" || r#type == "Float"
    }

    fn get_expression_type(&self, expression: &AstNode) -> Result<String, SemanticError> {
        match expression {
            AstNode::Add { lhs, rhs, .. }
            | AstNode::Sub { lhs, rhs, .. }
            | AstNode::Mul { lhs, rhs, .. }
            | AstNode::Div { lhs, rhs, .. } => {
                let lhs_type = self.get_expression_type(lhs)?;
                let rhs_type = self.get_expression_type(rhs)?;
                let r#type = if &lhs_type == "Int" && &rhs_type == "Int" {
                    String::from("Int")
                } else {
                    String::from("Float")
                };
                Ok(r#type)
            }

            AstNode::Neg { child, .. } => self.get_expression_type(child),

            AstNode::Identifier { name, span } => {
                self.variables.get(name).map(|v| v.r#type.clone()).ok_or(
                    SemanticError::VariableNotExist {
                        name: String::from(name),
                        span: span.clone(),
                    },
                )
            }

            AstNode::Literal { value, .. } => match value {
                Value::Integer(_) => Ok(String::from("Int")),
                Value::Float(_) => Ok(String::from("Float")),
            },

            _ => unreachable!(),
        }
    }
}

struct Variable {
    r#type: String,
}

#[derive(Debug, PartialEq)]
pub enum SemanticError {
    UnableToInferVariableType {
        name: String,
        span: Span,
    },

    UnableToAssignValueType {
        r#type: String,
        value_type: String,
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
}

impl SemanticError {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Self::UnableToInferVariableType { span, .. }
            | Self::UnableToAssignValueType { span, .. }
            | Self::VariableNotExist { span, .. }
            | Self::VariableAlreadyExist { span, .. }
            | Self::TypeNotExist { span, .. } => Some(span.clone()),
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
                r#type, value_type, ..
            } => {
                write!(
                    f,
                    "unable to assign value of type `{value_type}` to type `{}`",
                    r#type
                )
            }
            Self::VariableAlreadyExist { name, .. } => {
                write!(f, "variable `{name}` already exists")
            }
            Self::VariableNotExist { name, .. } => write!(f, "variable `{name}` is not exists"),
            Self::TypeNotExist { name, .. } => write!(f, "type `{name}` is not exists"),
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
            ("var x: Int = 5;", "Int"),
            ("var x: Int;", "Int"),
            ("var x = -0.5;", "Float"),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut checker = SemanticChecker::new();
            let result = checker.check(&ast);

            assert!(result.is_ok());
            assert_eq!(checker.variables.get("x").unwrap().r#type, expected);
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
            ("-5 + 50 * 200 / 7 - 999;", "Int"),
            ("-5 + -0.25;", "Float"),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let checker = SemanticChecker::new();
            let result = checker.get_expression_type(&ast.statements[0]);

            assert!(result.is_ok());
            assert_eq!(&result.unwrap(), expected);
        }
    }
}
