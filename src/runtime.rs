// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! A temporary prototype of the runtime that currently being used.
//!
//! It accept the raw AST as is and will be replaced by a real runtime
//! that operates on bytecodes.

use crate::ast::{AstNode, Program as ProgramAst, Value};
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt::Display,
    io::Write,
};

pub type WriteStream<'a> = &'a mut dyn Write;

pub struct Runtime<'a> {
    _src: String,
    ast: Option<ProgramAst>,
    variables: RefCell<HashMap<String, i64>>,
    pub error: Option<String>,

    // IO streams
    output_stream: RefCell<WriteStream<'a>>,
    _error_stream: RefCell<WriteStream<'a>>,
}

impl<'a> Runtime<'a> {
    pub fn new(
        src: &str,
        ast: ProgramAst,
        output_stream: WriteStream<'a>,
        error_stream: WriteStream<'a>,
    ) -> Self {
        Self {
            _src: String::from(src),
            ast: Some(ast),
            variables: RefCell::new(HashMap::new()),
            error: None,
            output_stream: RefCell::new(output_stream),
            _error_stream: RefCell::new(error_stream),
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        let mut statements = VecDeque::from(self.ast.take().unwrap().statements);

        loop {
            let statement = statements.pop_front();
            if statement.is_none() {
                break;
            }

            match statement.unwrap() {
                AstNode::VariableDeclaration {
                    identifier,
                    r#type,
                    value,
                } => self.create_variable(&identifier, r#type.clone(), value.map(|v| *v))?,

                AstNode::ValueAssignment { lhs, value } => self.update_value(*lhs, *value)?,

                node => {
                    self.run_expression(node)?;
                }
            };
        }

        Ok(())
    }

    fn create_variable(
        &self,
        identifier: &str,
        r#type: Option<String>,
        value: Option<AstNode>,
    ) -> Result<(), RuntimeError> {
        if self.variables.borrow_mut().contains_key(identifier) {
            return Err(RuntimeError::VariableAlreadyExist(String::from(identifier)));
        }

        if r#type.is_some() {
            todo!("variable declaration type notation");
        }

        let value = match value {
            Some(expression) => self.run_expression(expression)?,
            None => 0,
        };

        self.variables
            .borrow_mut()
            .insert(String::from(identifier), value);

        Ok(())
    }

    fn update_value(&self, lhs: AstNode, value: AstNode) -> Result<(), RuntimeError> {
        // TODO: make lhs to be able to use more expression (currently only identifier)

        match lhs {
            AstNode::Identifier(name) => {
                if !self.variables.borrow_mut().contains_key(&name) {
                    return Err(RuntimeError::VariableNotExist(name));
                }

                *self.variables.borrow_mut().get_mut(&name).unwrap() =
                    self.run_expression(value)?;
            }

            _ => todo!("more expression for value assignment"),
        }

        Ok(())
    }

    fn run_expression(&self, expression: AstNode) -> Result<i64, RuntimeError> {
        match expression {
            AstNode::Identifier(name) => self.get_variable_value(&name),
            AstNode::Val(Value::Integer(value)) => Ok(value),
            AstNode::FunctionCall { callee, args } => self.run_function_call(&callee, args),
            AstNode::Add(lhs, rhs) => Ok(self.run_expression(*lhs)? + self.run_expression(*rhs)?),
            AstNode::Sub(lhs, rhs) => Ok(self.run_expression(*lhs)? - self.run_expression(*rhs)?),
            AstNode::Mul(lhs, rhs) => Ok(self.run_expression(*lhs)? * self.run_expression(*rhs)?),
            AstNode::Div(lhs, rhs) => Ok(self.run_expression(*lhs)? / self.run_expression(*rhs)?),

            _ => unreachable!(),
        }
    }

    fn get_variable_value(&self, name: &str) -> Result<i64, RuntimeError> {
        self.variables
            .borrow()
            .get(name)
            .copied()
            .ok_or(RuntimeError::VariableNotExist(String::from(name)))
    }

    fn run_function_call(&self, callee: &AstNode, args: Vec<AstNode>) -> Result<i64, RuntimeError> {
        let callee = match callee {
            AstNode::Identifier(identifier) => identifier,
            _ => todo!("function callee"),
        };

        let mut evaluated_args: Vec<i64> = vec![];
        for arg in args {
            evaluated_args.push(self.run_expression(arg)?);
        }

        match callee.as_str() {
            "print" => {
                if evaluated_args.len() != 1 {
                    return Err(RuntimeError::WrongNumberOfArguments {
                        expected: 1,
                        provided: evaluated_args.len(),
                    });
                }
                writeln!(self.output_stream.borrow_mut(), "{}", evaluated_args[0]).unwrap();
            }

            _ => todo!("other function"),
        }

        Ok(0)
    }
}

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    VariableAlreadyExist(String),
    VariableNotExist(String),
    IdentifierNotExist(String),
    WrongNumberOfArguments { expected: usize, provided: usize },
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableAlreadyExist(name) => writeln!(f, "variable `{name}` already exist"),
            Self::VariableNotExist(name) => writeln!(f, "variable `{name}` is not exist"),
            Self::IdentifierNotExist(name) => writeln!(f, "identifier `{name}` is not exist"),
            Self::WrongNumberOfArguments { expected, provided } => writeln!(
                f,
                "expecting {} number of argument(s), get {} instead",
                expected, provided
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};
    use indoc::indoc;

    #[test]
    fn test_creating_variable() {
        let cases = [
            // (statement, variable name, expected value)
            ("var x = 10 * 20;", "x", 200),
            ("var abc = 200 + 50 * 2;", "abc", 300),
        ];

        let mut unused_output_stream = vec![];
        let mut unused_error_stream = vec![];

        for (statement, variable_name, expected) in cases {
            let tokens = lexer::lex(statement).unwrap();
            let ast = parser::parse(tokens).unwrap();
            let mut runtime = Runtime::new(
                statement,
                ast,
                &mut unused_output_stream,
                &mut unused_error_stream,
            );

            let result = runtime.run();

            assert!(result.is_ok());
            assert!(runtime.variables.borrow().contains_key(variable_name));
            assert_eq!(
                runtime.variables.borrow().get(variable_name).unwrap(),
                &expected
            );
        }
    }

    #[test]
    fn test_assigning_value() {
        let cases = [(
            indoc! {"
                var x = 50;
                var y = 100;

                x = x * y;
            "},
            "x",
            5000,
        )];

        let mut unused_output_stream = vec![];
        let mut unused_error_stream = vec![];

        for (statement, lhs, expected) in cases {
            let tokens = lexer::lex(statement).unwrap();
            let ast = parser::parse(tokens).unwrap();
            let mut runtime = Runtime::new(
                statement,
                ast,
                &mut unused_output_stream,
                &mut unused_error_stream,
            );

            let result = runtime.run();

            assert!(result.is_ok());
            assert!(runtime.variables.borrow().contains_key(lhs));
            assert_eq!(runtime.variables.borrow().get(lhs).unwrap(), &expected);
        }
    }

    #[test]
    fn test_print_value() {
        let cases = [
            // (input program, expected output stream content)
            (
                indoc! {"
                    var x = 10;

                    print(x);
                "},
                "10\n".as_bytes(),
            ),
            (
                indoc! {"
                    var x = 5;
                    var y = 10;

                    print(x * y);
                "},
                "50\n".as_bytes(),
            ),
            (
                indoc! {"
                    var x = 2048;
                    print(x);

                    x = 1024;
                    print(x);
                "},
                "2048\n1024\n".as_bytes(),
            ),
        ];

        for (input, output_content) in cases {
            let mut output_stream = vec![];
            let mut error_stream = vec![];

            let tokens = lexer::lex(input).unwrap();
            let ast = parser::parse(tokens).unwrap();

            let mut runtime = Runtime::new(input, ast, &mut output_stream, &mut error_stream);
            let result = runtime.run();

            assert!(result.is_ok());
            assert_eq!(output_content, &output_stream);
        }
    }
}
