// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! A temporary prototype of the runtime that currently being used.
//!
//! It accept the raw AST as is and will be replaced by a real runtime
//! that operates on bytecodes.

use compiler::ast::{AstNode, Program as ProgramAst, Value};
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt::Display,
    io::Write,
};

pub type WriteStream<'a> = &'a mut dyn Write;

pub struct Runtime<'a> {
    ast: Option<ProgramAst>,
    variables: RefCell<HashMap<String, Value>>,
    pub error: Option<String>,

    // IO streams
    output_stream: RefCell<WriteStream<'a>>,
    _error_stream: RefCell<WriteStream<'a>>,
}

impl<'a> Runtime<'a> {
    pub fn new(
        ast: ProgramAst,
        output_stream: WriteStream<'a>,
        error_stream: WriteStream<'a>,
    ) -> Self {
        Self {
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
            None => Value::Integer(0),
        };

        self.variables
            .borrow_mut()
            .insert(String::from(identifier), value);

        Ok(())
    }

    fn update_value(&self, lhs: AstNode, value: AstNode) -> Result<(), RuntimeError> {
        // TODO: make lhs to be able to use more expression (currently only identifier)

        match lhs {
            AstNode::Identifier { name, .. } => {
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

    fn run_expression(&self, expression: AstNode) -> Result<Value, RuntimeError> {
        match expression {
            AstNode::Identifier { name, .. } => self.get_variable_value(&name),
            AstNode::Literal { value, .. } => Ok(value),
            AstNode::FunctionCall { callee, args, .. } => self.run_function_call(&callee, args),
            AstNode::Add { lhs, rhs, .. } => {
                Ok(self.run_expression(*lhs)? + self.run_expression(*rhs)?)
            }
            AstNode::Sub { lhs, rhs, .. } => {
                Ok(self.run_expression(*lhs)? - self.run_expression(*rhs)?)
            }
            AstNode::Mul { lhs, rhs, .. } => {
                Ok(self.run_expression(*lhs)? * self.run_expression(*rhs)?)
            }
            AstNode::Div { lhs, rhs, .. } => {
                Ok(self.run_expression(*lhs)? / self.run_expression(*rhs)?)
            }
            AstNode::Neg { child, .. } => Ok(-self.run_expression(*child)?),

            _ => unreachable!(),
        }
    }

    fn get_variable_value(&self, name: &str) -> Result<Value, RuntimeError> {
        self.variables
            .borrow()
            .get(name)
            .copied()
            .ok_or(RuntimeError::VariableNotExist(String::from(name)))
    }

    fn run_function_call(
        &self,
        callee: &AstNode,
        args: Vec<AstNode>,
    ) -> Result<Value, RuntimeError> {
        let callee = match callee {
            AstNode::Identifier { name, .. } => name,
            _ => todo!("function callee"),
        };

        let mut evaluated_args: Vec<Value> = vec![];
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

        Ok(Value::Integer(0))
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
    use compiler::Compiler;
    use indoc::indoc;

    #[test]
    fn test_creating_variable() {
        let cases = [
            // (statement, variable name, expected value)
            ("var x = 10 * 20;", "x", Value::Integer(200)),
            ("var abc = 2 * 0.5;", "abc", Value::Float(1.0)),
        ];

        let mut unused_output_stream = vec![];
        let mut unused_error_stream = vec![];

        for (statement, variable_name, expected) in cases {
            let ast = Compiler::from_source_code(statement).compile().unwrap();
            let mut runtime =
                Runtime::new(ast, &mut unused_output_stream, &mut unused_error_stream);

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
            Value::Integer(5000),
        )];

        let mut unused_output_stream = vec![];
        let mut unused_error_stream = vec![];

        for (statement, lhs, expected) in cases {
            let ast = Compiler::from_source_code(statement).compile().unwrap();
            let mut runtime =
                Runtime::new(ast, &mut unused_output_stream, &mut unused_error_stream);

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

        for (source_code, output_content) in cases {
            let mut output_stream = vec![];
            let mut error_stream = vec![];

            let ast = Compiler::from_source_code(source_code).compile().unwrap();

            let mut runtime = Runtime::new(ast, &mut output_stream, &mut error_stream);
            let result = runtime.run();

            assert!(result.is_ok());
            assert_eq!(output_content, &output_stream);
        }
    }
}
