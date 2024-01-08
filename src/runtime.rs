// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! A temporary prototype of the runtime that currently being used.
//!
//! It accept the raw AST as is and will be replaced by a real runtime
//! that operates on bytecodes.

use compiler::ast::{AstNode, Program as ProgramAst, Value};
use std::{cell::RefCell, collections::HashMap, fmt::Display, io::Write};

pub type WriteStream<'a> = &'a mut dyn Write;
type Scope = HashMap<String, Value>;

pub struct Runtime<'a> {
    ast: Option<ProgramAst>,
    scopes: Vec<Scope>,
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
        let scopes = vec![
            HashMap::new(), // built-in
            HashMap::new(), // global
        ];

        Self {
            ast: Some(ast),
            scopes,
            error: None,
            output_stream: RefCell::new(output_stream),
            _error_stream: RefCell::new(error_stream),
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        let statements = self.ast.take().unwrap().statements;
        self.run_statements(&statements)
    }

    fn run_statements(&mut self, statements: &[AstNode]) -> Result<(), RuntimeError> {
        for statement in statements {
            match statement {
                AstNode::VariableDeclaration {
                    identifier, value, ..
                } => {
                    let name = identifier.unwrap_identifier().0;
                    self.create_variable(&name, value.as_deref())?
                }

                AstNode::ValueAssignment { lhs, value, .. } => self.update_value(lhs, value)?,

                AstNode::If {
                    condition,
                    body,
                    or_else,
                    ..
                } => self.run_conditional_branch(condition, body, or_else.as_deref())?,

                node => {
                    self.run_expression(node)?;
                }
            };
        }

        Ok(())
    }

    fn create_variable(
        &mut self,
        identifier: &str,
        value: Option<&AstNode>,
    ) -> Result<(), RuntimeError> {
        if self.is_current_scope_has_variable(identifier) {
            return Err(RuntimeError::VariableAlreadyExist(String::from(identifier)));
        }

        let value = match value {
            Some(expression) => self.run_expression(expression)?,
            None => Value::Integer(0),
        };

        self.create_variable_in_current_scope(identifier, value);

        Ok(())
    }

    fn update_value(&mut self, lhs: &AstNode, value: &AstNode) -> Result<(), RuntimeError> {
        // TODO: make lhs to be able to use more expression (currently only identifier)

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.update_variable_value(name, value)?;
            }

            _ => todo!("more expression for value assignment"),
        }

        Ok(())
    }

    fn run_conditional_branch(
        &mut self,
        condition: &AstNode,
        body: &[AstNode],
        or_else: Option<&AstNode>,
    ) -> Result<(), RuntimeError> {
        let should_execute = match self.run_expression(condition)? {
            Value::Boolean(b) => b,
            _ => unreachable!(),
        };

        if should_execute {
            self.scopes.push(HashMap::new());
            self.run_statements(body)?;
            self.scopes.pop();
        } else if let Some(alt) = or_else {
            match alt {
                AstNode::If {
                    condition,
                    body,
                    or_else,
                    ..
                } => self.run_conditional_branch(condition, body, or_else.as_deref())?,

                AstNode::Else { body, .. } => {
                    self.scopes.push(HashMap::new());
                    self.run_statements(body)?;
                    self.scopes.pop();
                }

                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn run_expression(&self, expression: &AstNode) -> Result<Value, RuntimeError> {
        match expression {
            AstNode::Eq { lhs, rhs, .. } => {
                Ok(self.run_eq(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Neq { lhs, rhs, .. } => {
                Ok(self.run_neq(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Gt { lhs, rhs, .. } => {
                Ok(self.run_gt(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Gte { lhs, rhs, .. } => {
                Ok(self.run_gte(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Lt { lhs, rhs, .. } => {
                Ok(self.run_lt(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Lte { lhs, rhs, .. } => {
                Ok(self.run_lte(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Add { lhs, rhs, .. } => {
                Ok(self.math_add(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Sub { lhs, rhs, .. } => {
                Ok(self.math_sub(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Mul { lhs, rhs, .. } => {
                Ok(self.math_mul(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }
            AstNode::Div { lhs, rhs, .. } => {
                Ok(self.math_div(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }

            AstNode::Neg { child, .. } => Ok(self.math_neg(&self.run_expression(child)?)),
            AstNode::FunctionCall { callee, args, .. } => self.run_function_call(callee, args),

            AstNode::Identifier { name, .. } => self.get_variable_value(name),
            AstNode::Literal { value, .. } => Ok(*value),

            _ => unreachable!(),
        }
    }

    fn run_eq(&self, lhs: &Value, rhs: &Value) -> Value {
        if lhs == rhs {
            Value::Boolean(true)
        } else {
            Value::Boolean(false)
        }
    }

    fn run_neq(&self, lhs: &Value, rhs: &Value) -> Value {
        if lhs != rhs {
            Value::Boolean(true)
        } else {
            Value::Boolean(false)
        }
    }

    fn run_gt(&self, lhs: &Value, rhs: &Value) -> Value {
        if lhs > rhs {
            Value::Boolean(true)
        } else {
            Value::Boolean(false)
        }
    }

    fn run_gte(&self, lhs: &Value, rhs: &Value) -> Value {
        if lhs >= rhs {
            Value::Boolean(true)
        } else {
            Value::Boolean(false)
        }
    }

    fn run_lt(&self, lhs: &Value, rhs: &Value) -> Value {
        if lhs < rhs {
            Value::Boolean(true)
        } else {
            Value::Boolean(false)
        }
    }

    fn run_lte(&self, lhs: &Value, rhs: &Value) -> Value {
        if lhs <= rhs {
            Value::Boolean(true)
        } else {
            Value::Boolean(false)
        }
    }

    fn math_add(&self, lhs: &Value, rhs: &Value) -> Value {
        match lhs {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l + r),
                Value::Float(r) => Value::Float(f64::from(*l) + r),
                _ => unreachable!(),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l + f64::from(*r)),
                Value::Float(r) => Value::Float(l + r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_sub(&self, lhs: &Value, rhs: &Value) -> Value {
        match lhs {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l - r),
                Value::Float(r) => Value::Float(f64::from(*l) - r),
                _ => unreachable!(),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l - f64::from(*r)),
                Value::Float(r) => Value::Float(l - r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_mul(&self, lhs: &Value, rhs: &Value) -> Value {
        match lhs {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l * r),
                Value::Float(r) => Value::Float(f64::from(*l) * r),
                _ => unreachable!(),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l * f64::from(*r)),
                Value::Float(r) => Value::Float(l * r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_div(&self, lhs: &Value, rhs: &Value) -> Value {
        match lhs {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Value::Integer(l / r),
                Value::Float(r) => Value::Float(f64::from(*l) / r),
                _ => unreachable!(),
            },
            Value::Float(l) => match rhs {
                Value::Integer(r) => Value::Float(l / f64::from(*r)),
                Value::Float(r) => Value::Float(l / r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_neg(&self, child: &Value) -> Value {
        match child {
            Value::Integer(n) => Value::Integer(-n),
            Value::Float(n) => Value::Float(-n),
            _ => unreachable!(),
        }
    }

    fn is_current_scope_has_variable(&self, name: &str) -> bool {
        let last_index = self.scopes.len() - 1;
        self.scopes[last_index].contains_key(name)
    }

    fn create_variable_in_current_scope(&mut self, name: &str, value: Value) {
        let last_index = self.scopes.len() - 1;
        self.scopes[last_index].insert(String::from(name), value);
    }

    fn update_variable_value(&mut self, name: &str, value: &AstNode) -> Result<(), RuntimeError> {
        let value = self.run_expression(value)?;
        let scope = self
            .scopes
            .iter_mut()
            .rev()
            .find(|scope| scope.contains_key(name))
            .unwrap();
        *scope.get_mut(name).unwrap() = value;
        Ok(())
    }

    fn get_variable_value(&self, name: &str) -> Result<Value, RuntimeError> {
        Ok(self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.contains_key(name))
            .unwrap()
            .get(name)
            .copied()
            .unwrap())
    }

    fn run_function_call(&self, callee: &AstNode, args: &[AstNode]) -> Result<Value, RuntimeError> {
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
            assert!(runtime.is_current_scope_has_variable(variable_name));
            assert_eq!(runtime.get_variable_value(variable_name).unwrap(), expected);
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
            assert!(runtime.is_current_scope_has_variable(lhs));
            assert_eq!(runtime.get_variable_value(lhs).unwrap(), expected);
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
            (
                indoc! {"
                    var x = 2048;
                    if true {
                        var x = 1024;
                        print(x);
                    }
                    print(x);
                "},
                "1024\n2048\n".as_bytes(),
            ),
            (
                indoc! {"
                    var x = 2048;
                    if true {
                        x = 1024;
                        print(x);
                    }
                    print(x);
                "},
                "1024\n1024\n".as_bytes(),
            ),
            (
                indoc! {"
                    var x = 2048;
                    if false {
                        x = 1024;
                    } else if false {
                        x = 512;
                    } else {
                        x = 256;
                    }
                    print(x);
                "},
                "256\n".as_bytes(),
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
