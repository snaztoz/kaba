// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! A temporary prototype of the runtime that currently being used.
//!
//! It accept the raw AST as is and will be replaced by a real runtime
//! that operates on bytecodes.

use self::{error::RuntimeError, flags::RuntimeFlags, stream::RuntimeStream, value::RuntimeValue};
use compiler::ast::{AstNode, Program as ProgramAst};
use std::{cell::RefCell, collections::HashMap};

type Result<T> = std::result::Result<T, RuntimeError>;
type Scope = HashMap<String, RuntimeValue>;

mod error;
mod flags;
pub mod stream;
mod value;

pub struct Runtime<'a> {
    ast: Option<ProgramAst>,
    scopes: RefCell<Vec<Scope>>,
    pub error: Option<String>,

    streams: RefCell<RuntimeStream<'a>>,
    flags: RefCell<RuntimeFlags>,
}

impl<'a> Runtime<'a> {
    pub fn new(ast: ProgramAst, streams: RuntimeStream<'a>) -> Self {
        let scopes = vec![
            HashMap::new(), // built-in
            HashMap::new(), // global
        ];

        Self {
            ast: Some(ast),
            scopes: RefCell::new(scopes),
            error: None,
            streams: RefCell::new(streams),
            flags: RefCell::new(RuntimeFlags::new()),
        }
    }

    pub fn run(&self) -> Result<()> {
        let stmts = &self.ast.as_ref().unwrap().stmts;
        self.register_globals(stmts);

        let main = self.get_variable_value("main")?;
        self.run_function_ptr_call(main, &[])?;

        Ok(())
    }

    fn register_globals(&self, stmts: &[AstNode]) {
        for (i, stmt) in stmts.iter().enumerate() {
            if let AstNode::FunctionDefinition { id, .. } = stmt {
                let (id, _) = id.unwrap_identifier();
                self.create_variable(&id, RuntimeValue::Function(i));
            } else {
                unreachable!()
            }
        }
    }

    fn run_statements(&self, stmts: &[AstNode]) -> Result<RuntimeValue> {
        for stmt in stmts {
            if self.flags.borrow().stop_exec {
                return Ok(RuntimeValue::Void);
            }

            match stmt {
                AstNode::VariableDeclaration { id, val, .. } => {
                    let name = id.unwrap_identifier().0;
                    let val = match val.as_deref() {
                        Some(v) => self.run_expression(v)?,
                        None => RuntimeValue::Integer(0),
                    };
                    self.create_variable(&name, val);
                }

                AstNode::If {
                    cond,
                    body,
                    or_else,
                    ..
                } => self.run_conditional_branch(cond, body, or_else.as_deref())?,

                AstNode::While { cond, body, .. } => self.run_while(cond, body)?,

                AstNode::Break { .. } => {
                    self.flags.borrow_mut().stop_exec = true;
                    self.flags.borrow_mut().exit_loop = true;
                }

                AstNode::Continue { .. } => {
                    self.flags.borrow_mut().stop_exec = true;
                }

                AstNode::Return { expr, .. } => {
                    if let Some(expr) = expr {
                        let val = self.run_expression(expr)?;
                        return Ok(val);
                    } else {
                        return Ok(RuntimeValue::Void);
                    }
                }

                AstNode::Debug { expr, .. } => self.run_debug_statement(expr)?,

                node => {
                    self.run_expression(node)?;
                }
            };
        }

        Ok(RuntimeValue::Void)
    }

    fn assign(&self, lhs: &AstNode, rhs: &AstNode) -> Result<RuntimeValue> {
        match lhs {
            AstNode::Identifier { name, .. } => {
                let val = self.run_expression(rhs)?;
                self.update_variable_value(name, val)?;
            }
            _ => todo!("more expression for value assignment"),
        }
        Ok(RuntimeValue::Void)
    }

    fn add_assign(&self, lhs: &AstNode, rhs: &AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.get_variable_value(&name)?;
        let val = self.run_expression(rhs)?;
        let new_val = self.math_add(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.update_variable_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn sub_assign(&self, lhs: &AstNode, rhs: &AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.get_variable_value(&name)?;
        let val = self.run_expression(rhs)?;
        let new_val = self.math_sub(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.update_variable_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn mul_assign(&self, lhs: &AstNode, rhs: &AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.get_variable_value(&name)?;
        let val = self.run_expression(rhs)?;
        let new_val = self.math_mul(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.update_variable_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn div_assign(&self, lhs: &AstNode, rhs: &AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.get_variable_value(&name)?;
        let val = self.run_expression(rhs)?;
        let new_val = self.math_div(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.update_variable_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn mod_assign(&self, lhs: &AstNode, rhs: &AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.get_variable_value(&name)?;
        let val = self.run_expression(rhs)?;
        let new_val = self.math_mod(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.update_variable_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn run_conditional_branch(
        &self,
        cond: &AstNode,
        body: &[AstNode],
        or_else: Option<&AstNode>,
    ) -> Result<()> {
        let should_exec = match self.run_expression(cond)? {
            RuntimeValue::Boolean(b) => b,
            _ => unreachable!(),
        };

        if should_exec {
            self.scopes.borrow_mut().push(HashMap::new());
            self.run_statements(body)?;
            self.scopes.borrow_mut().pop();
        } else if let Some(alt) = or_else {
            match alt {
                AstNode::If {
                    cond,
                    body,
                    or_else,
                    ..
                } => self.run_conditional_branch(cond, body, or_else.as_deref())?,

                AstNode::Else { body, .. } => {
                    self.scopes.borrow_mut().push(HashMap::new());
                    self.run_statements(body)?;
                    self.scopes.borrow_mut().pop();
                }

                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn run_while(&self, cond: &AstNode, body: &[AstNode]) -> Result<()> {
        loop {
            let should_exec = match self.run_expression(cond)? {
                RuntimeValue::Boolean(b) => b,
                _ => unreachable!(),
            };

            if should_exec {
                self.scopes.borrow_mut().push(HashMap::new());
                self.run_statements(body)?;
                self.scopes.borrow_mut().pop();

                if self.flags.borrow().stop_exec {
                    self.flags.borrow_mut().stop_exec = false;
                }

                if self.flags.borrow().exit_loop {
                    self.flags.borrow_mut().exit_loop = false;
                    break;
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn run_debug_statement(&self, expr: &AstNode) -> Result<()> {
        let val = self.run_expression(expr)?;
        writeln!(self.streams.borrow_mut().out_stream, "{}", val).unwrap();
        Ok(())
    }

    fn run_expression(&self, expr: &AstNode) -> Result<RuntimeValue> {
        match expr {
            AstNode::Assign { lhs, rhs, .. } => self.assign(lhs, rhs),

            AstNode::AddAssign { lhs, rhs, .. } => self.add_assign(lhs, rhs),
            AstNode::SubAssign { lhs, rhs, .. } => self.sub_assign(lhs, rhs),
            AstNode::MulAssign { lhs, rhs, .. } => self.mul_assign(lhs, rhs),
            AstNode::DivAssign { lhs, rhs, .. } => self.div_assign(lhs, rhs),
            AstNode::ModAssign { lhs, rhs, .. } => self.mod_assign(lhs, rhs),

            AstNode::Or { lhs, rhs, .. } => Ok(self.run_or(lhs, rhs)),
            AstNode::And { lhs, rhs, .. } => Ok(self.run_and(lhs, rhs)),
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
            AstNode::Mod { lhs, rhs, .. } => {
                Ok(self.math_mod(&self.run_expression(lhs)?, &self.run_expression(rhs)?))
            }

            AstNode::Not { child, .. } => Ok(self.run_not(&self.run_expression(child)?)),
            AstNode::Neg { child, .. } => Ok(self.math_neg(&self.run_expression(child)?)),
            AstNode::FunctionCall { callee, args, .. } => self.run_function_call(callee, args),

            AstNode::Identifier { name, .. } => self.get_variable_value(name),
            AstNode::Literal { value, .. } => Ok(RuntimeValue::from(*value)),

            _ => unreachable!(),
        }
    }

    fn run_or(&self, lhs: &AstNode, rhs: &AstNode) -> RuntimeValue {
        // Use short-circuiting

        if let Ok(RuntimeValue::Boolean(b)) = self.run_expression(lhs) {
            if b {
                return RuntimeValue::Boolean(true);
            }
        }
        if let Ok(RuntimeValue::Boolean(b)) = self.run_expression(rhs) {
            if b {
                return RuntimeValue::Boolean(true);
            }
        }

        RuntimeValue::Boolean(false)
    }

    fn run_and(&self, lhs: &AstNode, rhs: &AstNode) -> RuntimeValue {
        // Use short-circuiting

        if let Ok(RuntimeValue::Boolean(b_lhs)) = self.run_expression(lhs) {
            if b_lhs {
                if let Ok(RuntimeValue::Boolean(b_rhs)) = self.run_expression(rhs) {
                    if b_rhs {
                        return RuntimeValue::Boolean(true);
                    }
                }
            }
        }

        RuntimeValue::Boolean(false)
    }

    fn run_eq(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        if lhs == rhs {
            RuntimeValue::Boolean(true)
        } else {
            RuntimeValue::Boolean(false)
        }
    }

    fn run_neq(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        if lhs != rhs {
            RuntimeValue::Boolean(true)
        } else {
            RuntimeValue::Boolean(false)
        }
    }

    fn run_gt(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        if lhs > rhs {
            RuntimeValue::Boolean(true)
        } else {
            RuntimeValue::Boolean(false)
        }
    }

    fn run_gte(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        if lhs >= rhs {
            RuntimeValue::Boolean(true)
        } else {
            RuntimeValue::Boolean(false)
        }
    }

    fn run_lt(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        if lhs < rhs {
            RuntimeValue::Boolean(true)
        } else {
            RuntimeValue::Boolean(false)
        }
    }

    fn run_lte(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        if lhs <= rhs {
            RuntimeValue::Boolean(true)
        } else {
            RuntimeValue::Boolean(false)
        }
    }

    fn math_add(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l + r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) + r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l + f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l + r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_sub(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l - r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) - r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l - f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l - r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_mul(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l * r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) * r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l * f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l * r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_div(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l / r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) / r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l / f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l / r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_mod(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l % r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) % r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l % f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l % r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn run_not(&self, child: &RuntimeValue) -> RuntimeValue {
        match child {
            RuntimeValue::Boolean(b) => RuntimeValue::Boolean(!b),
            _ => unreachable!(),
        }
    }

    fn math_neg(&self, child: &RuntimeValue) -> RuntimeValue {
        match child {
            RuntimeValue::Integer(n) => RuntimeValue::Integer(-n),
            RuntimeValue::Float(n) => RuntimeValue::Float(-n),
            _ => unreachable!(),
        }
    }

    fn create_variable(&self, name: &str, val: RuntimeValue) {
        let last_i = self.scopes.borrow().len() - 1;
        self.scopes.borrow_mut()[last_i].insert(String::from(name), val);
    }

    fn update_variable_value(&self, name: &str, val: RuntimeValue) -> Result<()> {
        let mut scopes = self.scopes.borrow_mut();
        let scope = scopes
            .iter_mut()
            .rev()
            .find(|scope| scope.contains_key(name))
            .unwrap();
        *scope.get_mut(name).unwrap() = val;
        Ok(())
    }

    fn get_variable_value(&self, name: &str) -> Result<RuntimeValue> {
        Ok(self
            .scopes
            .borrow()
            .iter()
            .rev()
            .find(|scope| scope.contains_key(name))
            .unwrap()
            .get(name)
            .copied()
            .unwrap())
    }

    fn run_function_call(&self, callee: &AstNode, args: &[AstNode]) -> Result<RuntimeValue> {
        let callee = match callee {
            AstNode::Identifier { name, .. } => name,
            _ => todo!("function callee"),
        };

        let mut evaluated_args: Vec<RuntimeValue> = vec![];
        for arg in args {
            evaluated_args.push(self.run_expression(arg)?);
        }

        let f_ptr = self.get_variable_value(callee).unwrap();

        self.run_function_ptr_call(f_ptr, &evaluated_args)
    }

    fn run_function_ptr_call(
        &self,
        f_ptr: RuntimeValue,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue> {
        if let RuntimeValue::Function(ptr) = f_ptr {
            self.scopes.borrow_mut().push(HashMap::new());

            if let AstNode::FunctionDefinition { params, body, .. } =
                self.ast.as_ref().unwrap().stmts.get(ptr).unwrap()
            {
                for (i, (id, _)) in params.iter().enumerate() {
                    let (id, _) = id.unwrap_identifier();
                    let val = args[i];
                    self.create_variable(&id, val);
                }

                let val = self.run_statements(body)?;
                self.scopes.borrow_mut().pop();
                return Ok(val);
            }
        }

        unreachable!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::Compiler;
    use indoc::indoc;

    fn assert_output_equal(input: &str, expect: &[u8]) {
        let mut out_stream = vec![];
        let mut err_stream = vec![];

        let ast = Compiler::from_source_code(input).compile().unwrap();

        let streams = RuntimeStream::new(&mut out_stream, &mut err_stream);
        let runtime = Runtime::new(ast, streams);
        let result = runtime.run();

        assert!(result.is_ok());
        assert_eq!(expect, &out_stream);
    }

    #[test]
    fn test_simple_outputting() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 10;
                    debug x;
                end
            "},
            "10\n".as_bytes(),
        );
    }

    #[test]
    fn test_multiplication_result() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 5;
                    var y = 10;

                    debug x * y;
                end
            "},
            "50\n".as_bytes(),
        );
    }

    #[test]
    fn test_changing_value() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 2048;
                    debug x;

                    x = 1024;
                    debug x;
                end
            "},
            "2048\n1024\n".as_bytes(),
        );
    }

    #[test]
    fn test_conditional_branch() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 2048;
                    if true do
                        var x = 1024;
                        debug x;
                    end
                    debug x;
                end
            "},
            "1024\n2048\n".as_bytes(),
        );
    }

    #[test]
    fn test_multiple_conditional_branches() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 2048;
                    if false do
                        x = 1024;
                    else if false do
                        x = 512;
                    else do
                        x = 256;
                    end
                    debug x;
                end
            "},
            "256\n".as_bytes(),
        );
    }

    #[test]
    fn test_loop() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 0;
                    while true do
                        debug x;
                        if x == 5 do
                            break;
                        end
                        x = x + 1;
                    end
                end
            "},
            "0\n1\n2\n3\n4\n5\n".as_bytes(),
        );
    }

    #[test]
    fn test_boolean_logic_operators() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    if false && true do
                        debug 1;
                    end
                    if false || true do
                        debug 2;
                    end
                    if !false do
                        debug 3;
                    end
                end
            "},
            "2\n3\n".as_bytes(),
        );
    }

    #[test]
    fn test_loop_with_conditional_branches() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 0;
                    while true do
                        x = x + 1;
                        if x == 2 do
                            continue;
                        else if x == 5 do
                            break;
                        end
                        debug x;
                    end
                end
            "},
            "1\n3\n4\n".as_bytes(),
        );
    }

    #[test]
    fn test_shorthand_operators() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 5;
                    x += 5;
                    debug x;
                    x -= 2;
                    debug x;
                    x *= 2;
                    debug x;
                    x /= 4;
                    debug x;
                    x %= 3;
                    debug x;
                end
            "},
            "10\n8\n16\n4\n1\n".as_bytes(),
        );
    }

    #[test]
    fn test_function_call() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    debug add_two(5);
                end

                fn add_two(n: Int): Int do
                    return n + get_two();
                end

                fn get_two(): Int do
                    return 2;
                end
            "},
            "7\n".as_bytes(),
        );
    }

    // #[test]
    // fn test_recursion() {
    //     assert_output_equal(
    //         indoc! {"
    //             fn main() do
    //                 debug fibonacci(3);
    //             end

    //             fn fibonacci(n: Int): Int do
    //                 if n == 1 || n == 2 do
    //                     return 1;
    //                 end
    //                 return fibonacci(n-1) + fibonacci(n-2);
    //             end
    //         "},
    //         "3\n".as_bytes(),
    //     );
    // }
}
