use std::{cell::RefCell, collections::HashMap, io::Write};

use crate::parser::Rule;
use pest::iterators::Pair;

pub struct Runtime<'a> {
    _src: String,
    ast: Option<Pair<'a, Rule>>,
    variables: HashMap<String, i32>,
    pub error: Option<String>,

    // IO streams
    out_stream: RefCell<&'a mut dyn Write>,
    _err_stream: RefCell<&'a mut dyn Write>,
}

impl<'a> Runtime<'a> {
    pub fn new(
        src: &str,
        ast: Pair<'a, Rule>,
        out_stream: &'a mut dyn Write,
        err_stream: &'a mut dyn Write,
    ) -> Self {
        Self {
            _src: String::from(src),
            ast: Some(ast),
            variables: HashMap::new(),
            error: None,
            out_stream: RefCell::new(out_stream),
            _err_stream: RefCell::new(err_stream),
        }
    }

    pub fn run(&mut self) {
        let program = self.ast.take().unwrap().into_inner();
        for statement in program {
            match statement.as_rule() {
                Rule::Statement => {
                    let res = self.run_statement(statement.into_inner().next().unwrap());
                    if let Err(e) = res {
                        self.error = Some(e);
                        break;
                    }
                }

                Rule::EOI => break,
                _ => unreachable!(),
            }
        }
    }

    fn run_statement(&mut self, statement: Pair<'_, Rule>) -> Result<(), String> {
        match statement.as_rule() {
            Rule::VariableDeclarationStatement => {
                let mut components = statement.into_inner();
                components.next().unwrap(); // skip "var" keyword

                let identifier = components.next().unwrap().as_str();

                let expression = components.next().unwrap().into_inner().next().unwrap();
                let value = self.run_expression(expression)?;

                self.create_variable(identifier, value)
            }

            Rule::AssignmentStatement => {
                let mut components = statement.into_inner();

                let identifier = components.next().unwrap().as_str();

                let expression = components.next().unwrap().into_inner().next().unwrap();
                let value = self.run_expression(expression)?;

                self.assign_value(identifier, value)
            }

            Rule::ExpressionStatement => {
                let expression = statement
                    .into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap();
                self.run_expression(expression)?;
                Ok(())
            }

            Rule::EmptyStatement => Ok(()), // no-op

            _ => unreachable!(),
        }
    }

    fn create_variable(&mut self, identifier: &str, value: i32) -> Result<(), String> {
        if self.variables.contains_key(identifier) {
            return Err(format!("variable '{}' already exist", identifier));
        }

        self.variables.insert(String::from(identifier), value);

        Ok(())
    }

    fn assign_value(&mut self, identifier: &str, value: i32) -> Result<(), String> {
        if !self.variables.contains_key(identifier) {
            return Err(format!("variable '{}' is not exist", identifier));
        }

        *self.variables.get_mut(identifier).unwrap() = value;

        Ok(())
    }

    fn run_expression(&self, expression: Pair<'_, Rule>) -> Result<i32, String> {
        match expression.as_rule() {
            Rule::FunctionCall => {
                let mut components = expression.into_inner();

                let identifier = components.next().unwrap().as_str();

                let mut args: Vec<i32> = vec![];
                if let Some(c) = components.next() {
                    for arg in c.into_inner() {
                        let res = self.run_expression(arg.into_inner().next().unwrap())?;
                        args.push(res);
                    }
                }

                self.call_function(identifier, &args)
            }

            Rule::Identifier => self
                .variables
                .get(expression.as_str())
                .map(|n| *n)
                .ok_or_else(|| format!("variable '{}' is not exist", expression.as_str())),

            Rule::Integer => Ok(expression.as_str().parse::<i32>().unwrap()),

            _ => unreachable!(),
        }
    }

    fn call_function(&self, identifier: &str, args: &[i32]) -> Result<i32, String> {
        //
        // WARNING:
        //      Currently only support "print"
        //

        match identifier {
            "print" => {
                if args.len() != 1 {
                    return Err(format!(
                        "expecting 1 argument, but get {} instead",
                        args.len()
                    ));
                }
                writeln!(self.out_stream.borrow_mut(), "{}", args[0]).unwrap();
                Ok(0)
            }

            _ => todo!("function not yet supported"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use indoc::indoc;

    #[test]
    fn test_creating_variable() {
        let program = indoc! {"
            var x = 15;
            var y = 2048;
        "};

        let ast = parser::parse(program).unwrap().next().unwrap();

        let mut out = vec![];
        let mut err = vec![];
        let mut runtime = Runtime::new(program, ast, &mut out, &mut err);
        runtime.run();

        assert_eq!(runtime.variables.get("x"), Some(&15));
        assert_eq!(runtime.variables.get("y"), Some(&2048));
    }

    #[test]
    fn test_creating_duplicated_variable_at_same_scope() {
        let program = indoc! {"
            var x = 15;
            var x = 2048;
        "};

        let ast = parser::parse(program).unwrap().next().unwrap();

        let mut out = vec![];
        let mut err = vec![];
        let mut runtime = Runtime::new(program, ast, &mut out, &mut err);
        runtime.run();

        assert_eq!(
            runtime.error,
            Some(String::from("variable 'x' already exist"))
        );
    }

    #[test]
    fn test_assign_new_value_to_variable() {
        let program = indoc! {"
            var x = 15;
            x = 2048;
        "};

        let ast = parser::parse(program).unwrap().next().unwrap();

        let mut out = vec![];
        let mut err = vec![];
        let mut runtime = Runtime::new(program, ast, &mut out, &mut err);
        runtime.run();

        assert_eq!(runtime.variables.get("x"), Some(&2048));
    }

    #[test]
    fn test_assign_value_to_non_existing_variable() {
        let program = indoc! {"
            var x = 15;
            y = 2048;
        "};

        let ast = parser::parse(program).unwrap().next().unwrap();

        let mut out = vec![];
        let mut err = vec![];
        let mut runtime = Runtime::new(program, ast, &mut out, &mut err);
        runtime.run();

        assert_eq!(
            runtime.error,
            Some(String::from("variable 'y' is not exist"))
        );
    }

    #[test]
    fn test_running_print_statement() {
        let program = indoc! {"
            var x = 15;
            print(x);
            15;
            x;
        "};

        let ast = parser::parse(program).unwrap().next().unwrap();

        let mut out = vec![];
        let mut err = vec![];
        let mut runtime = Runtime::new(program, ast, &mut out, &mut err);
        runtime.run();

        assert!(runtime.error.is_none());
    }

    #[test]
    fn test_running_invalid_print_statement() {
        let program = indoc! {"
            var x = 15;
            print(x, x);
        "};

        let ast = parser::parse(program).unwrap().next().unwrap();

        let mut out = vec![];
        let mut err = vec![];
        let mut runtime = Runtime::new(program, ast, &mut out, &mut err);
        runtime.run();

        assert_eq!(
            runtime.error,
            Some(String::from("expecting 1 argument, but get 2 instead"))
        );
    }
}
