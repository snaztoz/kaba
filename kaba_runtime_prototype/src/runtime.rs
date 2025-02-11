//! A temporary prototype of the runtime that currently being used.
//!
//! It accept the raw AST as is and should be replaced by a real runtime that
//! operates on bytecodes (TODO).

use self::{error::Result, state::RuntimeState, stream::RuntimeStream, value::RuntimeValue};
use expression::ExpressionRunner;
use kaba_compiler::{AstNode, AstNodeVariant};

mod assignment;
mod body;
mod error;
mod expression;
mod state;
mod statement;
pub mod stream;
mod value;

pub struct Runtime<'src, 'a> {
    ast: Option<AstNode<'src>>,
    state: RuntimeState<'a>,
}

impl<'src, 'a> Runtime<'src, 'a> {
    pub fn new(ast: AstNode<'src>, streams: RuntimeStream<'a>) -> Self {
        Self {
            ast: Some(ast),
            state: RuntimeState::new(streams),
        }
    }

    pub fn run(&'a self) -> Result<()> {
        let body = if let AstNodeVariant::Program { body, .. } = &self.ast.as_ref().unwrap().variant
        {
            body
        } else {
            unreachable!()
        };

        self.register_globals(body);
        self.run_main()?;

        Ok(())
    }

    fn register_globals(&self, stmts: &[AstNode]) {
        for (i, stmt) in stmts.iter().enumerate() {
            if let AstNodeVariant::FunctionDefinition { sym, .. } = &stmt.variant {
                let sym = sym.variant.unwrap_symbol();
                self.state.store_value(sym, RuntimeValue::Function(i));
            } else {
                unreachable!()
            }
        }
    }

    fn run_main(&'a self) -> Result<RuntimeValue> {
        let ast = self.ast.as_ref().unwrap();
        let runner = ExpressionRunner::new(ast, ast, &self.state);
        runner.run_function_ptr_call(self.state.get_value("main")?, &[])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use kaba_compiler;

    fn assert_output_equal(input: &str, expect: &[u8]) {
        let mut out_stream = vec![];
        let mut err_stream = vec![];

        let (ast, _) = kaba_compiler::compile(input).unwrap();

        let streams = RuntimeStream::new(&mut out_stream, &mut err_stream);
        let runtime = Runtime::new(ast, streams);
        let result = runtime.run();

        assert!(result.is_ok());
        assert_eq!(&out_stream, expect);
    }

    #[test]
    fn simple_outputting() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 10;
                    debug x;
                }
            "},
            "10\n".as_bytes(),
        );
    }

    #[test]
    fn multiplication_result() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 5;
                    var y = 10;

                    debug x * y;
                }
            "},
            "50\n".as_bytes(),
        );
    }

    #[test]
    fn overflowing_math() {
        assert_output_equal(
            indoc! {"
                def main {
                    debug 2147483647 + 1;

                    var a = 2147483647;
                    debug a + 2;

                    var b = -2147483648;
                    debug b - 1;

                    var c = -2147483648;
                    debug c * -1;

                    var d = -2147483648;
                    debug d / -1;
                }
            "},
            "-2147483648\n-2147483647\n2147483647\n-2147483648\n-2147483648\n".as_bytes(),
        );
    }

    #[test]
    fn changing_value() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 2048;
                    debug x;

                    x = 1024;
                    debug x;
                }
            "},
            "2048\n1024\n".as_bytes(),
        );
    }

    #[test]
    fn conditional_branch() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 2048;
                    if true {
                        var x = 1024;
                        debug x;
                    }
                    debug x;
                }
            "},
            "1024\n2048\n".as_bytes(),
        );
    }

    #[test]
    fn multiple_conditional_branches() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 2048;
                    if false {
                        x = 1024;
                    } else if false {
                        x = 512;
                    } else {
                        x = 256;
                    }
                    debug x;
                }
            "},
            "256\n".as_bytes(),
        );
    }

    #[test]
    fn simple_loop() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 0;
                    while true {
                        debug x;
                        if x == 5 {
                            break;
                        }
                        x = x + 1;
                    }
                }
            "},
            "0\n1\n2\n3\n4\n5\n".as_bytes(),
        );
    }

    #[test]
    fn boolean_logic_operators() {
        assert_output_equal(
            indoc! {"
                def main {
                    if false && true {
                        debug 1;
                    }
                    if false || true {
                        debug 2;
                    }
                    if !false {
                        debug 3;
                    }
                }
            "},
            "2\n3\n".as_bytes(),
        );
    }

    #[test]
    fn loop_with_conditional_branches() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 0;
                    while true {
                        x = x + 1;
                        if x == 2 {
                            continue;
                        } else if x == 5 {
                            break;
                        }
                        debug x;
                    }
                }
            "},
            "1\n3\n4\n".as_bytes(),
        );
    }

    #[test]
    fn shorthand_operators() {
        assert_output_equal(
            indoc! {"
                def main {
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
                }
            "},
            "10\n8\n16\n4\n1\n".as_bytes(),
        );
    }

    #[test]
    fn function_call() {
        assert_output_equal(
            indoc! {"
                def main {
                    debug add_two(5);
                }

                def add_two(n: int): int {
                    return n + get_two();
                }

                def get_two: int {
                    return 2;
                }
            "},
            "7\n".as_bytes(),
        );
    }

    #[test]
    fn calling_function_with_variable_argument() {
        assert_output_equal(
            indoc! {"
                def main {
                    var x = 10;
                    dbg(x);
                }

                def dbg(n: int) {
                    debug n;
                }
            "},
            "10\n".as_bytes(),
        );
    }

    #[test]
    fn doing_math_on_function_call_results() {
        assert_output_equal(
            indoc! {"
                def main {
                    debug one() + two();
                }

                def one: int {
                    return 1;
                }

                def two: int {
                    return 2;
                }
            "},
            "3\n".as_bytes(),
        );
    }

    #[test]
    fn function_accept_string_parameter_and_return_value() {
        assert_output_equal(
            indoc! {r#"
                def main {
                    debug greet("snaztoz");
                }

                def greet(name: string): string {
                    debug "Hello";
                    debug name;
                    return name;
                }
            "#},
            "Hello\nsnaztoz\nsnaztoz\n".as_bytes(),
        );
    }

    #[test]
    fn recursion() {
        assert_output_equal(
            indoc! {"
                def main {
                    debug fibonacci(3);
                }

                def fibonacci(n: int): int {
                    if n == 1 || n == 2 {
                        return 1;
                    }
                    return fibonacci(n-1) + fibonacci(n-2);
                }
            "},
            "2\n".as_bytes(),
        );
    }

    #[test]
    fn recursive_counter() {
        assert_output_equal(
            indoc! {"
                def main {
                    count_to_zero(5);
                }

                def count_to_zero(n: int) {
                    if n < 0 {
                        return;
                    }
                    debug n;
                    count_to_zero(n-1);
                }
            "},
            "5\n4\n3\n2\n1\n0\n".as_bytes(),
        );
    }

    #[test]
    fn function_as_argument_to_function_call() {
        assert_output_equal(
            indoc! {"
                def main {
                    print(produce);
                }

                def print(producer: () -> int) {
                    var x: () -> int = producer;
                    debug x();

                    var y = producer;
                    debug y();
                }

                def produce: int {
                    return 5;
                }
            "},
            "5\n5\n".as_bytes(),
        );
    }

    #[test]
    fn calling_function_returned_from_another_function_call() {
        assert_output_equal(
            indoc! {"
                def main {
                    debug foo()();
                }

                def foo: () -> int {
                    return bar;
                }

                def bar: int {
                    return 25;
                }
            "},
            "25\n".as_bytes(),
        );
    }

    #[test]
    fn debug_array() {
        assert_output_equal(
            indoc! {"
                def main {
                    debug [[]int [int 1, 2], [int 3, 4]][1][0];

                    var arr = [int 1, 3];
                    var x = 98;
                    debug arr[99 - x] + 5;
                }
            "},
            "3\n8\n".as_bytes(),
        );
    }

    #[test]
    fn assign_to_array() {
        assert_output_equal(
            indoc! {"
                def main {
                    var arr = [int 0, 1, 2];

                    arr[0] = 99;

                    arr[1] += 5;
                    arr[1] -= 2;
                    arr[2] *= 3;
                    arr[2] /= 3;
                    arr[0] %= 3;

                    debug arr[0];
                    debug arr[1];
                    debug arr[2];
                }
            "},
            "0\n4\n2\n".as_bytes(),
        );
    }

    #[test]
    fn calling_array_elements() {
        assert_output_equal(
            indoc! {"
                def main {
                    var arr = [(int) -> int
                        add_one,
                        add_two,
                        add_three,
                    ];

                    var i = 0;
                    while i < 3 {
                        debug arr[i](5);
                        i += 1;
                    }
                }

                def add_one(n: int): int {
                    return n + 1;
                }

                def add_two(n: int): int {
                    return n + 2;
                }

                def add_three(n: int): int {
                    return n + 3;
                }
            "},
            "6\n7\n8\n".as_bytes(),
        );
    }

    #[test]
    fn returning_array_from_a_function() {
        assert_output_equal(
            indoc! {"
                def main {
                    var arr_1 = foo();
                    var arr_2 = foo();

                    debug arr_1[0];
                    debug arr_2[0];

                    arr_1[0] = 10;

                    debug arr_1[0];
                    debug arr_2[0];
                }

                def foo: []int {
                    return [int 0];
                }
            "},
            "0\n0\n10\n0\n".as_bytes(),
        );
    }

    #[test]
    fn iterate_array_using_each_loop_statement() {
        assert_output_equal(
            indoc! {"
                def main {
                    each n in [int 1, 2, 3, 4, 5, 6] {
                        if n == 3 {
                            continue;
                        }

                        if n == 5 {
                            break;
                        }

                        debug n * 2;
                    }
                }
            "},
            "2\n4\n8\n".as_bytes(),
        );
    }
}
