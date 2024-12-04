//! A temporary prototype of the runtime that currently being used.
//!
//! It accept the raw AST as is and should be replaced by a real runtime that
//! operates on bytecodes (TODO).

use self::{error::Result, state::RuntimeState, stream::RuntimeStream, value::RuntimeValue};
use compiler::ast::AstNode;
use expression::ExpressionRunner;

mod assignment;
mod body;
mod error;
mod expression;
mod state;
mod statement;
pub mod stream;
mod value;

pub struct Runtime<'a> {
    ast: Option<AstNode>,
    state: RuntimeState<'a>,
}

impl<'a> Runtime<'a> {
    pub fn new(ast: AstNode, streams: RuntimeStream<'a>) -> Self {
        Self {
            ast: Some(ast),
            state: RuntimeState::new(streams),
        }
    }

    pub fn run(&'a self) -> Result<()> {
        let body = if let AstNode::Program { body } = &self.ast.as_ref().unwrap() {
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
            if let AstNode::FunctionDefinition { id, .. } = stmt {
                let (id, _) = id.unwrap_identifier();
                self.state.store_value(&id, RuntimeValue::Function(i));
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
    use compiler::Compiler;
    use indoc::indoc;

    fn assert_output_equal(input: &str, expect: &[u8]) {
        let mut out_stream = vec![];
        let mut err_stream = vec![];

        let ast = Compiler::from(input).compile().unwrap();

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
                fn main() do
                    var x = 10;
                    debug x;
                end
            "},
            "10\n".as_bytes(),
        );
    }

    #[test]
    fn multiplication_result() {
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
    fn changing_value() {
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
    fn conditional_branch() {
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
    fn multiple_conditional_branches() {
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
    fn simple_loop() {
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
    fn boolean_logic_operators() {
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
    fn loop_with_conditional_branches() {
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
    fn shorthand_operators() {
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
    fn function_call() {
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

    #[test]
    fn calling_function_with_variable_argument() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var x = 10;
                    dbg(x);
                end

                fn dbg(n: Int) do
                    debug n;
                end
            "},
            "10\n".as_bytes(),
        );
    }

    #[test]
    fn doing_math_on_function_call_results() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    debug one() + two();
                end

                fn one(): Int do
                    return 1;
                end

                fn two(): Int do
                    return 2;
                end
            "},
            "3\n".as_bytes(),
        );
    }

    #[test]
    fn recursion() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    debug fibonacci(3);
                end

                fn fibonacci(n: Int): Int do
                    if n == 1 || n == 2 do
                        return 1;
                    end
                    return fibonacci(n-1) + fibonacci(n-2);
                end
            "},
            "2\n".as_bytes(),
        );
    }

    #[test]
    fn recursive_counter() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    count_to_zero(5);
                end

                fn count_to_zero(n: Int) do
                    if n < 0 do
                        return;
                    end
                    debug n;
                    count_to_zero(n-1);
                end
            "},
            "5\n4\n3\n2\n1\n0\n".as_bytes(),
        );
    }

    #[test]
    fn function_as_argument_to_function_call() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    print(produce);
                end

                fn print(producer: () -> Int) do
                    var x: () -> Int = producer;
                    debug x();

                    var y = producer;
                    debug y();
                end

                fn produce(): Int do
                    return 5;
                end
            "},
            "5\n5\n".as_bytes(),
        );
    }

    #[test]
    fn calling_function_returned_from_another_function_call() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    debug foo()();
                end

                fn foo(): () -> Int do
                    return bar;
                end

                fn bar(): Int do
                    return 25;
                end
            "},
            "25\n".as_bytes(),
        );
    }

    #[test]
    fn debug_array() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    debug [[1, 2], [3, 4]][1][0];

                    var arr = [1, 3];
                    var x = 98;
                    debug arr[99 - x] + 5;
                end
            "},
            "3\n8\n".as_bytes(),
        );
    }

    #[test]
    fn assign_to_array() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var arr = [0, 1, 2];

                    arr[0] = 99;

                    arr[1] += 5;
                    arr[1] -= 2;
                    arr[2] *= 3;
                    arr[2] /= 3;
                    arr[0] %= 3;

                    debug arr[0];
                    debug arr[1];
                    debug arr[2];
                end
            "},
            "0\n4\n2\n".as_bytes(),
        );
    }

    #[test]
    fn calling_array_elements() {
        assert_output_equal(
            indoc! {"
                fn main() do
                    var arr = [
                        add_one,
                        add_two,
                        add_three,
                    ];

                    var i = 0;
                    while i < 3 do
                        debug arr[i](5);
                        i += 1;
                    end
                end

                fn add_one(n: Int): Int do
                    return n + 1;
                end

                fn add_two(n: Int): Int do
                    return n + 2;
                end

                fn add_three(n: Int): Int do
                    return n + 3;
                end
            "},
            "6\n7\n8\n".as_bytes(),
        );
    }
}
