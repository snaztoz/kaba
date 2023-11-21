use colored::Colorize;
use runtime::{Runtime, WriteStream};

mod parser;
pub mod runtime;

pub fn run<'a>(program: &str, out_stream: WriteStream<'a>, err_stream: WriteStream<'a>) {
    let ast = parser::parse(program).map_err(|e| e.to_string());

    let ast = match ast {
        Ok(mut pairs) => pairs.next().unwrap(),

        Err(e) => {
            writeln!(err_stream, "{}", e.red()).unwrap();
            return;
        }
    };

    let mut runtime = Runtime::new(program, ast, out_stream, err_stream);
    runtime.run();

    if let Some(e) = runtime.error {
        writeln!(err_stream, "{}", e.red()).unwrap();
    }
}
