use colored::Colorize;
use runtime::{Runtime, WriteStream};

mod parser;
pub mod runtime;

pub fn run<'a>(program: &str, out_stream: WriteStream<'a>, err_stream: WriteStream<'a>) {
    let ast = parser::parse(program).map_err(|e| e.to_string());

    let ast = match ast {
        Ok(mut pairs) => pairs.next().unwrap(),

        Err(e) => {
            eprintln!("{}", e.red());
            return;
        }
    };

    let mut runtime = Runtime::new(program, ast, out_stream, err_stream);
    runtime.run();

    if let Some(e) = runtime.error {
        eprintln!("{} {}", "ERR:".red(), e.red());
    }
}
