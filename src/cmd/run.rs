use compiler::Compiler;
use kaba::runtime::{stream::RuntimeStream, Runtime};
use std::{io, path::Path, process};

macro_rules! exit_on_error {
    ($expression:expr) => {
        match $expression {
            Ok(value) => value,
            Err(err) => {
                eprintln!("{err}");
                process::exit(1);
            }
        }
    };
}

pub fn handle(file_path: &Path) {
    let mut compiler = exit_on_error!(Compiler::try_from(file_path));
    let ast = exit_on_error!(compiler.compile());

    let mut out_stream = io::stdout();
    let mut err_stream = io::stderr();
    let streams = RuntimeStream::new(&mut out_stream, &mut err_stream);

    exit_on_error!(Runtime::new(ast, streams).run());
}
