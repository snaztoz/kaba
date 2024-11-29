use kaba::runtime::{stream::RuntimeStream, Runtime};
use std::{io, path::Path, process};

pub fn handle(file_path: &Path) {
    let res = compiler::compile(file_path);
    if let Err(e) = res {
        eprintln!("{e}");
        process::exit(1);
    }

    let ast = res.unwrap();

    let mut out_stream = io::stdout();
    let mut err_stream = io::stderr();
    let streams = RuntimeStream::new(&mut out_stream, &mut err_stream);

    let res = Runtime::new(ast, streams).run();
    if let Err(e) = res {
        eprintln!("{e}");
        process::exit(1);
    }
}
