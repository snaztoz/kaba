use kaba::error::{Error, Result};
use kabart::runtime::{stream::RuntimeStream, Runtime};
use std::{fs, io, path::Path, process};

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
    let src = exit_on_error!(read_content(file_path));

    let compilation = kabac::compile(&src).map_err(|e| Error::CompilationError {
        path: Some(file_path),
        src: &src,
        message: e.message,
        span: e.span,
    });
    let (ast, _) = exit_on_error!(compilation);

    let mut out_stream = io::stdout();
    let mut err_stream = io::stderr();
    let streams = RuntimeStream::new(&mut out_stream, &mut err_stream);

    exit_on_error!(Runtime::new(ast, streams).run());
}

fn read_content(file_path: &Path) -> Result<String> {
    let ext = file_path.extension().and_then(|e| e.to_str());
    if !matches!(ext, Some("kaba")) {
        return Err(Error::InvalidExtension);
    }

    if !file_path.exists() {
        return Err(Error::FileNotExist { path: file_path });
    }

    Ok(fs::read_to_string(file_path).unwrap())
}
