use colored::Colorize;
use kaba::runtime::WriteStream;
use std::{
    env, fmt, fs,
    io::{self, ErrorKind},
    path::PathBuf,
    process,
};

fn main() {
    let args: Vec<String> = env::args().collect();

    let res = _main(&args, &mut io::stdout(), &mut io::stderr());
    if let Err(e) = res {
        eprintln!("{} {}\n", "ERR:".red(), e);
        process::exit(1);
    }
}

fn _main(
    args: &[String],
    output_stream: WriteStream<'_>,
    error_stream: WriteStream<'_>,
) -> Result<(), CliError> {
    if args.len() != 2 {
        return Err(CliError::NoInputFile);
    } else if !args[1].ends_with(".kaba") {
        return Err(CliError::WrongFileExtension);
    }

    let program = fs::read_to_string(&args[1]);
    match &program {
        Ok(_) => (),

        Err(e) if e.kind() == ErrorKind::NotFound => {
            return Err(CliError::FileNotFound(PathBuf::from(&args[1])));
        }

        _ => unimplemented!(),
    }

    let result = kaba::run(&program.unwrap(), output_stream, error_stream);
    if let Err(e) = result {
        writeln!(output_stream, "{} {}", "ERR:".red(), e.red()).unwrap();
    } else {
        writeln!(output_stream).unwrap(); // print empty line
    }

    Ok(())
}

#[derive(Debug, PartialEq)]
pub enum CliError {
    FileNotFound(PathBuf),
    NoInputFile,
    WrongFileExtension,
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FileNotFound(p) => write!(
                f,
                "{} {}",
                "File not found:".red(),
                p.as_os_str().to_str().unwrap().red()
            ),
            Self::NoInputFile => write!(
                f,
                "{}",
                "No input file supplied. Usage: `kaba <filename>`".red()
            ),
            Self::WrongFileExtension => write!(
                f,
                "{}",
                "Kaba program file must have `.kaba` extension".red()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
        let args = build_args(&["./tests/data/swap_variables.kaba"]);
        let res = _main(&args, &mut vec![], &mut vec![]);

        assert!(res.is_ok());
    }

    #[test]
    fn test_run_without_input_file() {
        let args = build_args(&[]);
        let res = _main(&args, &mut vec![], &mut vec![]);

        assert_eq!(res, Err(CliError::NoInputFile));
    }

    #[test]
    fn test_run_with_invalid_input_file_extension() {
        let args = build_args(&["invalid.k"]);
        let res = _main(&args, &mut vec![], &mut vec![]);

        assert_eq!(res, Err(CliError::WrongFileExtension));
    }

    #[test]
    fn test_run_with_non_existing_input_file() {
        let args = build_args(&["not-exist.kaba"]);
        let res = _main(&args, &mut vec![], &mut vec![]);

        assert_eq!(
            res,
            Err(CliError::FileNotFound(PathBuf::from("not-exist.kaba")))
        );
    }

    fn build_args(args: &[&str]) -> Vec<String> {
        let mut v = vec![String::from("kaba")];
        for &a in args {
            v.push(String::from(a));
        }
        v
    }
}
