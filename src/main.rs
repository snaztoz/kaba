use colored::Colorize;
use std::{
    env, fmt, fs,
    io::{self, ErrorKind, Write},
    path::PathBuf,
    process,
};

fn main() {
    let args: Vec<String> = env::args().collect();

    let res = _main(&args, &mut io::stdout(), &mut io::stderr());
    if let Err(e) = res {
        e.print();
        process::exit(1);
    }
}

fn _main(
    args: &[String],
    out_stream: &mut dyn Write,
    err_stream: &mut dyn Write,
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

    kaba::run(&program.unwrap(), out_stream, err_stream);
    writeln!(out_stream).unwrap(); // print empty line

    Ok(())
}

#[derive(Debug, PartialEq)]
pub enum CliError {
    FileNotFound(PathBuf),
    NoInputFile,
    WrongFileExtension,
}

impl CliError {
    fn print(&self) {
        eprintln!("{} {}\n", "ERR:".red(), self);
    }
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
