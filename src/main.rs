// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use std::{path::{PathBuf, Path}, fs, process, io};

use clap::{Args, Parser, Subcommand};
use kaba::runtime::Runtime;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run Kaba program.
    Run(RunArgs),
}

#[derive(Args)]
struct RunArgs {
    /// Kaba program source code. The file's extension must be `.kaba`.
    #[arg(value_parser = Validators::validate_kaba_file_path)]
    file: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run(args) => handle_run(args.file.as_path())
    }
}

fn handle_run(file: &Path) {
    let program = fs::read_to_string(file).unwrap();

    let res = compiler::compile(&program);
    if let Err(e) = res {
        eprintln!("{}", e);
        process::exit(1);
    }

    let ast = res.unwrap();
    let res = Runtime::new(&program, ast, &mut io::stdout(), &mut io::stderr()).run();
    if let Err(e) = res {
        eprintln!("{}", e);
        process::exit(1);
    }
}

struct Validators;

impl Validators {
    fn validate_kaba_file_path(path: &str) -> Result<PathBuf, String> {
        let path = PathBuf::from(path);

        if !matches!(path.extension().and_then(|e| e.to_str()), Some("kaba")) {
            return Err(String::from("source code file must have '.kaba' extension"));
        } else if !path.exists() {
            return Err(String::from("file not exist"));
        }

        Ok(path)
    }
}
