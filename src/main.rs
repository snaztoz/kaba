// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use clap::Parser;
use colored::Colorize;
use kaba::{
    cli::{Cli, Commands},
    runtime::Runtime,
};
use std::{fs, io, path::Path, process};

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run(args) => handle_run(args.file.as_path()),
    }
}

fn handle_run(file: &Path) {
    let program = fs::read_to_string(file).unwrap();

    let res = compiler::compile(&program);
    if let Err(e) = res {
        eprint!(
            "{} {}",
            "error:".bright_red().bold(),
            e.get_verbose_message(&file.display().to_string(), &program)
        );
        process::exit(1);
    }

    let ast = res.unwrap();
    let res = Runtime::new(&program, ast, &mut io::stdout(), &mut io::stderr()).run();
    if let Err(e) = res {
        eprintln!("{}", e);
        process::exit(1);
    }
}
