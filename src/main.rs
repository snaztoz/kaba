// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use clap::Parser;
use colored::Colorize;
use kaba::{
    cli::{Cli, Commands},
    runtime::Runtime,
};
use std::{io, path::Path, process};

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run(args) => handle_run(args.file.as_path()),
    }
}

fn handle_run(file_path: &Path) {
    let res = compiler::compile(file_path);
    if let Err(e) = res {
        eprintln!("{} {}", "error:".bright_red().bold(), e);
        process::exit(1);
    }

    let ast = res.unwrap();
    let res = Runtime::new(ast, &mut io::stdout(), &mut io::stderr()).run();
    if let Err(e) = res {
        eprintln!("{}", e);
        process::exit(1);
    }
}
