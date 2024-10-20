use clap::Parser;
use kaba::{
    cli::{Cli, Commands},
    runtime::{stream::RuntimeStream, Runtime},
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
        eprintln!("{e}");
        process::exit(1);
    }

    let ast = res.unwrap();

    let mut out_stream = io::stdout();
    let mut err_stream = io::stderr();
    let streams = RuntimeStream::new(&mut out_stream, &mut err_stream);

    let res = Runtime::new(ast, streams).run();
    if let Err(e) = res {
        eprintln!("{}", e);
        process::exit(1);
    }
}
