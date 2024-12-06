use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;

pub mod run;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cmd {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Run a Kaba program directly without manually compiling first.
    Run(RunArgs),
}

#[derive(Args)]
pub struct RunArgs {
    /// Path to source code. The extension itself must be `.kaba`.
    pub path: PathBuf,
}
