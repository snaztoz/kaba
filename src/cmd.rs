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
    /// Run Kaba program.
    Run(RunArgs),
}

#[derive(Args)]
pub struct RunArgs {
    /// Kaba program source code. The file's extension must be `.kaba`.
    pub file: PathBuf,
}
