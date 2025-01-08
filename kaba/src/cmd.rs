use clap::{Args, Parser, Subcommand};
use indoc::indoc;
use std::path::PathBuf;

pub mod run;

#[derive(Parser)]
#[command(
    author,
    version,
    about,
    long_about = indoc!{"
        Kaba is a strong and statically-typed programming language.

        This binary (`kaba`) is used to manage tools related to the language. At
        the moment, it contains:

          1. The compiler, and
          2. The runtime (prototype)"
    },
)]
pub struct Cmd {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Compile and run Kaba program.
    Run(RunArgs),
}

#[derive(Args)]
pub struct RunArgs {
    /// Path to the source code (must have `.kaba` extension).
    pub path: PathBuf,
}
