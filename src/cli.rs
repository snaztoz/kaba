use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
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
    #[arg(value_parser = Validators::validate_kaba_file_path)]
    pub file: PathBuf,
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
