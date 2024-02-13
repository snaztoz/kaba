// Copyright 2023-2024 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

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
    pub file: PathBuf,
}
