use self::cmd::{run, Cmd, Commands};
use clap::Parser;

mod cmd;

fn main() {
    let cli = Cmd::parse();

    match &cli.command {
        Commands::Run(args) => run::handle(args.file.as_path()),
    }
}
