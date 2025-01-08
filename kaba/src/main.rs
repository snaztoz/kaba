use self::cmd::{run, Cmd, Commands};
use clap::Parser;

mod cmd;

fn main() {
    match Cmd::parse().command {
        Commands::Run(args) => run::handle(args.path.as_path()),
    }
}
