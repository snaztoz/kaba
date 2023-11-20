use colored::Colorize;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!(
            "{}",
            "ERR: No input file supplied. Usage: `kaba <filename>`\n".red()
        );
        return;
    }

    let program = fs::read_to_string(&args[1]);
    if let Err(e) = program {
        eprintln!("{}\n", format!("ERR: {}", e.to_string()).red());
        return;
    }

    kaba::run(&program.unwrap());
    println!();
}
