use colored::Colorize;
use runtime::Runtime;

mod parser;
mod runtime;

pub fn run(program: &str) {
    let ast = parser::parse(program).map_err(|e| e.to_string());

    let ast = match ast {
        Ok(mut pairs) => pairs.next().unwrap(),

        Err(e) => {
            eprintln!("{}", e.red());
            return;
        }
    };

    let mut runtime = Runtime::new(program, ast);
    runtime.run();

    if let Some(e) = runtime.error {
        eprintln!("{} {}", "ERR:".red(), e.red());
    }
}
