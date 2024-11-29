use crate::{lexer, parser, semantic::ProgramChecker};

pub fn check_and_assert_is_ok(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = ProgramChecker::new(&ast).check();

    assert!(result.is_ok());
}

pub fn check_and_assert_is_err(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = ProgramChecker::new(&ast).check();

    assert!(result.is_err());
}
