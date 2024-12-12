use super::types::Type;
use crate::{
    ast::AstNode,
    lexer, parser,
    semantic::{expression::ExpressionChecker, state::SharedState, ProgramChecker},
};

pub fn assert_is_ok(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = ProgramChecker::new(&ast).check();

    assert!(result.is_ok());
}

pub fn assert_is_err(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = ProgramChecker::new(&ast).check();

    assert!(result.is_err());
}

pub fn assert_expression_type(input: &str, expected_t: Type) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = if let AstNode::Program { body, .. } = &ast {
        let state = SharedState::default();
        ExpressionChecker::new(&body[0], &state).check()
    } else {
        unreachable!();
    };

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), expected_t);
}

pub fn assert_expression_is_err(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = if let AstNode::Program { body, .. } = &ast {
        let state = SharedState::default();
        ExpressionChecker::new(&body[0], &state).check()
    } else {
        unreachable!();
    };

    assert!(result.is_err());
}
