use super::types::Type;
use crate::{
    ast::AstNode,
    lexer, parser,
    semantic::{expression::ExpressionAnalyzer, state::SharedState, ProgramAnalyzer},
};

pub fn assert_is_ok(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = ProgramAnalyzer::new(&ast).analyze();

    assert!(result.is_ok());
}

pub fn assert_is_err(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = ProgramAnalyzer::new(&ast).analyze();

    assert!(result.is_err());
}

pub fn assert_expression_type(input: &str, expected_t: Type) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = if let AstNode::Program { body, .. } = &ast {
        let state = SharedState::new();
        ExpressionAnalyzer::new(&body[0], &state).analyze()
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
        let state = SharedState::new();
        ExpressionAnalyzer::new(&body[0], &state).analyze()
    } else {
        unreachable!();
    };

    assert!(result.is_err());
}
