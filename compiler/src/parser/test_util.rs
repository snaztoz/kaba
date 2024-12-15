use crate::{ast::AstNode, lexer, parser::parse};

pub fn parse_and_assert_result(input: &str, expect: AstNode) {
    let tokens = lexer::lex(input).unwrap();
    let result = parse(tokens);

    assert!(result.is_ok());
    assert_eq!(
        result.unwrap(),
        AstNode::Program {
            body: vec![expect],
            span: 0..input.len(),
        }
    );
}

pub fn parse_and_assert_error(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let result = parse(tokens);

    assert!(result.is_err());
}
