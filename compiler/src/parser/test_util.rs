use crate::{
    ast::{AstNode, AstNodeVariant},
    lexer,
    parser::parse,
};

pub fn assert_ast(input: &str, expect: AstNode) {
    let tokens = lexer::lex(input).unwrap();
    let result = parse(tokens);

    assert!(result.is_ok());
    assert_eq!(
        result.unwrap(),
        AstNode {
            id: 0,
            variant: AstNodeVariant::Program { body: vec![expect] },
            span: 0..input.len(),
        }
    );
}

pub fn assert_is_err(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let result = parse(tokens);

    assert!(result.is_err());
}
