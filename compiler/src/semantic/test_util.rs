use super::{error::Result, types::Type};
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

pub fn assert_expr_type(input: &str, symbols: &[(&str, Type)], expected_t: Type) {
    let result = eval_expr(input, symbols);

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), expected_t);
}

pub fn assert_expr_is_err(input: &str, symbols: &[(&str, Type)]) {
    let result = eval_expr(input, symbols);

    assert!(result.is_err());
}

pub fn eval_expr(input: &str, symbols: &[(&str, Type)]) -> Result<Type> {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    if let AstNode::Program { body, .. } = &ast {
        let state = SharedState::new();
        for (sym, t) in symbols.iter() {
            state
                .save_sym_or_else(sym, t.clone(), || unreachable!())
                .unwrap();
        }
        ExpressionAnalyzer::new(&body[0], &state).analyze()
    } else {
        unreachable!();
    }
}
