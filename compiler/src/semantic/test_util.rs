use super::{error::Result, typ::Type};
use crate::{
    ast::{AstNodeVariant, NodeId},
    lexer, parser,
    semantic::{self, expression, state::AnalyzerState},
};

pub fn assert_is_ok(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = semantic::analyze(&ast);

    assert!(result.is_ok());
}

pub fn assert_is_err(input: &str) {
    let tokens = lexer::lex(input).unwrap();
    let ast = parser::parse(tokens).unwrap();

    let result = semantic::analyze(&ast);

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

    if let AstNodeVariant::Program { body, .. } = &ast.variant {
        let mut state = AnalyzerState::new(ast.id);
        for (i, (sym, t)) in symbols.iter().enumerate() {
            // make id large to avoid collision during test
            let sym_id: NodeId = (i + 99999).try_into().unwrap();
            state.save_entity(sym_id, sym, t.clone());
        }

        expression::analyze(&state, &body[0]).map(|res| res.into_owned())
    } else {
        unreachable!();
    }
}
