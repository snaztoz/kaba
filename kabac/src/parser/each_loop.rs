use super::{
    block,
    error::{ParsingError, ParsingErrorVariant},
    expression,
    state::ParserState,
    Result,
};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub fn parse(state: &ParserState) -> Result<AstNode> {
    let start = state.tokens.current().span.start;

    // Expecting "each" keyword
    state.tokens.skip(&TokenKind::Each)?;

    // Expecting element symbol
    let elem_sym_id = state.next_symbol_id();
    let elem_sym = parse_sym(state)?;

    // Expecting "in" keyword
    state.tokens.skip(&TokenKind::In)?;

    // Expecting expression
    let arr = expression::parse(state)?;

    // Expecting block
    let scope_id = state.next_scope_id();
    let block = block::parse(state)?;

    let end = block.span.end;

    Ok(AstNode::Each {
        iterable: Box::new(arr),
        elem_sym: Box::new(elem_sym),
        elem_sym_id,
        body: block.body,
        scope_id,
        span: start..end,
    })
}

fn parse_sym(state: &ParserState) -> Result<AstNode> {
    let sym = match state.tokens.current_kind() {
        TokenKind::Symbol(name) => Ok(AstNode::Symbol {
            name,
            span: state.tokens.current().span,
        }),

        kind => Err(ParsingError {
            variant: ParsingErrorVariant::UnexpectedToken {
                expect: TokenKind::Symbol(String::from("elem")),
                found: kind.clone(),
            },
            span: state.tokens.current().span,
        }),
    };

    state.tokens.advance();

    sym
}

#[cfg(test)]
mod tests {
    use crate::{ast::AstNode, parser::test_util::assert_ast};

    #[test]
    fn each_statement() {
        assert_ast(
            "each elem in arr {}",
            AstNode::Each {
                elem_sym: Box::new(AstNode::Symbol {
                    name: String::from("elem"),
                    span: 5..9,
                }),
                elem_sym_id: 1,
                iterable: Box::new(AstNode::Symbol {
                    name: String::from("arr"),
                    span: 13..16,
                }),
                body: vec![],
                scope_id: 2,
                span: 0..19,
            },
        );
    }
}
