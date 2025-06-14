use super::{block, expression, state::ParserState, sym, Result};
use crate::{
    ast::{AstNode, AstNodeVariant},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    state.tokens.expect(&TokenKind::Each)?;

    let elem_sym = sym::parse(state, "item name")?;

    state.tokens.expect(&TokenKind::In)?;

    let arr = expression::parse(state)?;

    let block = block::parse(state)?;

    let end = block.span.end;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::Each {
            iterable: Box::new(arr),
            elem_sym: Box::new(elem_sym),
            body: block.body,
        },
        span: start..end,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, AstNodeVariant},
        parser::test_util::assert_ast,
    };

    #[test]
    fn each_statement() {
        assert_ast(
            "each elem in arr {}",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Each {
                    elem_sym: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "elem" },
                        span: 5..9,
                    }),

                    iterable: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "arr" },
                        span: 13..16,
                    }),
                    body: vec![],
                },
                span: 0..19,
            },
        );
    }
}
