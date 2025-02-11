use super::{block, expression, state::ParserState, sym, Result};
use crate::{
    ast::{AstNode, AstNodeVariant},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "each" keyword
    state.tokens.skip(&TokenKind::Each)?;

    // Expecting element symbol
    let elem_sym_id = state.next_symbol_id();
    let elem_sym = sym::parse(state, "item name")?;

    // Expecting "in" keyword
    state.tokens.skip(&TokenKind::In)?;

    // Expecting expression
    let arr = expression::parse(state)?;

    // Expecting block
    let scope_id = state.next_scope_id();
    let block = block::parse(state)?;

    let end = block.span.end;

    Ok(AstNode {
        variant: AstNodeVariant::Each {
            iterable: Box::new(arr),
            elem_sym: Box::new(elem_sym),
            elem_sym_id,
            body: block.body,
            scope_id,
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
                variant: AstNodeVariant::Each {
                    elem_sym: Box::new(AstNode {
                        variant: AstNodeVariant::Symbol { name: "elem" },
                        span: 5..9,
                    }),
                    elem_sym_id: 1,
                    iterable: Box::new(AstNode {
                        variant: AstNodeVariant::Symbol { name: "arr" },
                        span: 13..16,
                    }),
                    body: vec![],
                    scope_id: 2,
                },
                span: 0..19,
            },
        );
    }
}
