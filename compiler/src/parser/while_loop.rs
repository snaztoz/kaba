use super::{block, expression, state::ParserState, Result};
use crate::{
    ast::{AstNode, AstNodeVariant},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "while" keyword
    state.tokens.skip(&TokenKind::While)?;

    // Expecting expression
    let cond = expression::parse(state)?;

    // Expecting block
    let block = block::parse(state)?;

    let end = block.span.end;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::While {
            cond: Box::new(cond),
            body: block.body,
        },
        span: start..end,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, AstNodeVariant, Literal},
        parser::test_util::assert_ast,
    };

    #[test]
    fn while_statement() {
        assert_ast(
            "while true {}",
            AstNode {
                id: 0,
                variant: AstNodeVariant::While {
                    cond: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Bool(true),
                        },
                        span: 6..10,
                    }),
                    body: vec![],
                },
                span: 0..13,
            },
        );
    }

    #[test]
    fn while_statement_with_loop_control_statements() {
        assert_ast(
            "while true { continue; break; }",
            AstNode {
                id: 0,
                variant: AstNodeVariant::While {
                    cond: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Bool(true),
                        },
                        span: 6..10,
                    }),
                    body: vec![
                        AstNode {
                            id: 0,
                            variant: AstNodeVariant::Continue,
                            span: 13..21,
                        },
                        AstNode {
                            id: 0,
                            variant: AstNodeVariant::Break,
                            span: 23..28,
                        },
                    ],
                },
                span: 0..31,
            },
        );
    }
}
