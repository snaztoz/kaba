use super::{
    block,
    error::{ParsingError, ParsingErrorVariant},
    expression,
    state::ParserState,
    Result,
};
use crate::{
    ast::{AstNode, AstNodeVariant},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;
    let mut end;

    // Expecting "if" keyword
    state.tokens.skip(&TokenKind::If)?;

    // Expecting expression
    let cond = expression::parse(state)?;

    // Expecting block
    let scope_id = state.next_scope_id();
    let block = block::parse(state)?;

    end = block.span.end;

    // Expecting >= 0 "else if" or 1 "else"
    let or_else = parse_alt_branch(state, &mut end)?;

    Ok(AstNode {
        variant: AstNodeVariant::If {
            cond: Box::new(cond),
            body: block.body,
            scope_id,
            or_else: or_else.map(Box::new),
        },
        span: start..end,
    })
}

fn parse_alt_branch<'src>(
    state: &ParserState<'src, '_>,
    end_pos: &mut usize,
) -> Result<'src, Option<AstNode<'src>>> {
    if !state.tokens.current_is(&TokenKind::Else) {
        return Ok(None);
    }

    let start = state.tokens.current().span.start;

    // Expecting "else" keyword
    state.tokens.skip(&TokenKind::Else)?;

    match state.tokens.current_kind() {
        TokenKind::If => {
            // Expecting "else if ..." statement
            let alt = parse(state)?;

            *end_pos = alt.span.end;

            Ok(Some(alt))
        }

        TokenKind::LBrace => {
            // Expecting block
            let scope_id = state.next_scope_id();
            let block = block::parse(state)?;

            *end_pos = block.span.end;

            Ok(Some(AstNode {
                variant: AstNodeVariant::Else {
                    body: block.body,
                    scope_id,
                },
                span: start..block.span.end,
            }))
        }

        kind => Err(ParsingError {
            variant: ParsingErrorVariant::UnexpectedToken {
                expect: TokenKind::Else,
                found: kind,
            },
            span: state.tokens.current().span,
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, AstNodeVariant, Literal},
        parser::test_util::assert_ast,
    };

    #[test]
    fn if_statement() {
        assert_ast(
            "if 15 > 10 { print(1); }",
            AstNode {
                variant: AstNodeVariant::If {
                    cond: Box::new(AstNode {
                        variant: AstNodeVariant::Gt {
                            lhs: Box::new(AstNode {
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(15),
                                },
                                span: 3..5,
                            }),
                            rhs: Box::new(AstNode {
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(10),
                                },
                                span: 8..10,
                            }),
                        },
                        span: 3..10,
                    }),
                    body: vec![AstNode {
                        variant: AstNodeVariant::FunctionCall {
                            callee: Box::new(AstNode {
                                variant: AstNodeVariant::Symbol { name: "print" },
                                span: 13..18,
                            }),
                            args: vec![AstNode {
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(1),
                                },
                                span: 19..20,
                            }],
                        },
                        span: 13..21,
                    }],
                    scope_id: 2,
                    or_else: None,
                },
                span: 0..24,
            },
        );
    }

    #[test]
    fn if_else_branches() {
        assert_ast(
            "if false {} else if false {} else {}",
            AstNode {
                variant: AstNodeVariant::If {
                    cond: Box::new(AstNode {
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Bool(false),
                        },
                        span: 3..8,
                    }),
                    body: vec![],
                    scope_id: 2,
                    or_else: Some(Box::new(AstNode {
                        variant: AstNodeVariant::If {
                            cond: Box::new(AstNode {
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Bool(false),
                                },
                                span: 20..25,
                            }),
                            body: vec![],
                            scope_id: 3,
                            or_else: Some(Box::new(AstNode {
                                variant: AstNodeVariant::Else {
                                    body: vec![],
                                    scope_id: 4,
                                },
                                span: 29..36,
                            })),
                        },
                        span: 17..36,
                    })),
                },
                span: 0..36,
            },
        );
    }
}
