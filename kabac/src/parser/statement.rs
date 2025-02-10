use super::{
    conditional, each_loop, expression, function, record, state::ParserState, variable, while_loop,
    Result,
};
use crate::{
    ast::AstNode,
    lexer::token::{Token, TokenKind},
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    // Check if statement starts with a keyword
    match state.tokens.current_kind() {
        TokenKind::Var => return variable::parse(state),
        TokenKind::If => return conditional::parse(state),
        TokenKind::While => return while_loop::parse(state),
        TokenKind::Each => return each_loop::parse(state),
        TokenKind::Break | TokenKind::Continue => return parse_loop_control(state),
        TokenKind::Def => return function::parse(state),
        TokenKind::Return => return parse_return_statement(state),
        TokenKind::Debug => return parse_debug_statement(state),
        TokenKind::Record => return record::parse(state),
        _ => (),
    }

    // Expecting expression
    let expr = expression::parse(state)?;

    // Expecting ";"
    state.tokens.skip(&TokenKind::Semicolon)?;

    Ok(expr.unwrap_group())
}

fn parse_loop_control<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let Token { kind, span, .. } = state.tokens.current();

    // Expecting either "break" or "continue" keyword
    let control = match kind {
        TokenKind::Break => AstNode::Break { span },
        TokenKind::Continue => AstNode::Continue { span },
        _ => unreachable!(),
    };

    state.tokens.advance();

    // Expecting ";"
    state.tokens.skip(&TokenKind::Semicolon)?;

    Ok(control)
}

fn parse_return_statement<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;
    let mut end = state.tokens.current().span.end;

    // Expecting "return" keyword
    state.tokens.skip(&TokenKind::Return)?;

    // Expecting expression (optional)
    let expr = if state.tokens.current_is(&TokenKind::Semicolon) {
        None
    } else {
        let expr = expression::parse(state)?;
        end = expr.span().end;
        Some(expr)
    };

    // Expecting ";"
    state.tokens.skip(&TokenKind::Semicolon)?;

    Ok(AstNode::Return {
        expr: expr.map(Box::new),
        span: start..end,
    })
}

fn parse_debug_statement<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "debug" keyword
    state.tokens.skip(&TokenKind::Debug)?;

    // Expecting expression
    let expr = Box::new(expression::parse(state)?);

    let end = expr.span().end;

    // Expecting ";"
    state.tokens.skip(&TokenKind::Semicolon)?;

    Ok(AstNode::Debug {
        expr,
        span: start..end,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal},
        parser::test_util::assert_ast,
    };

    #[test]
    fn debug_statement() {
        assert_ast(
            "debug 5 + 5 * 7;",
            AstNode::Debug {
                expr: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 6..7,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            lit: Literal::Int(5),
                            span: 10..11,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            lit: Literal::Int(7),
                            span: 14..15,
                        }),
                        span: 10..15,
                    }),
                    span: 6..15,
                }),
                span: 0..15,
            },
        )
    }
}
