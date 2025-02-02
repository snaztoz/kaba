use super::{block, expression, state::ParserState, Result};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub fn parse(state: &ParserState) -> Result<AstNode> {
    let start = state.tokens.current().span.start;

    // Expecting "while" keyword
    state.tokens.skip(&TokenKind::While)?;

    // Expecting expression
    let cond = expression::parse(state)?;

    // Expecting block
    let scope_id = state.next_scope_id();
    let block = block::parse(state)?;

    let end = block.span.end;

    Ok(AstNode::While {
        cond: Box::new(cond),
        body: block.body,
        scope_id,
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
    fn while_statement() {
        assert_ast(
            "while true {}",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 6..10,
                }),
                body: vec![],
                scope_id: 2,
                span: 0..13,
            },
        );
    }

    #[test]
    fn while_statement_with_loop_control_statements() {
        assert_ast(
            "while true { continue; break; }",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 6..10,
                }),
                body: vec![
                    AstNode::Continue { span: 13..21 },
                    AstNode::Break { span: 23..28 },
                ],
                scope_id: 2,
                span: 0..31,
            },
        );
    }
}
