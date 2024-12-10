use super::{block::BlockParser, expression::ExpressionParser, stream::TokenStream, Result};
use crate::{ast::AstNode, lexer::TokenKind};

pub struct WhileLoopParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> WhileLoopParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl WhileLoopParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "while" keyword
        self.tokens.skip(&TokenKind::While)?;

        // Expecting expression
        let cond = ExpressionParser::new(self.tokens).parse()?;

        // Expecting block
        let block = BlockParser::new(self.tokens).parse()?;

        let end = block.span.end;

        Ok(AstNode::While {
            cond: Box::new(cond),
            body: block.body,
            span: start..end,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal},
        parser::test_util::parse_and_assert_result,
    };

    #[test]
    fn while_statement() {
        parse_and_assert_result(
            "while true do end",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 6..10,
                }),
                body: vec![],
                span: 0..17,
            },
        );
    }

    #[test]
    fn while_statement_with_loop_control_statements() {
        parse_and_assert_result(
            "while true do continue; break; end",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 6..10,
                }),
                body: vec![
                    AstNode::Continue { span: 14..22 },
                    AstNode::Break { span: 24..29 },
                ],
                span: 0..34,
            },
        );
    }
}
