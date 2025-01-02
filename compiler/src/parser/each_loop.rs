use super::{
    block::BlockParser, error::ParsingError, expression::ExpressionParser, stream::TokenStream,
    Result,
};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub struct EachLoopParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> EachLoopParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl EachLoopParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "each" keyword
        self.tokens.skip(&TokenKind::Each)?;

        // Expecting element identifier
        let elem_id = self.parse_id()?;

        // Expecting "in" keyword
        self.tokens.skip(&TokenKind::In)?;

        // Expecting expression
        let arr = ExpressionParser::new(self.tokens).parse()?;

        // Expecting block
        let block = BlockParser::new(self.tokens).parse()?;

        let end = block.span.end;

        Ok(AstNode::Each {
            iterable: Box::new(arr),
            elem_id: Box::new(elem_id),
            body: block.body,
            span: start..end,
        })
    }

    fn parse_id(&self) -> Result<AstNode> {
        let id = match self.tokens.current_kind() {
            TokenKind::Identifier(name) => Ok(AstNode::Identifier {
                name,
                span: self.tokens.current().span,
            }),

            kind => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Identifier(String::from("elem")),
                found: kind.clone(),
                span: self.tokens.current().span,
            }),
        };

        self.tokens.advance();

        id
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::AstNode, parser::test_util::parse_and_assert_result};

    #[test]
    fn each_statement() {
        parse_and_assert_result(
            "each elem in arr {}",
            AstNode::Each {
                elem_id: Box::new(AstNode::Identifier {
                    name: String::from("elem"),
                    span: 5..9,
                }),
                iterable: Box::new(AstNode::Identifier {
                    name: String::from("arr"),
                    span: 13..16,
                }),
                body: vec![],
                span: 0..19,
            },
        );
    }
}
