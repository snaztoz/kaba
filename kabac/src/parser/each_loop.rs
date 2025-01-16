use super::{
    block::BlockParser, error::ParsingError, expression::ExpressionParser, state::ParserState,
    Result,
};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub struct EachLoopParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> EachLoopParser<'a> {
    pub const fn new(state: &'a ParserState) -> Self {
        Self { state }
    }
}

impl EachLoopParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;

        // Expecting "each" keyword
        self.state.tokens.skip(&TokenKind::Each)?;

        // Expecting element symbol
        let elem_sym = self.parse_sym()?;

        // Expecting "in" keyword
        self.state.tokens.skip(&TokenKind::In)?;

        // Expecting expression
        let arr = ExpressionParser::new(self.state).parse()?;

        // Expecting block
        let block = BlockParser::new(self.state).parse()?;

        let end = block.span.end;

        Ok(AstNode::Each {
            iterable: Box::new(arr),
            elem_sym: Box::new(elem_sym),
            elem_sym_id: self.state.next_id(),
            body: block.body,
            span: start..end,
        })
    }

    fn parse_sym(&self) -> Result<AstNode> {
        let sym = match self.state.tokens.current_kind() {
            TokenKind::Symbol(name) => Ok(AstNode::Symbol {
                name,
                span: self.state.tokens.current().span,
            }),

            kind => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Symbol(String::from("elem")),
                found: kind.clone(),
                span: self.state.tokens.current().span,
            }),
        };

        self.state.tokens.advance();

        sym
    }
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
                span: 0..19,
            },
        );
    }
}
