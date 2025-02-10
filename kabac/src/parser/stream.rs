use super::{
    error::{ParsingError, ParsingErrorVariant},
    Result,
};
use crate::lexer::token::{Token, TokenKind};
use std::cell::RefCell;

pub struct TokenStream<'src> {
    tokens: Vec<Token<'src>>,
    cursor: RefCell<usize>,
}

impl<'src> TokenStream<'src> {
    pub const fn new(tokens: Vec<Token<'src>>) -> Self {
        Self {
            tokens,
            cursor: RefCell::new(0),
        }
    }

    pub fn advance(&self) {
        *self.cursor.borrow_mut() += 1;
    }

    pub fn skip(&self, expected_token: &TokenKind<'src>) -> Result<'src, ()> {
        self.expect_current(expected_token)?;
        self.advance();
        Ok(())
    }

    pub fn current(&self) -> Token<'src> {
        self.tokens.get(*self.cursor.borrow()).cloned().unwrap()
    }

    pub fn current_kind(&self) -> TokenKind<'src> {
        self.tokens.get(*self.cursor.borrow()).unwrap().kind.clone()
    }

    pub fn current_is(&self, token: &TokenKind) -> bool {
        &self.current_kind() == token
    }

    fn expect_current(&self, expect: &TokenKind<'src>) -> Result<'src, ()> {
        let current = self.current();
        if &current.kind != expect {
            return Err(ParsingError {
                variant: ParsingErrorVariant::UnexpectedToken {
                    expect: expect.clone(),
                    found: current.kind.clone(),
                },
                span: current.span,
            });
        }

        Ok(())
    }
}
