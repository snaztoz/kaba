use super::{
    error::{ParsingError, ParsingErrorVariant},
    Result,
};
use crate::lexer::token::{Token, TokenKind};
use std::cell::RefCell;

pub struct TokenStream {
    tokens: Vec<Token>,
    cursor: RefCell<usize>,
}

impl TokenStream {
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: RefCell::new(0),
        }
    }

    pub fn advance(&self) {
        *self.cursor.borrow_mut() += 1;
    }

    pub fn skip(&self, expected_token: &TokenKind) -> Result<()> {
        self.expect_current(expected_token)?;
        self.advance();
        Ok(())
    }

    pub fn current(&self) -> Token {
        self.tokens.get(*self.cursor.borrow()).cloned().unwrap()
    }

    pub fn current_kind(&self) -> TokenKind {
        self.tokens.get(*self.cursor.borrow()).unwrap().kind.clone()
    }

    pub fn current_is(&self, token: &TokenKind) -> bool {
        &self.current_kind() == token
    }

    fn expect_current(&self, expect: &TokenKind) -> Result<()> {
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
