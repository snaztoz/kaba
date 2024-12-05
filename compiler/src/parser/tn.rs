use super::{
    error::{ParsingError, Result},
    stream::TokenStream,
};
use crate::{
    ast::{AstNode, TypeNotation},
    lexer::TokenKind,
};

pub struct TypeNotationParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> TypeNotationParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl TypeNotationParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        match self.tokens.current_kind() {
            TokenKind::Identifier(name) => {
                let tn = AstNode::TypeNotation {
                    tn: TypeNotation::Identifier(name),
                    span: self.tokens.current().span.clone(),
                };

                self.tokens.advance();

                Ok(tn)
            }

            TokenKind::LBrack => self.parse_array_tn(),
            TokenKind::LParen => self.parse_function_tn(),

            _ => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Identifier(String::from("foo")),
                found: self.tokens.current().kind.clone(),
                span: self.tokens.current().span,
            }),
        }
    }

    fn parse_array_tn(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "["
        self.tokens.skip(&TokenKind::LBrack)?;

        // Expecting "]"
        self.tokens.skip(&TokenKind::RBrack)?;

        // Parse array element type
        let elem_tn = self.parse()?;

        let end = elem_tn.span().end;

        Ok(AstNode::TypeNotation {
            tn: TypeNotation::Array(Box::new(elem_tn)),
            span: start..end,
        })
    }

    fn parse_function_tn(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "("
        self.tokens.skip(&TokenKind::LParen)?;

        // Expecting parameter type notation(s)
        let mut params_tn = vec![];
        while !self.tokens.current_is(&TokenKind::RParen) {
            // Expecting type notation
            let tn = self.parse()?;

            params_tn.push(tn);

            match self.tokens.current_kind() {
                TokenKind::Comma => {
                    self.tokens.skip(&TokenKind::Comma)?;
                    continue;
                }

                TokenKind::RParen => continue,

                kind => {
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                        span: self.tokens.current().span,
                    });
                }
            }
        }

        // Expecting ")"
        self.tokens.skip(&TokenKind::RParen)?;

        // Expecting "->"
        self.tokens.skip(&TokenKind::RightPoint)?;

        // Expecting return type notation
        let return_tn = Box::new(self.parse()?);

        let end = return_tn.span().end;

        Ok(AstNode::TypeNotation {
            tn: TypeNotation::Callable {
                params_tn,
                return_tn,
            },
            span: start..end,
        })
    }
}
