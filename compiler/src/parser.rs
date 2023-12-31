// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the required logic operations during the
//! parsing stage of a Kaba tokens.

use crate::{
    ast::{AstNode, Program as ProgramAst, Value},
    lexer::{RichToken, Token},
};
use logos::Span;
use std::fmt::Display;

/// Provide a quick way to parse Kaba tokens, without the needs to
/// setting up and running the parser manually.
///
/// Produces an AST that represents the entire source code of the
/// given tokens (see [`crate::ast::Program`]).
pub fn parse(tokens: Vec<RichToken>) -> Result<ProgramAst, ParsingError> {
    Parser::new(tokens).parse()
}

struct Parser {
    tokens: Vec<RichToken>,
    cursor: usize,
}

impl Parser {
    fn new(tokens: Vec<RichToken>) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn parse(&mut self) -> Result<ProgramAst, ParsingError> {
        let mut statements = vec![];

        // don't count EOF token
        while self.cursor < self.tokens.len() - 1 {
            let statement = self.parse_statement()?;
            statements.push(statement)
        }

        Ok(ProgramAst { statements })
    }

    fn parse_statement(&mut self) -> Result<AstNode, ParsingError> {
        // Check if statement starts with a keyword

        if self.get_current_token() == Token::Var {
            return self.parse_variable_declaration();
        } else {
            // TODO: support other statements
        }

        // Expecting expression

        let mut expression = self.parse_expression()?;

        // Expecting "=" (optional, transform to: variable assignment)

        if self.get_current_token() == Token::Assign {
            self.advance(); // skip "="
            let rhs = self.parse_expression()?;

            expression = AstNode::ValueAssignment {
                lhs: Box::new(expression),
                value: Box::new(rhs),
            };
        }

        self.expect_current_token(Token::Semicolon)?;
        self.advance();

        Ok(expression)
    }

    fn parse_variable_declaration(&mut self) -> Result<AstNode, ParsingError> {
        self.advance(); // skip "var" keyword

        // Parse identifier

        let identifier = match self.get_current_token() {
            Token::Identifier(name) => {
                self.advance();
                name
            }
            _ => {
                let token = self.get_current_rich_token();
                self.advance();

                return Err(ParsingError::UnexpectedToken {
                    expected: Token::Identifier(String::from("foo")),
                    found: token.kind.clone(),
                    span: token.span,
                });
            }
        };

        // Expecting ":" (optional)

        let r#type = if self.get_current_token() == Token::Colon {
            self.advance(); // skip ":"
            todo!("implementing variable type notation")
        } else {
            None
        };

        // Expecting "=" (optional)

        let value = if self.get_current_token() == Token::Assign {
            self.advance(); // skip "="
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        // Expecting ";"

        self.expect_current_token(Token::Semicolon)?;
        self.advance();

        Ok(AstNode::VariableDeclaration {
            identifier,
            r#type,
            value,
        })
    }

    fn parse_expression(&mut self) -> Result<AstNode, ParsingError> {
        // TODO: make this rule starts from higher rule

        self.parse_additive_expression()
    }

    fn parse_additive_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut node = self.parse_multiplicative_expression()?;

        loop {
            // Expecting "+" or "-" (both are optional)

            match self.get_current_token() {
                Token::Add => {
                    self.advance(); // skip "+"
                    let rhs = self.parse_multiplicative_expression()?;
                    let add = AstNode::Add(Box::new(node), Box::new(rhs));
                    node = add;
                }
                Token::Sub => {
                    self.advance(); // skip "-"
                    let rhs = self.parse_multiplicative_expression()?;
                    let sub = AstNode::Sub(Box::new(node), Box::new(rhs));
                    node = sub;
                }
                _ => return Ok(node),
            }
        }
    }

    fn parse_multiplicative_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut node = self.parse_unary_expression()?;

        loop {
            // Expecting "*" or "/" (both are optional)

            match self.get_current_token() {
                Token::Mul => {
                    self.advance(); // skip "*"
                    let rhs = self.parse_unary_expression()?;
                    let mul = AstNode::Mul(Box::new(node), Box::new(rhs));
                    node = mul;
                }
                Token::Div => {
                    self.advance(); // skip "/"
                    let rhs = self.parse_unary_expression()?;
                    let div = AstNode::Div(Box::new(node), Box::new(rhs));
                    node = div;
                }
                _ => return Ok(node),
            }
        }
    }

    fn parse_unary_expression(&mut self) -> Result<AstNode, ParsingError> {
        //  Prefixed by >= 0 negation expression

        if self.get_current_token() == Token::Sub {
            self.advance();
            let child = self.parse_unary_expression()?.unwrap_group();
            return Ok(AstNode::Neg(Box::new(child)));
        }

        let mut node = self.parse_primary_expression()?.unwrap_group();

        // Followed by >= 0 function call, field access, or indexed access

        // TODO: field access and indexed access

        #[allow(clippy::while_let_loop)] // temporary
        loop {
            match self.get_current_token() {
                Token::LParen => {
                    self.advance(); // skip "("
                    let args = self.parse_function_call()?;
                    node = AstNode::FunctionCall {
                        callee: Box::new(node),
                        args,
                    };
                }

                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_primary_expression(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.get_current_rich_token();

        Ok(match token.kind {
            Token::LParen => {
                // Parse group expression

                let lparen_start = token.span.start;
                self.advance(); // skip "("

                let expression = self.parse_expression()?;

                self.expect_current_token(Token::RParen)?;
                let rparen_end = self.get_current_rich_token().span.end;
                self.advance();

                AstNode::Group {
                    child: Box::new(expression),
                    span: lparen_start..rparen_end,
                }
            }

            // Expecting either identifier or literals
            Token::Identifier(name) => {
                self.advance();
                AstNode::Identifier {
                    name,
                    span: token.span,
                }
            }
            Token::Integer(n) => {
                self.advance();
                AstNode::Literal {
                    value: Value::Integer(n),
                    span: token.span,
                }
            }
            Token::Float(n) => {
                self.advance();
                AstNode::Literal {
                    value: Value::Float(n),
                    span: token.span,
                }
            }

            k => {
                let token = self.get_current_rich_token();
                self.advance();

                return Err(ParsingError::UnexpectedToken {
                    expected: Token::Identifier(String::from("foo")),
                    found: k.clone(),
                    span: token.span,
                });
            }
        })
    }

    fn parse_function_call(&mut self) -> Result<Vec<AstNode>, ParsingError> {
        // Can have >= 0 arguments

        let mut args = vec![];

        loop {
            // Stop when encounter a closing parentheses

            if self.get_current_token() == Token::RParen {
                self.advance(); // skip ")"
                return Ok(args);
            }

            // Parse argument

            let expression = self.parse_expression()?;
            args.push(expression);

            // Continue if encounter "," or break out of loop if encounter ")"

            match self.get_current_token() {
                Token::Comma => {
                    self.advance(); // skip "("
                    continue;
                }

                Token::RParen => continue,

                _ => {
                    // Error if encounter neither "," or ")"

                    let token = self.get_current_rich_token();
                    self.advance();

                    return Err(ParsingError::UnexpectedToken {
                        expected: Token::RParen,
                        found: token.kind.clone(),
                        span: token.span,
                    });
                }
            }
        }
    }

    fn expect_current_token(&mut self, expected: Token) -> Result<(), ParsingError> {
        let current = self.get_current_token();
        if current != expected {
            let found = self.get_current_rich_token();
            self.advance();

            return Err(ParsingError::UnexpectedToken {
                expected,
                found: found.kind.clone(),
                span: found.span,
            });
        }

        Ok(())
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn get_current_rich_token(&mut self) -> RichToken {
        self.tokens.get(self.cursor).cloned().unwrap()
    }

    fn get_current_token(&self) -> Token {
        self.tokens.get(self.cursor).unwrap().kind.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedToken {
        expected: Token,
        found: Token,
        span: Span,
    },
}

impl ParsingError {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Self::UnexpectedToken { span, .. } => Some(span.clone()),
        }
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken {
                expected, found, ..
            } => {
                write!(f, "expecting to find {expected} but found {found} instead",)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_parsing_variable_declaration() {
        let cases = [
            (
                "var x;",
                AstNode::VariableDeclaration {
                    identifier: String::from("x"),
                    r#type: None,
                    value: None,
                },
            ),
            (
                "var abc = 123 * x;",
                AstNode::VariableDeclaration {
                    identifier: String::from("abc"),
                    r#type: None,
                    value: Some(Box::new(AstNode::Mul(
                        Box::new(AstNode::Literal {
                            value: Value::Integer(123),
                            span: 10..13,
                        }),
                        Box::new(AstNode::Identifier {
                            name: String::from("x"),
                            span: 16..17,
                        }),
                    ))),
                },
            ),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = parse(tokens);

            assert!(result.is_ok());
            assert_eq!(
                result.unwrap(),
                ProgramAst {
                    statements: vec![expected]
                }
            );
        }
    }

    #[test]
    fn test_parsing_variable_assignment() {
        let cases = [(
            "abc = 123 * x;",
            AstNode::ValueAssignment {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("abc"),
                    span: 0..3,
                }),
                value: Box::new(AstNode::Mul(
                    Box::new(AstNode::Literal {
                        value: Value::Integer(123),
                        span: 6..9,
                    }),
                    Box::new(AstNode::Identifier {
                        name: String::from("x"),
                        span: 12..13,
                    }),
                )),
            },
        )];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = parse(tokens);

            assert!(result.is_ok());
            assert_eq!(
                result.unwrap(),
                ProgramAst {
                    statements: vec![expected]
                }
            );
        }
    }

    #[test]
    fn test_parsing_expression() {
        let cases = [
            (
                "abc + 512 * 200 - abc / 3;",
                AstNode::Sub(
                    Box::new(AstNode::Add(
                        Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 0..3,
                        }),
                        Box::new(AstNode::Mul(
                            Box::new(AstNode::Literal {
                                value: Value::Integer(512),
                                span: 6..9,
                            }),
                            Box::new(AstNode::Literal {
                                value: Value::Integer(200),
                                span: 12..15,
                            }),
                        )),
                    )),
                    Box::new(AstNode::Div(
                        Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 18..21,
                        }),
                        Box::new(AstNode::Literal {
                            value: Value::Integer(3),
                            span: 24..25,
                        }),
                    )),
                ),
            ),
            (
                "(123 - 53) * 7;",
                AstNode::Mul(
                    Box::new(AstNode::Sub(
                        Box::new(AstNode::Literal {
                            value: Value::Integer(123),
                            span: 1..4,
                        }),
                        Box::new(AstNode::Literal {
                            value: Value::Integer(53),
                            span: 7..9,
                        }),
                    )),
                    Box::new(AstNode::Literal {
                        value: Value::Integer(7),
                        span: 13..14,
                    }),
                ),
            ),
            (
                "abc(123, 50 + 2) * 7;",
                AstNode::Mul(
                    Box::new(AstNode::FunctionCall {
                        callee: Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 0..3,
                        }),
                        args: vec![
                            AstNode::Literal {
                                value: Value::Integer(123),
                                span: 4..7,
                            },
                            AstNode::Add(
                                Box::new(AstNode::Literal {
                                    value: Value::Integer(50),
                                    span: 9..11,
                                }),
                                Box::new(AstNode::Literal {
                                    value: Value::Integer(2),
                                    span: 14..15,
                                }),
                            ),
                        ],
                    }),
                    Box::new(AstNode::Literal {
                        value: Value::Integer(7),
                        span: 19..20,
                    }),
                ),
            ),
            (
                "abc(xyz(123, 456),);",
                AstNode::FunctionCall {
                    callee: Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 0..3,
                    }),
                    args: vec![AstNode::FunctionCall {
                        callee: Box::new(AstNode::Identifier {
                            name: String::from("xyz"),
                            span: 4..7,
                        }),
                        args: vec![
                            AstNode::Literal {
                                value: Value::Integer(123),
                                span: 8..11,
                            },
                            AstNode::Literal {
                                value: Value::Integer(456),
                                span: 13..16,
                            },
                        ],
                    }],
                },
            ),
            (
                "-abc + (-(5)) * -(-7);",
                AstNode::Add(
                    Box::new(AstNode::Neg(Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 1..4,
                    }))),
                    Box::new(AstNode::Mul(
                        Box::new(AstNode::Neg(Box::new(AstNode::Literal {
                            value: Value::Integer(5),
                            span: 10..11,
                        }))),
                        Box::new(AstNode::Neg(Box::new(AstNode::Neg(Box::new(
                            AstNode::Literal {
                                value: Value::Integer(7),
                                span: 19..20,
                            },
                        ))))),
                    )),
                ),
            ),
            (
                "2 * (-0.5);",
                AstNode::Mul(
                    Box::new(AstNode::Literal {
                        value: Value::Integer(2),
                        span: 0..1,
                    }),
                    Box::new(AstNode::Neg(Box::new(AstNode::Literal {
                        value: Value::Float(0.5),
                        span: 6..9,
                    }))),
                ),
            ),
            (
                "-(-abc(-foo));",
                AstNode::Neg(Box::new(AstNode::Neg(Box::new(AstNode::FunctionCall {
                    callee: Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 3..6,
                    }),
                    args: vec![AstNode::Neg(Box::new(AstNode::Identifier {
                        name: String::from("foo"),
                        span: 8..11,
                    }))],
                })))),
            ),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = parse(tokens);

            assert!(result.is_ok());
            assert_eq!(
                result.unwrap(),
                ProgramAst {
                    statements: vec![expected]
                }
            );
        }
    }

    #[test]
    fn test_parsing_invalid_expression() {
        let cases = [(
            "(123 + 5;",
            ParsingError::UnexpectedToken {
                expected: Token::RParen,
                found: Token::Semicolon,
                span: 8..9,
            },
        )];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = parse(tokens);

            assert!(result.is_err());
            assert_eq!(result.unwrap_err(), expected);
        }
    }
}
