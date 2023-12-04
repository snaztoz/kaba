// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the required logic operations during the
//! parsing stage of a Kaba tokens.

use crate::ast::{AstNode, Program as ProgramAst, Value};
use crate::error::Error;
use crate::lexer::{RichToken, Token};

/// Provide a quick way to parse Kaba tokens, without the needs to
/// setting up and running the parser manually.
///
/// Produces an AST that represents the entire source code of the
/// given tokens (see [`crate::ast::Program`]).
pub fn parse(tokens: Vec<RichToken>) -> Result<ProgramAst, Error> {
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

    fn parse(&mut self) -> Result<ProgramAst, Error> {
        let mut statements = vec![];

        while self.cursor < self.tokens.len() {
            let statement = self.parse_statement()?;
            statements.push(statement)
        }

        Ok(ProgramAst { statements })
    }

    fn parse_statement(&mut self) -> Result<AstNode, Error> {
        // Check if statement starts with a keyword

        if self.get_current_token_kind_or_error()? == Token::Var {
            return self.parse_variable_declaration();
        } else {
            // TODO: support other statements
        }

        // Expecting expression

        let mut expression = self.parse_expression()?;

        // Expecting "=" (optional, transform to: variable assignment)

        if self.get_current_token_kind_or_error()? == Token::Assign {
            self.advance(); // skip "="
            let rhs = self.parse_expression()?;

            expression = AstNode::ValueAssignment {
                lhs: Box::from(expression),
                value: Box::from(rhs),
            };
        }

        self.assert_current_is_semicolon_and_advance()?;

        Ok(expression)
    }

    fn parse_variable_declaration(&mut self) -> Result<AstNode, Error> {
        self.advance(); // skip "var" keyword

        // Parse identifier

        let identifier = match self.get_current_token_kind_or_error()? {
            Token::Identifier(name) => {
                self.advance();
                name
            }
            _ => {
                let token = self.get_current_token_and_advance().unwrap();
                return Err(Error::ParsingUnexpectedToken {
                    expected: Token::Identifier(String::from("foo")),
                    found: token.kind.clone(),
                    pos: token.range.start,
                });
            }
        };

        // Expecting ":" (optional)

        let r#type = if self.get_current_token_kind_or_error()? == Token::Colon {
            self.advance(); // skip ":"
            todo!("implementing variable type notation")
        } else {
            None
        };

        // Expecting "=" (optional)

        let value = if self.get_current_token_kind_or_error()? == Token::Assign {
            self.advance(); // skip "="
            Some(Box::from(self.parse_expression()?))
        } else {
            None
        };

        // Expecting ";"

        self.assert_current_is_semicolon_and_advance()?;

        Ok(AstNode::VariableDeclaration {
            identifier,
            r#type,
            value,
        })
    }

    fn parse_expression(&mut self) -> Result<AstNode, Error> {
        // TODO: make this rule starts from higher rule

        self.parse_additive_expression()
    }

    fn parse_additive_expression(&mut self) -> Result<AstNode, Error> {
        // Parse first term

        let mut node = self.parse_multiplicative_expression()?;

        loop {
            // Expecting "+" or "-" (both are optional)

            match self.get_current_token_kind_or_error()? {
                Token::Add => {
                    self.advance(); // skip "+"
                    let rhs = self.parse_multiplicative_expression()?;
                    let add = AstNode::Add(Box::from(node), Box::from(rhs));
                    node = add;
                }
                Token::Sub => {
                    self.advance(); // skip "-"
                    let rhs = self.parse_multiplicative_expression()?;
                    let sub = AstNode::Sub(Box::from(node), Box::from(rhs));
                    node = sub;
                }
                _ => return Ok(node),
            }
        }
    }

    fn parse_multiplicative_expression(&mut self) -> Result<AstNode, Error> {
        // Parse first term

        let mut node = self.parse_primary_expression()?;

        loop {
            // Expecting "*" or "/" (both are optional)

            match self.get_current_token_kind_or_error()? {
                Token::Mul => {
                    self.advance(); // skip "*"
                    let rhs = self.parse_primary_expression()?;
                    let mul = AstNode::Mul(Box::from(node), Box::from(rhs));
                    node = mul;
                }
                Token::Div => {
                    self.advance(); // skip "/"
                    let rhs = self.parse_primary_expression()?;
                    let div = AstNode::Div(Box::from(node), Box::from(rhs));
                    node = div;
                }
                _ => return Ok(node),
            }
        }
    }

    fn parse_primary_expression(&mut self) -> Result<AstNode, Error> {
        // TODO: expand this rule

        let mut node = match self.get_current_token_kind_or_error()? {
            Token::LParen => {
                // Parse prioritized expression
                self.advance(); // skip "("
                let expression = self.parse_expression()?;
                self.advance(); // skip ")"
                expression
            }

            // Expecting either identifier or integer
            Token::Identifier(name) => {
                self.advance();
                AstNode::Identifier(name)
            }
            Token::Integer(n) => {
                self.advance();
                AstNode::Val(Value::Integer(n))
            }

            k => {
                let token = self.get_current_token_and_advance().unwrap();
                return Err(Error::ParsingUnexpectedToken {
                    expected: Token::Identifier(String::from("foo")),
                    found: k.clone(),
                    pos: token.range.start,
                });
            }
        };

        // Followed by >= 0 function call, field access, indexed access

        // TODO: field access and indexed access

        loop {
            match self.get_current_token_kind_or_error()? {
                Token::LParen => {
                    self.advance(); // skip "("
                    let args = self.parse_function_call()?;
                    node = AstNode::FunctionCall {
                        callee: Box::from(node),
                        args,
                    };
                }

                _ => return Ok(node),
            }
        }
    }

    fn parse_function_call(&mut self) -> Result<Vec<AstNode>, Error> {
        // Can have >= 0 arguments

        let mut args = vec![];

        loop {
            // Stop when encounter a closing parentheses

            if self.get_current_token_kind_or_error()? == Token::RParen {
                self.advance(); // skip ")"
                return Ok(args);
            }

            // Parse argument

            let expression = self.parse_expression()?;
            args.push(expression);

            // Continue if encounter "," or break out of loop if encounter ")"

            match self.get_current_token_kind_or_error()? {
                Token::Comma => {
                    self.advance(); // skip "("
                    continue;
                }

                Token::RParen => continue,

                _ => {
                    // Error if encounter neither "," or ")"

                    let token = self.get_current_token_and_advance().unwrap();
                    return Err(Error::ParsingUnexpectedToken {
                        expected: Token::RParen,
                        found: token.kind.clone(),
                        pos: token.range.start,
                    });
                }
            }
        }
    }

    fn assert_current_is_semicolon_and_advance(&mut self) -> Result<(), Error> {
        if self.get_current_token_kind_or_error()? != Token::Semicolon {
            let token = self.get_current_token_and_advance().unwrap();
            return Err(Error::ParsingUnexpectedToken {
                expected: Token::Semicolon,
                found: token.kind.clone(),
                pos: token.range.start,
            });
        }

        self.advance(); // skip ";"

        Ok(())
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn get_current_token_and_advance(&mut self) -> Option<RichToken> {
        let token = self.tokens.get(self.cursor).cloned();
        self.advance();
        token
    }

    fn get_current_token_kind_or_error(&self) -> Result<Token, Error> {
        self.tokens
            .get(self.cursor)
            .map(|rt| rt.kind.clone())
            .ok_or(Error::ParsingUnexpectedEof)
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
                    value: Some(Box::from(AstNode::Mul(
                        Box::from(AstNode::Val(Value::Integer(123))),
                        Box::from(AstNode::Identifier(String::from("x"))),
                    ))),
                },
            ),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = Parser::new(tokens).parse();

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
                lhs: Box::from(AstNode::Identifier(String::from("abc"))),
                value: Box::from(AstNode::Mul(
                    Box::from(AstNode::Val(Value::Integer(123))),
                    Box::from(AstNode::Identifier(String::from("x"))),
                )),
            },
        )];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = Parser::new(tokens).parse();

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
                    Box::from(AstNode::Add(
                        Box::from(AstNode::Identifier(String::from("abc"))),
                        Box::from(AstNode::Mul(
                            Box::from(AstNode::Val(Value::Integer(512))),
                            Box::from(AstNode::Val(Value::Integer(200))),
                        )),
                    )),
                    Box::from(AstNode::Div(
                        Box::from(AstNode::Identifier(String::from("abc"))),
                        Box::from(AstNode::Val(Value::Integer(3))),
                    )),
                ),
            ),
            (
                "(123 - 53) * 7;",
                AstNode::Mul(
                    Box::from(AstNode::Sub(
                        Box::from(AstNode::Val(Value::Integer(123))),
                        Box::from(AstNode::Val(Value::Integer(53))),
                    )),
                    Box::from(AstNode::Val(Value::Integer(7))),
                ),
            ),
            (
                "abc(123, 50 + 2) * 7;",
                AstNode::Mul(
                    Box::from(AstNode::FunctionCall {
                        callee: Box::from(AstNode::Identifier(String::from("abc"))),
                        args: vec![
                            AstNode::Val(Value::Integer(123)),
                            AstNode::Add(
                                Box::from(AstNode::Val(Value::Integer(50))),
                                Box::from(AstNode::Val(Value::Integer(2))),
                            ),
                        ],
                    }),
                    Box::from(AstNode::Val(Value::Integer(7))),
                ),
            ),
            (
                "abc(xyz(123, 456),);",
                AstNode::FunctionCall {
                    callee: Box::from(AstNode::Identifier(String::from("abc"))),
                    args: vec![AstNode::FunctionCall {
                        callee: Box::from(AstNode::Identifier(String::from("xyz"))),
                        args: vec![
                            AstNode::Val(Value::Integer(123)),
                            AstNode::Val(Value::Integer(456)),
                        ],
                    }],
                },
            ),
        ];

        for (input, expected) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = Parser::new(tokens).parse();

            assert!(result.is_ok());
            assert_eq!(
                result.unwrap(),
                ProgramAst {
                    statements: vec![expected]
                }
            );
        }
    }
}
