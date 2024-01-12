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

        loop {
            if self.current_token_is(Token::Eof) {
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement)
        }

        Ok(ProgramAst { statements })
    }

    fn parse_block(&mut self) -> Result<(Vec<AstNode>, Span), ParsingError> {
        // Expecting "{"

        let start = self.get_current_rich_token().span.start;
        self.skip(Token::LBrace)?;

        // Expecting 0 >= statements, delimited with "}"

        let mut statements = vec![];
        loop {
            if self.current_token_is(Token::RBrace) {
                break;
            } else if self.current_token_is(Token::Eof) {
                return Err(ParsingError::UnexpectedToken {
                    expected: Token::RBrace,
                    found: Token::Eof,
                    span: self.get_current_rich_token().span,
                });
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        // Expecting "}"

        let end = self.get_current_rich_token().span.end;
        self.skip(Token::RBrace)?;

        Ok((statements, start..end))
    }

    fn parse_statement(&mut self) -> Result<AstNode, ParsingError> {
        // Check if statement starts with a keyword

        match self.get_current_token() {
            Token::Var => return self.parse_variable_declaration(),
            Token::If => return self.parse_conditional_branch(),
            Token::While => return self.parse_while(),
            Token::Break | Token::Continue => return self.parse_loop_control(),
            _ => (),
        }

        // Expecting expression

        let mut expression = self.parse_expression()?;

        // Expecting "=" (optional, transform to: variable assignment)

        if self.current_token_is(Token::Assign) {
            self.skip(Token::Assign)?;

            let rhs = self.parse_expression()?;
            let span = expression.get_span().start..rhs.get_span().end;

            expression = AstNode::Assign {
                lhs: Box::new(expression.unwrap_group()),
                value: Box::new(rhs.unwrap_group()),
                span,
            };
        }

        // Expecting ";"

        self.skip(Token::Semicolon)?;

        Ok(expression.unwrap_group())
    }

    fn parse_variable_declaration(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_rich_token().span.start;
        let mut end;

        self.skip(Token::Var)?;

        // Parse identifier

        let token = self.get_current_rich_token();
        let identifier = match token.kind {
            Token::Identifier(name) => Box::new(AstNode::Identifier {
                name,
                span: token.span.clone(),
            }),
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    expected: Token::Identifier(String::from("foo")),
                    found: token.kind.clone(),
                    span: token.span,
                });
            }
        };

        end = token.span.end;
        self.advance();

        // Expecting ":" (optional)

        let var_type = if self.current_token_is(Token::Colon) {
            self.skip(Token::Colon)?;

            let vt = self.parse_type_notation()?;

            end = vt.get_span().end;
            self.advance();

            Some(Box::new(vt))
        } else {
            None
        };

        // Expecting "=" (optional)

        let value = if self.current_token_is(Token::Assign) {
            self.skip(Token::Assign)?;

            let expression = self.parse_expression()?;
            end = expression.get_span().end;

            Some(Box::new(expression.unwrap_group()))
        } else {
            None
        };

        // Expecting ";"

        self.skip(Token::Semicolon)?;

        Ok(AstNode::VariableDeclaration {
            identifier,
            var_type,
            value,
            span: start..end,
        })
    }

    fn parse_type_notation(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.get_current_rich_token();
        match token.kind {
            Token::Identifier(name) => Ok(AstNode::TypeNotation {
                name,
                span: token.span.clone(),
            }),
            _ => Err(ParsingError::UnexpectedToken {
                expected: Token::Identifier(String::from("foo")),
                found: token.kind.clone(),
                span: token.span,
            }),
        }
    }

    fn parse_conditional_branch(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_rich_token().span.start;
        let mut end;
        self.skip(Token::If)?;

        // Expecting expression

        let condition = self.parse_expression()?;

        // Expecting block

        let (block_statements, block_span) = self.parse_block()?;
        end = block_span.end;

        // Expecting >= 0 "else if" or 1 "else"

        let or_else = if self.current_token_is(Token::Else) {
            let else_start = self.get_current_rich_token().span.start;
            self.skip(Token::Else)?;

            let token = self.get_current_rich_token();
            match token.kind {
                Token::If => {
                    // Expecting "else if ..." statement

                    let alt = self.parse_conditional_branch()?;
                    end = alt.get_span().end;

                    Some(Box::new(alt))
                }

                Token::LBrace => {
                    // Expecting block

                    let (block_statements, block_span) = self.parse_block()?;
                    let else_end = block_span.end;
                    end = else_end;

                    Some(Box::new(AstNode::Else {
                        body: block_statements,
                        span: else_start..else_end,
                    }))
                }

                kind => {
                    return Err(ParsingError::UnexpectedToken {
                        expected: Token::Else,
                        found: kind.clone(),
                        span: token.span,
                    });
                }
            }
        } else {
            None
        };

        Ok(AstNode::If {
            condition: Box::new(condition),
            body: block_statements,
            or_else,
            span: start..end,
        })
    }

    fn parse_while(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_rich_token().span.start;
        self.skip(Token::While)?;

        // Expecting expression

        let condition = self.parse_expression()?;

        // Expecting block

        let (block_statements, block_span) = self.parse_block()?;
        let end = block_span.end;

        Ok(AstNode::While {
            condition: Box::new(condition),
            body: block_statements,
            span: start..end,
        })
    }

    fn parse_loop_control(&mut self) -> Result<AstNode, ParsingError> {
        let RichToken { kind, span, .. } = self.get_current_rich_token();

        // Expecting either "break" or "continue" keyword

        let control = match kind {
            Token::Break => AstNode::Break { span },
            Token::Continue => AstNode::Continue { span },
            _ => unreachable!(),
        };

        self.advance();

        // Expecting ";"

        self.skip(Token::Semicolon)?;

        Ok(control)
    }

    fn parse_expression(&mut self) -> Result<AstNode, ParsingError> {
        // TODO: make this rule starts from higher rule

        self.parse_logical_and_or_expression()
    }

    fn parse_logical_and_or_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut child = self.parse_equality_expression()?;

        loop {
            // Expecting "||" or "&&" (both are optional)

            match self.get_current_token() {
                Token::Or => {
                    self.skip(Token::Or)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Or {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                Token::And => {
                    self.skip(Token::And)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::And {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(child),
            }
        }
    }

    fn parse_equality_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut child = self.parse_comparison_expression()?;

        loop {
            // Expecting "==" or "!=" (both are optional)

            match self.get_current_token() {
                Token::Eq => {
                    self.skip(Token::Eq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Eq {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                Token::Neq => {
                    self.skip(Token::Neq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Neq {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(child),
            }
        }
    }

    fn parse_comparison_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let child = self.parse_additive_expression()?;

        // Expecting ">", ">=", "<" or "<=" (all are optional)

        match self.get_current_token() {
            Token::Gt => {
                self.skip(Token::Gt)?;

                let rhs = self.parse_additive_expression()?;
                let span = child.get_span().start..rhs.get_span().end;

                Ok(AstNode::Gt {
                    lhs: Box::new(child.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            Token::Gte => {
                self.skip(Token::Gte)?;

                let rhs = self.parse_additive_expression()?;
                let span = child.get_span().start..rhs.get_span().end;

                Ok(AstNode::Gte {
                    lhs: Box::new(child.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            Token::Lt => {
                self.skip(Token::Lt)?;

                let rhs = self.parse_additive_expression()?;
                let span = child.get_span().start..rhs.get_span().end;

                Ok(AstNode::Lt {
                    lhs: Box::new(child.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            Token::Lte => {
                self.skip(Token::Lte)?;

                let rhs = self.parse_additive_expression()?;
                let span = child.get_span().start..rhs.get_span().end;

                Ok(AstNode::Lte {
                    lhs: Box::new(child.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            _ => Ok(child),
        }
    }

    fn parse_additive_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut child = self.parse_multiplicative_expression()?;

        loop {
            // Expecting "+" or "-" (both are optional)

            match self.get_current_token() {
                Token::Add => {
                    self.skip(Token::Add)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Add {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                Token::Sub => {
                    self.skip(Token::Sub)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Sub {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(child),
            }
        }
    }

    fn parse_multiplicative_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut child = self.parse_unary_expression()?;

        loop {
            // Expecting "*", "/" or "%" (all are optional)

            match self.get_current_token() {
                Token::Mul => {
                    self.skip(Token::Mul)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Mul {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                Token::Div => {
                    self.skip(Token::Div)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Div {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                Token::Mod => {
                    self.skip(Token::Mod)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = child.get_span().start..rhs.get_span().end;

                    child = AstNode::Mod {
                        lhs: Box::new(child.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(child),
            }
        }
    }

    fn parse_unary_expression(&mut self) -> Result<AstNode, ParsingError> {
        //  Prefixed by >= 0 negation or not expression

        if self.current_token_is(Token::Sub) {
            return self.parse_prefix_expression(Token::Sub);
        } else if self.current_token_is(Token::Not) {
            return self.parse_prefix_expression(Token::Not);
        }

        // Parse primary expression

        let mut child = self.parse_primary_expression()?;

        // Followed by >= 0 function call, field access, or indexed access

        // TODO: field access and indexed access

        #[allow(clippy::while_let_loop)] // temporary
        loop {
            match self.get_current_token() {
                Token::LParen => {
                    let callee_start = child.get_span().start;
                    self.skip(Token::LParen)?;

                    let args = self.parse_function_call()?;

                    let span = callee_start..self.get_current_rich_token().span.end;
                    self.skip(Token::RParen)?;

                    child = AstNode::FunctionCall {
                        callee: Box::new(child.unwrap_group()),
                        args,
                        span,
                    };
                }

                _ => break,
            }
        }

        Ok(child)
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Result<AstNode, ParsingError> {
        let start = self.get_current_rich_token().span.start;
        self.skip(token.clone())?;

        let child = self.parse_unary_expression()?;
        let span = start..child.get_span().end;

        match token {
            Token::Sub => Ok(AstNode::Neg {
                child: Box::new(child.unwrap_group()),
                span,
            }),
            Token::Not => Ok(AstNode::Not {
                child: Box::new(child.unwrap_group()),
                span,
            }),

            _ => unreachable!(),
        }
    }

    fn parse_primary_expression(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.get_current_rich_token();

        Ok(match token.kind {
            Token::LParen => {
                // Parse group expression

                let lparen_start = token.span.start;
                self.skip(Token::LParen)?;

                let expression = self.parse_expression()?;

                let span = lparen_start..self.get_current_rich_token().span.end;
                self.skip(Token::RParen)?;

                AstNode::Group {
                    child: Box::new(expression),
                    span,
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
            Token::BooleanTrue => {
                self.advance();
                AstNode::Literal {
                    value: Value::Boolean(true),
                    span: token.span,
                }
            }
            Token::BooleanFalse => {
                self.advance();
                AstNode::Literal {
                    value: Value::Boolean(false),
                    span: token.span,
                }
            }

            kind => {
                return Err(ParsingError::UnexpectedToken {
                    expected: Token::Identifier(String::from("foo")),
                    found: kind.clone(),
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

            if self.current_token_is(Token::RParen) {
                return Ok(args);
            }

            // Parse argument

            let expression = self.parse_expression()?;
            args.push(expression);

            // Continue if encounter "," or break out of loop if encounter ")"

            let token = self.get_current_rich_token();
            match token.kind {
                Token::Comma => {
                    self.skip(Token::Comma)?;
                    continue;
                }

                Token::RParen => continue,

                kind => {
                    // Error if encountering neither "," or ")"

                    return Err(ParsingError::UnexpectedToken {
                        expected: Token::RParen,
                        found: kind.clone(),
                        span: token.span,
                    });
                }
            }
        }
    }

    fn expect_current_token(&mut self, expected: Token) -> Result<(), ParsingError> {
        let current = self.get_current_rich_token();
        if current.kind != expected {
            return Err(ParsingError::UnexpectedToken {
                expected,
                found: current.kind.clone(),
                span: current.span,
            });
        }

        Ok(())
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn skip(&mut self, expected_token: Token) -> Result<(), ParsingError> {
        self.expect_current_token(expected_token)?;
        self.advance();
        Ok(())
    }

    fn get_current_rich_token(&mut self) -> RichToken {
        self.tokens.get(self.cursor).cloned().unwrap()
    }

    fn get_current_token(&self) -> Token {
        self.tokens.get(self.cursor).unwrap().kind.clone()
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.get_current_token() == token
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
        write!(
            f,
            "{}",
            match self {
                Self::UnexpectedToken {
                    expected, found, ..
                } => {
                    format!("expecting to find {expected} but get {found} instead",)
                }
            }
        )
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
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("x"),
                        span: 4..5,
                    }),
                    var_type: None,
                    value: None,
                    span: 0..5,
                },
            ),
            (
                "var abc = 123 * x;",
                AstNode::VariableDeclaration {
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 4..7,
                    }),
                    var_type: None,
                    value: Some(Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Integer(123),
                            span: 10..13,
                        }),
                        rhs: Box::new(AstNode::Identifier {
                            name: String::from("x"),
                            span: 16..17,
                        }),
                        span: 10..17,
                    })),
                    span: 0..17,
                },
            ),
            (
                "var x = (123 + 50);",
                AstNode::VariableDeclaration {
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("x"),
                        span: 4..5,
                    }),
                    var_type: None,
                    value: Some(Box::new(AstNode::Add {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Integer(123),
                            span: 9..12,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            value: Value::Integer(50),
                            span: 15..17,
                        }),
                        span: 9..17,
                    })),
                    span: 0..18,
                },
            ),
            (
                "var x = ((((foo))));",
                AstNode::VariableDeclaration {
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("x"),
                        span: 4..5,
                    }),
                    var_type: None,
                    value: Some(Box::new(AstNode::Identifier {
                        name: String::from("foo"),
                        span: 12..15,
                    })),
                    span: 0..19,
                },
            ),
            (
                "var x: Int;",
                AstNode::VariableDeclaration {
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("x"),
                        span: 4..5,
                    }),
                    var_type: Some(Box::from(AstNode::TypeNotation {
                        name: String::from("Int"),
                        span: 7..10,
                    })),
                    value: None,
                    span: 0..10,
                },
            ),
            (
                "var x: Float = 5;",
                AstNode::VariableDeclaration {
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("x"),
                        span: 4..5,
                    }),
                    var_type: Some(Box::from(AstNode::TypeNotation {
                        name: String::from("Float"),
                        span: 7..12,
                    })),
                    value: Some(Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 15..16,
                    })),
                    span: 0..16,
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
        let cases = [
            (
                "abc = 123 * x;",
                AstNode::Assign {
                    lhs: Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 0..3,
                    }),
                    value: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Integer(123),
                            span: 6..9,
                        }),
                        rhs: Box::new(AstNode::Identifier {
                            name: String::from("x"),
                            span: 12..13,
                        }),
                        span: 6..13,
                    }),
                    span: 0..13,
                },
            ),
            (
                "x = (-5);",
                AstNode::Assign {
                    lhs: Box::new(AstNode::Identifier {
                        name: String::from("x"),
                        span: 0..1,
                    }),
                    value: Box::new(AstNode::Neg {
                        child: Box::new(AstNode::Literal {
                            value: Value::Integer(5),
                            span: 6..7,
                        }),
                        span: 5..7,
                    }),
                    span: 0..8,
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
    fn test_parsing_conditional_branch() {
        let cases = [
            (
                "if 15 > 10 { print(1); }",
                AstNode::If {
                    condition: Box::new(AstNode::Gt {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Integer(15),
                            span: 3..5,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            value: Value::Integer(10),
                            span: 8..10,
                        }),
                        span: 3..10,
                    }),
                    body: vec![AstNode::FunctionCall {
                        callee: Box::new(AstNode::Identifier {
                            name: String::from("print"),
                            span: 13..18,
                        }),
                        args: vec![AstNode::Literal {
                            value: Value::Integer(1),
                            span: 19..20,
                        }],
                        span: 13..21,
                    }],
                    or_else: None,
                    span: 0..24,
                },
            ),
            (
                "if false {} else if false {} else {}",
                AstNode::If {
                    condition: Box::new(AstNode::Literal {
                        value: Value::Boolean(false),
                        span: 3..8,
                    }),
                    body: vec![],
                    or_else: Some(Box::new(AstNode::If {
                        condition: Box::new(AstNode::Literal {
                            value: Value::Boolean(false),
                            span: 20..25,
                        }),
                        body: vec![],
                        or_else: Some(Box::new(AstNode::Else {
                            body: vec![],
                            span: 29..36,
                        })),
                        span: 17..36,
                    })),
                    span: 0..36,
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
    fn test_parsing_while() {
        let cases = [
            (
                "while true {}",
                AstNode::While {
                    condition: Box::new(AstNode::Literal {
                        value: Value::Boolean(true),
                        span: 6..10,
                    }),
                    body: vec![],
                    span: 0..13,
                },
            ),
            (
                "while true { continue; break; }",
                AstNode::While {
                    condition: Box::new(AstNode::Literal {
                        value: Value::Boolean(true),
                        span: 6..10,
                    }),
                    body: vec![
                        AstNode::Continue { span: 13..21 },
                        AstNode::Break { span: 23..28 },
                    ],
                    span: 0..31,
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
    fn test_parsing_expression() {
        let cases = [
            (
                "abc + 512 * 200 - abc / 3;",
                AstNode::Sub {
                    lhs: Box::new(AstNode::Add {
                        lhs: Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 0..3,
                        }),
                        rhs: Box::new(AstNode::Mul {
                            lhs: Box::new(AstNode::Literal {
                                value: Value::Integer(512),
                                span: 6..9,
                            }),
                            rhs: Box::new(AstNode::Literal {
                                value: Value::Integer(200),
                                span: 12..15,
                            }),
                            span: 6..15,
                        }),
                        span: 0..15,
                    }),
                    rhs: Box::new(AstNode::Div {
                        lhs: Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 18..21,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            value: Value::Integer(3),
                            span: 24..25,
                        }),
                        span: 18..25,
                    }),
                    span: 0..25,
                },
            ),
            (
                "50.0 % 2.0;",
                AstNode::Mod {
                    lhs: Box::new(AstNode::Literal {
                        value: Value::Float(50.0),
                        span: 0..4,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        value: Value::Float(2.0),
                        span: 7..10,
                    }),
                    span: 0..10,
                },
            ),
            (
                "(((75)));",
                AstNode::Literal {
                    value: Value::Integer(75),
                    span: 3..5,
                },
            ),
            (
                "(123 - 53) * 7;",
                AstNode::Mul {
                    lhs: Box::new(AstNode::Sub {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Integer(123),
                            span: 1..4,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            value: Value::Integer(53),
                            span: 7..9,
                        }),
                        span: 1..9,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        value: Value::Integer(7),
                        span: 13..14,
                    }),
                    span: 0..14,
                },
            ),
            (
                "123 + (foo - 50);",
                AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        value: Value::Integer(123),
                        span: 0..3,
                    }),
                    rhs: Box::new(AstNode::Sub {
                        lhs: Box::new(AstNode::Identifier {
                            name: String::from("foo"),
                            span: 7..10,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            value: Value::Integer(50),
                            span: 13..15,
                        }),
                        span: 7..15,
                    }),
                    span: 0..16,
                },
            ),
            (
                "abc(123, 50 + 2) * 7;",
                AstNode::Mul {
                    lhs: Box::new(AstNode::FunctionCall {
                        callee: Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 0..3,
                        }),
                        args: vec![
                            AstNode::Literal {
                                value: Value::Integer(123),
                                span: 4..7,
                            },
                            AstNode::Add {
                                lhs: Box::new(AstNode::Literal {
                                    value: Value::Integer(50),
                                    span: 9..11,
                                }),
                                rhs: Box::new(AstNode::Literal {
                                    value: Value::Integer(2),
                                    span: 14..15,
                                }),
                                span: 9..15,
                            },
                        ],
                        span: 0..16,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        value: Value::Integer(7),
                        span: 19..20,
                    }),
                    span: 0..20,
                },
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
                        span: 4..17,
                    }],
                    span: 0..19,
                },
            ),
            (
                "-abc + (-(5)) * -(-7);",
                AstNode::Add {
                    lhs: Box::new(AstNode::Neg {
                        child: Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 1..4,
                        }),
                        span: 0..4,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Neg {
                            child: Box::new(AstNode::Literal {
                                value: Value::Integer(5),
                                span: 10..11,
                            }),
                            span: 8..12,
                        }),
                        rhs: Box::new(AstNode::Neg {
                            child: Box::new(AstNode::Neg {
                                child: Box::new(AstNode::Literal {
                                    value: Value::Integer(7),
                                    span: 19..20,
                                }),
                                span: 18..20,
                            }),
                            span: 16..21,
                        }),
                        span: 7..21,
                    }),
                    span: 0..21,
                },
            ),
            (
                "2 * (-0.5);",
                AstNode::Mul {
                    lhs: Box::new(AstNode::Literal {
                        value: Value::Integer(2),
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode::Neg {
                        child: Box::new(AstNode::Literal {
                            value: Value::Float(0.5),
                            span: 6..9,
                        }),
                        span: 5..9,
                    }),
                    span: 0..10,
                },
            ),
            (
                "-(-(abc)(-foo));",
                AstNode::Neg {
                    child: Box::new(AstNode::Neg {
                        child: Box::new(AstNode::FunctionCall {
                            callee: Box::new(AstNode::Identifier {
                                name: String::from("abc"),
                                span: 4..7,
                            }),
                            args: vec![AstNode::Neg {
                                child: Box::new(AstNode::Identifier {
                                    name: String::from("foo"),
                                    span: 10..13,
                                }),
                                span: 9..13,
                            }],
                            span: 3..14,
                        }),
                        span: 2..14,
                    }),
                    span: 0..15,
                },
            ),
            (
                "true;",
                AstNode::Literal {
                    value: Value::Boolean(true),
                    span: 0..4,
                },
            ),
            (
                "1 >= 5 == true;",
                AstNode::Eq {
                    lhs: Box::new(AstNode::Gte {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Integer(1),
                            span: 0..1,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            value: Value::Integer(5),
                            span: 5..6,
                        }),
                        span: 0..6,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        value: Value::Boolean(true),
                        span: 10..14,
                    }),
                    span: 0..14,
                },
            ),
            (
                "false || !false && true;",
                AstNode::And {
                    lhs: Box::new(AstNode::Or {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Boolean(false),
                            span: 0..5,
                        }),
                        rhs: Box::new(AstNode::Not {
                            child: Box::new(AstNode::Literal {
                                value: Value::Boolean(false),
                                span: 10..15,
                            }),
                            span: 9..15,
                        }),
                        span: 0..15,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        value: Value::Boolean(true),
                        span: 19..23,
                    }),
                    span: 0..23,
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
