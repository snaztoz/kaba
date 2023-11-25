use std::fmt::Display;

use crate::ast::{AstNode, Program as ProgramAst};
use crate::lexer::{RichToken, Token};

pub fn parse(tokens: Vec<RichToken>) -> Result<ProgramAst, ParserError> {
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

    fn parse(&mut self) -> Result<ProgramAst, ParserError> {
        let mut statements = vec![];

        while self.cursor < self.tokens.len() {
            let statement = self.parse_statement()?;
            statements.push(statement)
        }

        Ok(ProgramAst { statements })
    }

    fn parse_statement(&mut self) -> Result<AstNode, ParserError> {
        // Check if statement starts with a keyword

        if self.tokens[self.cursor].kind == Token::Var {
            return self.parse_variable_declaration();
        } else {
            // TODO: support other statements
        }

        // Expecting expression

        let mut expression = self.parse_expression()?;

        // Expecting "=" (optional, transform to: variable assignment)

        let token = &self.tokens[self.cursor];
        if token.kind == Token::Assign {
            self.cursor += 1;
            let rhs = self.parse_expression()?;

            expression = AstNode::ValueAssignment {
                lhs: Box::from(expression),
                value: Box::from(rhs),
            };
        }

        self.assert_current_is_semicolon_and_advance()?;

        Ok(expression)
    }

    fn parse_variable_declaration(&mut self) -> Result<AstNode, ParserError> {
        self.cursor += 1; // skip "var" keyword

        // Parse identifier

        let token = &self.tokens[self.cursor];
        let identifier = if token.kind == Token::Identifier {
            token.value.clone()
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: Token::Identifier,
                found: token.kind.clone(),
                pos: token.range.start,
            });
        };

        self.cursor += 1;

        // Expecting ":" (optional)

        let token = &self.tokens[self.cursor];
        let r#type = if token.kind == Token::Colon {
            self.cursor += 1;
            todo!("implementing variable type notation")
        } else {
            None
        };

        // Expecting "=" (optional)

        let token = &self.tokens[self.cursor];
        let value = if token.kind == Token::Assign {
            self.cursor += 1;
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

    fn parse_expression(&mut self) -> Result<AstNode, ParserError> {
        // TODO: make this rule starts from higher rule

        self.parse_additive_expression()
    }

    fn parse_additive_expression(&mut self) -> Result<AstNode, ParserError> {
        // Parse first term

        let mut node = self.parse_multiplicative_expression()?;

        loop {
            // Expecting "+" or "-" (both are optional)

            let token = &self.tokens[self.cursor];
            match token.kind {
                Token::Add => {
                    self.cursor += 1;
                    let rhs = self.parse_multiplicative_expression()?;
                    let add = AstNode::Add(Box::from(node), Box::from(rhs));
                    node = add;
                }
                Token::Sub => {
                    self.cursor += 1;
                    let rhs = self.parse_multiplicative_expression()?;
                    let sub = AstNode::Sub(Box::from(node), Box::from(rhs));
                    node = sub;
                }
                _ => return Ok(node),
            }
        }
    }

    fn parse_multiplicative_expression(&mut self) -> Result<AstNode, ParserError> {
        // Parse first term

        let mut node = self.parse_primary_expression()?;

        loop {
            // Expecting "*" or "/" (both are optional)

            let token = &self.tokens[self.cursor];
            match token.kind {
                Token::Mul => {
                    self.cursor += 1;
                    let rhs = self.parse_primary_expression()?;
                    let mul = AstNode::Mul(Box::from(node), Box::from(rhs));
                    node = mul;
                }
                Token::Div => {
                    self.cursor += 1;
                    let rhs = self.parse_primary_expression()?;
                    let div = AstNode::Div(Box::from(node), Box::from(rhs));
                    node = div;
                }
                _ => return Ok(node),
            }
        }
    }

    fn parse_primary_expression(&mut self) -> Result<AstNode, ParserError> {
        // TODO: expand this rule

        let token = &self.tokens[self.cursor];
        let mut node = match &token.kind {
            Token::LParen => {
                // Parse prioritized expression
                self.cursor += 1;
                self.parse_expression()?
            }

            // Expecting either identifier or integer
            Token::Identifier => AstNode::Identifier(token.value.clone()),
            Token::Integer(n) => AstNode::Integer(*n),

            k => {
                return Err(ParserError::UnexpectedToken {
                    expected: Token::Identifier,
                    found: k.clone(),
                    pos: token.range.start,
                });
            }
        };

        self.cursor += 1;

        // Followed by >= 0 function call, field access, indexed access

        // TODO: field access and indexed access

        loop {
            let token = &self.tokens[self.cursor];
            match token.kind {
                Token::LParen => {
                    self.cursor += 1;
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

    fn parse_function_call(&mut self) -> Result<Vec<AstNode>, ParserError> {
        // Can have >= 0 arguments

        let mut args = vec![];

        loop {
            // Stop when encounter a closing parentheses

            let token = &self.tokens[self.cursor];
            if token.kind == Token::RParen {
                self.cursor += 1;
                return Ok(args);
            }

            // Parse argument

            let expression = self.parse_expression()?;
            args.push(expression);

            // Continue if encounter "," or break out of loop if encounter ")"

            let token = &self.tokens[self.cursor];
            if token.kind == Token::Comma {
                self.cursor += 1;
                continue;
            } else if token.kind == Token::RParen {
                continue;
            }

            // Error if encounter neither "," or ")"

            return Err(ParserError::UnexpectedToken {
                expected: Token::RParen,
                found: token.kind.clone(),
                pos: token.range.start,
            });
        }
    }

    fn assert_current_is_semicolon_and_advance(&mut self) -> Result<(), ParserError> {
        let token = &self.tokens[self.cursor];
        if token.kind != Token::Semicolon {
            return Err(ParserError::UnexpectedToken {
                expected: Token::Semicolon,
                found: token.kind.clone(),
                pos: token.range.start,
            });
        }
        self.cursor += 1;
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedToken {
        expected: Token,
        found: Token,
        pos: usize,
    },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken {
                expected, found, ..
            } => writeln!(
                f,
                "expecting to find token {}, but get {} instead",
                expected, found
            ),
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
                    value: Some(Box::from(AstNode::Mul(
                        Box::from(AstNode::Integer(123)),
                        Box::from(AstNode::Identifier(String::from("x"))),
                    ))),
                },
            ),
        ];

        for (input, expecting) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = Parser::new(tokens).parse();

            assert!(result.is_ok());
            assert_eq!(
                result.unwrap(),
                ProgramAst {
                    statements: vec![expecting]
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
                    Box::from(AstNode::Integer(123)),
                    Box::from(AstNode::Identifier(String::from("x"))),
                )),
            },
        )];

        for (input, expecting) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = Parser::new(tokens).parse();

            assert!(result.is_ok());
            assert_eq!(
                result.unwrap(),
                ProgramAst {
                    statements: vec![expecting]
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
                            Box::from(AstNode::Integer(512)),
                            Box::from(AstNode::Integer(200)),
                        )),
                    )),
                    Box::from(AstNode::Div(
                        Box::from(AstNode::Identifier(String::from("abc"))),
                        Box::from(AstNode::Integer(3)),
                    )),
                ),
            ),
            (
                "(123 - 53) * 7;",
                AstNode::Mul(
                    Box::from(AstNode::Sub(
                        Box::from(AstNode::Integer(123)),
                        Box::from(AstNode::Integer(53)),
                    )),
                    Box::from(AstNode::Integer(7)),
                ),
            ),
            (
                "abc(123, 50 + 2) * 7;",
                AstNode::Mul(
                    Box::from(AstNode::FunctionCall {
                        callee: Box::from(AstNode::Identifier(String::from("abc"))),
                        args: vec![
                            AstNode::Integer(123),
                            AstNode::Add(
                                Box::from(AstNode::Integer(50)),
                                Box::from(AstNode::Integer(2)),
                            ),
                        ],
                    }),
                    Box::from(AstNode::Integer(7)),
                ),
            ),
            (
                "abc(xyz(123, 456),);",
                AstNode::FunctionCall {
                    callee: Box::from(AstNode::Identifier(String::from("abc"))),
                    args: vec![AstNode::FunctionCall {
                        callee: Box::from(AstNode::Identifier(String::from("xyz"))),
                        args: vec![AstNode::Integer(123), AstNode::Integer(456)],
                    }],
                },
            ),
        ];

        for (input, expecting) in cases {
            let tokens = lexer::lex(input).unwrap();
            let result = Parser::new(tokens).parse();

            assert!(result.is_ok());
            assert_eq!(
                result.unwrap(),
                ProgramAst {
                    statements: vec![expecting]
                }
            );
        }
    }
}
