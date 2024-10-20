//! This module contains the required logic operations during the
//! parsing stage of a Kaba tokens.

use crate::{
    ast::{AstNode, Program as ProgramAst, Value},
    lexer::{Token, TokenKind},
};
use logos::Span;
use std::fmt::Display;

/// Provide a quick way to parse Kaba tokens, without the needs to
/// setting up and running the parser manually.
///
/// Produces an AST that represents the entire source code of the
/// given tokens (see [`crate::ast::Program`]).
pub fn parse(tokens: Vec<Token>) -> Result<ProgramAst, ParsingError> {
    Parser::new(tokens).parse()
}

struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn parse(&mut self) -> Result<ProgramAst, ParsingError> {
        let mut stmts = vec![];

        loop {
            if self.current_token_is(&TokenKind::Eof) {
                break;
            }
            let stmt = self.parse_statement()?;
            stmts.push(stmt)
        }

        Ok(ProgramAst { stmts })
    }

    /// Parse a code block.
    ///
    /// Block is an area of code between `do` and `end` keywords and may
    /// contains statements. All symbols defined inside a block are stored
    /// in a dedicated scope. Below is an example of a loop block:
    ///
    /// ```text
    /// while true do
    ///   print(true)
    /// end
    /// ```
    ///
    /// Block may also ended by another token (`possible_delimiter`). For
    /// example, in a conditional `if ... else ...` block, it would look
    /// like this:
    ///
    /// ```text
    /// if false do
    ///   print(false)
    /// else do     # note that there is no `end` before `else` keyword
    ///   print(true)
    /// end
    /// ```
    fn parse_block(
        &mut self,
        possible_delimiter: Option<TokenKind>,
    ) -> Result<(Vec<AstNode>, Span), ParsingError> {
        // Expecting "do"

        let start = self.get_current_token().span.start;
        self.skip(&TokenKind::Do)?;

        // Expecting 0 >= statements, delimited with "end" or
        // "additional_delimiter"

        let mut stmts = vec![];
        loop {
            if let Some(tok) = &possible_delimiter {
                if self.current_token_is(tok) {
                    break;
                }
            }

            if self.current_token_is(&TokenKind::End) {
                break;
            } else if self.current_token_is(&TokenKind::Eof) {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::End,
                    found: TokenKind::Eof,
                    span: self.get_current_token().span,
                });
            }

            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        // Expecting "end" or "additional delimiter"

        let end = self.get_current_token().span.end;

        // skip *only if* delimiter is not "end"
        if self.current_token_is(&TokenKind::End) {
            self.skip(&TokenKind::End)?;
        }

        Ok((stmts, start..end))
    }

    /// Parse a statement.
    ///
    /// Statement is a piece of code that ended with a semicolon ";"
    /// symbol or ended with a block.
    ///
    /// 1. Example of a statement that ended with semicolon:
    ///
    /// ```text
    /// var x = 5;
    /// ```
    ///
    /// 2. Example of a statement that ended with a block:
    ///
    /// ```text
    /// if true do
    ///   # this is a block
    /// end
    /// ```
    fn parse_statement(&mut self) -> Result<AstNode, ParsingError> {
        // Check if statement starts with a keyword

        match self.get_current_token_kind() {
            TokenKind::Var => return self.parse_variable_declaration(),
            TokenKind::If => return self.parse_conditional_branch(),
            TokenKind::While => return self.parse_while(),
            TokenKind::Break | TokenKind::Continue => return self.parse_loop_control(),
            TokenKind::Fn => return self.parse_function_definition(),
            TokenKind::Return => return self.parse_return_statement(),
            TokenKind::Debug => return self.parse_debug_statement(),
            _ => (),
        }

        // Expecting expression

        let expr = self.parse_expression()?;

        // Expecting ";"

        self.skip(&TokenKind::Semicolon)?;

        Ok(expr.unwrap_group())
    }

    fn parse_variable_declaration(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_token().span.start;
        let mut end;

        self.skip(&TokenKind::Var)?;

        // Parse identifier

        let token = self.get_current_token();
        let id = match token.kind {
            TokenKind::Identifier(name) => Box::new(AstNode::Identifier {
                name,
                span: token.span.clone(),
            }),
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::Identifier(String::from("foo")),
                    found: token.kind.clone(),
                    span: token.span,
                });
            }
        };

        end = token.span.end;
        self.advance();

        // Expecting ":" (optional)

        let tn = if self.current_token_is(&TokenKind::Colon) {
            self.skip(&TokenKind::Colon)?;

            let tn = self.parse_type_notation()?;
            end = tn.span().end;

            Some(Box::new(tn))
        } else {
            None
        };

        // Expecting "=" (optional)

        let val = if self.current_token_is(&TokenKind::Assign) {
            self.skip(&TokenKind::Assign)?;

            let expr = self.parse_expression()?;
            end = expr.span().end;

            Some(Box::new(expr.unwrap_group()))
        } else {
            None
        };

        // Expecting ";"

        self.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::VariableDeclaration {
            id,
            tn,
            val,
            span: start..end,
        })
    }

    fn parse_type_notation(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.get_current_token();
        match token.kind {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(AstNode::TypeNotation {
                    identifier: Box::new(AstNode::Identifier {
                        name,
                        span: token.span.clone(),
                    }),
                    span: token.span.clone(),
                })
            }
            _ => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Identifier(String::from("foo")),
                found: token.kind.clone(),
                span: token.span,
            }),
        }
    }

    fn parse_conditional_branch(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_token().span.start;
        let mut end;
        self.skip(&TokenKind::If)?;

        // Expecting expression

        let cond = Box::new(self.parse_expression()?);

        // Expecting block

        let (body, block_span) = self.parse_block(Some(TokenKind::Else))?;
        end = block_span.end;

        // Expecting >= 0 "else if" or 1 "else"

        let or_else = if self.current_token_is(&TokenKind::Else) {
            let else_start = self.get_current_token().span.start;
            self.skip(&TokenKind::Else)?;

            let token = self.get_current_token();
            match token.kind {
                TokenKind::If => {
                    // Expecting "else if ..." statement

                    let alt = self.parse_conditional_branch()?;
                    end = alt.span().end;

                    Some(Box::new(alt))
                }

                TokenKind::Do => {
                    // Expecting block

                    let (body, block_span) = self.parse_block(Some(TokenKind::Else))?;
                    let else_end = block_span.end;
                    end = else_end;

                    Some(Box::new(AstNode::Else {
                        body,
                        span: else_start..else_end,
                    }))
                }

                kind => {
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::Else,
                        found: kind.clone(),
                        span: token.span,
                    });
                }
            }
        } else {
            None
        };

        Ok(AstNode::If {
            cond,
            body,
            or_else,
            span: start..end,
        })
    }

    fn parse_while(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_token().span.start;
        self.skip(&TokenKind::While)?;

        // Expecting expression

        let cond = Box::new(self.parse_expression()?);

        // Expecting block

        let (body, block_span) = self.parse_block(None)?;
        let end = block_span.end;

        Ok(AstNode::While {
            cond,
            body,
            span: start..end,
        })
    }

    fn parse_loop_control(&mut self) -> Result<AstNode, ParsingError> {
        let Token { kind, span, .. } = self.get_current_token();

        // Expecting either "break" or "continue" keyword

        let control = match kind {
            TokenKind::Break => AstNode::Break { span },
            TokenKind::Continue => AstNode::Continue { span },
            _ => unreachable!(),
        };

        self.advance();

        // Expecting ";"

        self.skip(&TokenKind::Semicolon)?;

        Ok(control)
    }

    fn parse_function_definition(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_token().span.start;
        self.skip(&TokenKind::Fn)?;

        // Expecting identifier

        let token = self.get_current_token();
        let id = match token.kind {
            TokenKind::Identifier(name) => {
                self.advance();
                Box::new(AstNode::Identifier {
                    name,
                    span: token.span,
                })
            }

            kind => {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::Identifier(String::from("function_name")),
                    found: kind.clone(),
                    span: token.span,
                });
            }
        };

        // Expecting "("

        self.skip(&TokenKind::LParen)?;

        // Followed by >= 0 function parameter declaration(s)

        let mut params = vec![];
        while !self.current_token_is(&TokenKind::RParen) {
            // Expecting identifier

            let token = self.get_current_token();
            let param = match token.kind {
                TokenKind::Identifier(name) => {
                    self.advance();
                    AstNode::Identifier {
                        name,
                        span: token.span,
                    }
                }

                kind => {
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::Identifier(String::from("function_name")),
                        found: kind.clone(),
                        span: token.span,
                    });
                }
            };

            // Expecting ":"

            self.skip(&TokenKind::Colon)?;

            // Expecting type notation

            let param_t = self.parse_type_notation()?;

            // Add to parameter list

            params.push((param, param_t));

            // Expecting either "," or ")"

            let token = self.get_current_token();
            match token.kind {
                TokenKind::Comma => {
                    self.skip(&TokenKind::Comma)?;
                    continue;
                }

                TokenKind::RParen => continue,

                kind => {
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                        span: token.span,
                    });
                }
            }
        }

        // Expecting ")"

        self.skip(&TokenKind::RParen)?;

        // Expecting return type notation (optional)

        let return_t = if self.current_token_is(&TokenKind::Colon) {
            self.skip(&TokenKind::Colon)?;

            Some(Box::new(self.parse_type_notation()?))
        } else {
            None
        };

        // Expecting function body

        let (body, body_span) = self.parse_block(None)?;

        Ok(AstNode::FunctionDefinition {
            id,
            params,
            return_t,
            body,
            span: start..body_span.end,
        })
    }

    fn parse_return_statement(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_token().span.start;
        let mut end = self.get_current_token().span.end;
        self.skip(&TokenKind::Return)?;

        // Expecting expression (optional)

        let expr = if self.current_token_is(&TokenKind::Semicolon) {
            None
        } else {
            let expr = self.parse_expression()?;
            end = expr.span().end;
            Some(Box::new(expr))
        };

        // Expecting ";"

        self.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::Return {
            expr,
            span: start..end,
        })
    }

    fn parse_debug_statement(&mut self) -> Result<AstNode, ParsingError> {
        let start = self.get_current_token().span.start;
        self.skip(&TokenKind::Debug)?;

        // Expecting expression

        let expr = Box::new(self.parse_expression()?);
        let end = expr.span().end;

        // Expecting ";"

        self.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::Debug {
            expr,
            span: start..end,
        })
    }

    fn parse_expression(&mut self) -> Result<AstNode, ParsingError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let lhs = self.parse_logical_and_or_expression()?;

        // Expecting "=", "+=", "-=", "*=", "/=", or "%=" (optional)

        match self.get_current_token_kind() {
            TokenKind::Assign => {
                self.skip(&TokenKind::Assign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Assign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::AddAssign => {
                self.skip(&TokenKind::AddAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::AddAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::SubAssign => {
                self.skip(&TokenKind::SubAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::SubAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::MulAssign => {
                self.skip(&TokenKind::MulAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::MulAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::DivAssign => {
                self.skip(&TokenKind::DivAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::DivAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::ModAssign => {
                self.skip(&TokenKind::ModAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::ModAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            _ => Ok(lhs),
        }
    }

    fn parse_logical_and_or_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut lhs = self.parse_equality_expression()?;

        loop {
            // Expecting "||" or "&&" (both are optional)

            match self.get_current_token_kind() {
                TokenKind::Or => {
                    self.skip(&TokenKind::Or)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Or {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::And => {
                    self.skip(&TokenKind::And)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::And {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_equality_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut lhs = self.parse_comparison_expression()?;

        loop {
            // Expecting "==" or "!=" (both are optional)

            match self.get_current_token_kind() {
                TokenKind::Eq => {
                    self.skip(&TokenKind::Eq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Eq {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Neq => {
                    self.skip(&TokenKind::Neq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Neq {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_comparison_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let lhs = self.parse_additive_expression()?;

        // Expecting ">", ">=", "<" or "<=" (all are optional)

        match self.get_current_token_kind() {
            TokenKind::Gt => {
                self.skip(&TokenKind::Gt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Gte => {
                self.skip(&TokenKind::Gte)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gte {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lt => {
                self.skip(&TokenKind::Lt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Lt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lte => {
                self.skip(&TokenKind::Lte)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Lte {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            _ => Ok(lhs),
        }
    }

    fn parse_additive_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut lhs = self.parse_multiplicative_expression()?;

        loop {
            // Expecting "+" or "-" (both are optional)

            match self.get_current_token_kind() {
                TokenKind::Add => {
                    self.skip(&TokenKind::Add)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Add {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Sub => {
                    self.skip(&TokenKind::Sub)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Sub {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_multiplicative_expression(&mut self) -> Result<AstNode, ParsingError> {
        // Parse first term

        let mut lhs = self.parse_unary_expression()?;

        loop {
            // Expecting "*", "/" or "%" (all are optional)

            match self.get_current_token_kind() {
                TokenKind::Mul => {
                    self.skip(&TokenKind::Mul)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Mul {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Div => {
                    self.skip(&TokenKind::Div)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Div {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Mod => {
                    self.skip(&TokenKind::Mod)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Mod {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_unary_expression(&mut self) -> Result<AstNode, ParsingError> {
        //  Prefixed by >= 0 negation or not expression

        if self.current_token_is(&TokenKind::Sub) {
            return self.parse_prefix_expression(&TokenKind::Sub);
        } else if self.current_token_is(&TokenKind::Not) {
            return self.parse_prefix_expression(&TokenKind::Not);
        }

        // Parse primary expression

        let mut child = self.parse_primary_expression()?;

        // Followed by >= 0 function call, field access, or indexed access

        // TODO: field access and indexed access

        #[allow(clippy::while_let_loop)] // temporary
        loop {
            match self.get_current_token_kind() {
                TokenKind::LParen => {
                    let callee_start = child.span().start;
                    self.skip(&TokenKind::LParen)?;

                    let args = self.parse_function_call()?;

                    let span = callee_start..self.get_current_token().span.end;
                    self.skip(&TokenKind::RParen)?;

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

    fn parse_prefix_expression(&mut self, token: &TokenKind) -> Result<AstNode, ParsingError> {
        let start = self.get_current_token().span.start;
        self.skip(token)?;

        let child = self.parse_unary_expression()?;
        let span = start..child.span().end;

        match token {
            TokenKind::Sub => Ok(AstNode::Neg {
                child: Box::new(child.unwrap_group()),
                span,
            }),
            TokenKind::Not => Ok(AstNode::Not {
                child: Box::new(child.unwrap_group()),
                span,
            }),

            _ => unreachable!(),
        }
    }

    fn parse_primary_expression(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.get_current_token();

        Ok(match token.kind {
            TokenKind::LParen => {
                // Parse group expression

                let lparen_start = token.span.start;
                self.skip(&TokenKind::LParen)?;

                let expression = self.parse_expression()?;

                let span = lparen_start..self.get_current_token().span.end;
                self.skip(&TokenKind::RParen)?;

                AstNode::Group {
                    child: Box::new(expression),
                    span,
                }
            }

            // Expecting either identifier or literals
            TokenKind::Identifier(name) => {
                self.advance();
                AstNode::Identifier {
                    name,
                    span: token.span,
                }
            }
            TokenKind::Integer(n) => {
                self.advance();
                AstNode::Literal {
                    value: Value::Integer(n),
                    span: token.span,
                }
            }
            TokenKind::Float(n) => {
                self.advance();
                AstNode::Literal {
                    value: Value::Float(n),
                    span: token.span,
                }
            }
            TokenKind::BooleanTrue => {
                self.advance();
                AstNode::Literal {
                    value: Value::Boolean(true),
                    span: token.span,
                }
            }
            TokenKind::BooleanFalse => {
                self.advance();
                AstNode::Literal {
                    value: Value::Boolean(false),
                    span: token.span,
                }
            }

            kind => {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::Identifier(String::from("foo")),
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

            if self.current_token_is(&TokenKind::RParen) {
                return Ok(args);
            }

            // Parse argument

            args.push(self.parse_expression()?);

            // Continue if encounter "," or break out of loop if encounter ")"

            let token = self.get_current_token();
            match token.kind {
                TokenKind::Comma => {
                    self.skip(&TokenKind::Comma)?;
                    continue;
                }

                TokenKind::RParen => continue,

                kind => {
                    // Error if encountering neither "," or ")"

                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                        span: token.span,
                    });
                }
            }
        }
    }

    fn expect_current_token(&mut self, expect: &TokenKind) -> Result<(), ParsingError> {
        let current = self.get_current_token();
        if &current.kind != expect {
            return Err(ParsingError::UnexpectedToken {
                expect: expect.clone(),
                found: current.kind.clone(),
                span: current.span,
            });
        }

        Ok(())
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn skip(&mut self, expected_token: &TokenKind) -> Result<(), ParsingError> {
        self.expect_current_token(expected_token)?;
        self.advance();
        Ok(())
    }

    fn get_current_token(&mut self) -> Token {
        self.tokens.get(self.cursor).cloned().unwrap()
    }

    fn get_current_token_kind(&self) -> TokenKind {
        self.tokens.get(self.cursor).unwrap().kind.clone()
    }

    fn current_token_is(&self, token: &TokenKind) -> bool {
        &self.get_current_token_kind() == token
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedToken {
        expect: TokenKind,
        found: TokenKind,
        span: Span,
    },
}

impl ParsingError {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::UnexpectedToken { span, .. } => Some(span.clone()),
        }
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expect, found, .. } => {
                write!(f, "expecting to find {expect} but get {found} instead",)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    fn parse_and_assert_result(input: &str, expect: AstNode) {
        let tokens = lexer::lex(input).unwrap();
        let result = parse(tokens);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            ProgramAst {
                stmts: vec![expect]
            }
        );
    }

    //
    // Test variable declarations
    //

    #[test]
    fn test_parsing_without_type_annotation_and_initial_value() {
        parse_and_assert_result(
            "var x;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: None,
                val: None,
                span: 0..5,
            },
        );
    }

    #[test]
    fn test_parsing_without_t_annotation_but_with_initial_value() {
        parse_and_assert_result(
            "var abc = 123 * x;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("abc"),
                    span: 4..7,
                }),
                tn: None,
                val: Some(Box::new(AstNode::Mul {
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
        );
    }

    #[test]
    fn test_parsing_with_grouped_expression_as_initial_value() {
        parse_and_assert_result(
            "var x = (123 + 50);",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: None,
                val: Some(Box::new(AstNode::Add {
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
        );
    }

    #[test]
    fn test_parsing_with_nested_grouped_expression_as_initial_value() {
        parse_and_assert_result(
            "var x = ((((foo))));",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: None,
                val: Some(Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 12..15,
                })),
                span: 0..19,
            },
        );
    }

    #[test]
    fn test_parsing_with_type_annotation_but_without_initial_value() {
        parse_and_assert_result(
            "var x: Int;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("Int"),
                        span: 7..10,
                    }),
                    span: 7..10,
                })),
                val: None,
                span: 0..10,
            },
        );
    }

    #[test]
    fn test_parsing_with_both_type_annotation_and_initial_value() {
        parse_and_assert_result(
            "var x: Int = 5;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    identifier: Box::from(AstNode::Identifier {
                        name: String::from("Int"),
                        span: 7..10,
                    }),
                    span: 7..10,
                })),
                val: Some(Box::new(AstNode::Literal {
                    value: Value::Integer(5),
                    span: 13..14,
                })),
                span: 0..14,
            },
        );
    }

    //
    // Test conditional branches
    //

    #[test]
    fn test_parsing_if_statement() {
        parse_and_assert_result(
            "if 15 > 10 do print(1); end",
            AstNode::If {
                cond: Box::new(AstNode::Gt {
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
                        span: 14..19,
                    }),
                    args: vec![AstNode::Literal {
                        value: Value::Integer(1),
                        span: 20..21,
                    }],
                    span: 14..22,
                }],
                or_else: None,
                span: 0..27,
            },
        );
    }

    #[test]
    fn test_parsing_if_else_branches() {
        parse_and_assert_result(
            "if false do else if false do else do end",
            AstNode::If {
                cond: Box::new(AstNode::Literal {
                    value: Value::Boolean(false),
                    span: 3..8,
                }),
                body: vec![],
                or_else: Some(Box::new(AstNode::If {
                    cond: Box::new(AstNode::Literal {
                        value: Value::Boolean(false),
                        span: 20..25,
                    }),
                    body: vec![],
                    or_else: Some(Box::new(AstNode::Else {
                        body: vec![],
                        span: 29..40,
                    })),
                    span: 17..40,
                })),
                span: 0..40,
            },
        );
    }

    //
    // Test loops
    //

    #[test]
    fn test_parsing_while_statement() {
        parse_and_assert_result(
            "while true do end",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    value: Value::Boolean(true),
                    span: 6..10,
                }),
                body: vec![],
                span: 0..17,
            },
        );
    }

    #[test]
    fn test_parsing_while_statement_with_control_loops() {
        parse_and_assert_result(
            "while true do continue; break; end",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    value: Value::Boolean(true),
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

    //
    // Test function definitions
    //

    #[test]
    fn test_parsing_empty_function_definition() {
        parse_and_assert_result(
            "fn foo() do end",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                params: vec![],
                return_t: None,
                body: vec![],
                span: 0..15,
            },
        );
    }

    #[test]
    fn test_parsing_function_definition_with_parameters_and_trailing_comma() {
        parse_and_assert_result(
            "fn foo(x: Int, y: Bool,) do end",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                params: vec![
                    (
                        AstNode::Identifier {
                            name: String::from("x"),
                            span: 7..8,
                        },
                        AstNode::TypeNotation {
                            identifier: Box::new(AstNode::Identifier {
                                name: String::from("Int"),
                                span: 10..13,
                            }),
                            span: 10..13,
                        },
                    ),
                    (
                        AstNode::Identifier {
                            name: String::from("y"),
                            span: 15..16,
                        },
                        AstNode::TypeNotation {
                            identifier: Box::new(AstNode::Identifier {
                                name: String::from("Bool"),
                                span: 18..22,
                            }),
                            span: 18..22,
                        },
                    ),
                ],
                return_t: None,
                body: vec![],
                span: 0..31,
            },
        );
    }

    #[test]
    fn test_parsing_function_definition_parameter_and_body() {
        parse_and_assert_result(
            "fn write(x: Int) do print(x); end",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("write"),
                    span: 3..8,
                }),
                params: vec![(
                    AstNode::Identifier {
                        name: String::from("x"),
                        span: 9..10,
                    },
                    AstNode::TypeNotation {
                        identifier: Box::new(AstNode::Identifier {
                            name: String::from("Int"),
                            span: 12..15,
                        }),
                        span: 12..15,
                    },
                )],
                return_t: None,
                body: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Identifier {
                        name: String::from("print"),
                        span: 20..25,
                    }),
                    args: vec![AstNode::Identifier {
                        name: String::from("x"),
                        span: 26..27,
                    }],
                    span: 20..28,
                }],
                span: 0..33,
            },
        );
    }

    #[test]
    fn test_parsing_function_definition_with_return_statement() {
        parse_and_assert_result(
            "fn foo(): Int do return 5; end",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                params: vec![],
                return_t: Some(Box::new(AstNode::TypeNotation {
                    identifier: Box::new(AstNode::Identifier {
                        name: String::from("Int"),
                        span: 10..13,
                    }),
                    span: 10..13,
                })),
                body: vec![AstNode::Return {
                    expr: Some(Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 24..25,
                    })),
                    span: 17..25,
                }],
                span: 0..30,
            },
        );
    }

    //
    // Test debug
    //

    #[test]
    fn test_parsing_debug_statement() {
        parse_and_assert_result(
            "debug 5 + 5 * 7;",
            AstNode::Debug {
                expr: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 6..7,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            value: Value::Integer(5),
                            span: 10..11,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            value: Value::Integer(7),
                            span: 14..15,
                        }),
                        span: 10..15,
                    }),
                    span: 6..15,
                }),
                span: 0..15,
            },
        )
    }

    //
    // Test expressions
    //

    #[test]
    fn test_parsing_math_expression() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_assignment_to_variable() {
        parse_and_assert_result(
            "abc = 123 * x;",
            AstNode::Assign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("abc"),
                    span: 0..3,
                }),
                rhs: Box::new(AstNode::Mul {
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
        );
    }

    #[test]
    fn test_parsing_assigning_negative_value_to_variable() {
        parse_and_assert_result(
            "x = (-5);",
            AstNode::Assign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 6..7,
                    }),
                    span: 5..7,
                }),
                span: 0..8,
            },
        );
    }

    #[test]
    fn test_parsing_add_assign_to_variable() {
        parse_and_assert_result(
            "x += (-5);",
            AstNode::AddAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn test_parsing_sub_assign_to_variable() {
        parse_and_assert_result(
            "x -= (-5);",
            AstNode::SubAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn test_parsing_mul_assign_to_variable() {
        parse_and_assert_result(
            "x *= (-5);",
            AstNode::MulAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn test_parsing_div_assign_to_variable() {
        parse_and_assert_result(
            "x /= (-5);",
            AstNode::DivAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn test_parsing_mod_assign_to_variable() {
        parse_and_assert_result(
            "x %= (-5);",
            AstNode::ModAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        value: Value::Integer(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn test_parsing_mod_operation_between_float_numbers() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_left_grouped_expression() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_right_grouped_expression() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_nested_grouped_expression() {
        parse_and_assert_result(
            "(((75)));",
            AstNode::Literal {
                value: Value::Integer(75),
                span: 3..5,
            },
        );
    }

    #[test]
    fn test_parsing_function_call_in_math_expression() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_nested_function_call() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_negative_operations_in_math_expression() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_negating_function_call() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_boolean_literal() {
        parse_and_assert_result(
            "true;",
            AstNode::Literal {
                value: Value::Boolean(true),
                span: 0..4,
            },
        );
    }

    #[test]
    fn test_parsing_comparison_and_equality_operations() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_boolean_expression() {
        parse_and_assert_result(
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
        );
    }

    #[test]
    fn test_parsing_grouped_expression_with_no_closing_tag() {
        let input = "(123 + 5;";
        let tokens = lexer::lex(input).unwrap();
        let result = parse(tokens);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            ParsingError::UnexpectedToken {
                expect: TokenKind::RParen,
                found: TokenKind::Semicolon,
                span: 8..9,
            }
        );
    }
}
