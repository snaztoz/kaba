//! This module contains the required logic operations during the
//! parsing stage of a Kaba tokens.

use crate::{
    ast::{AstNode, FunctionParam, Literal, TypeNotation},
    lexer::{Token, TokenKind},
};
use logos::Span;
use std::fmt::Display;
use stream::TokenStream;

mod stream;

type Result<T> = std::result::Result<T, ParsingError>;

/// Provide a quick way to parse Kaba tokens, without the needs to
/// setting up and running the parser manually.
///
/// Produces an AST that represents the entire source code of the
/// given tokens (see [`crate::ast::Program`]).
pub fn parse(tokens: Vec<Token>) -> Result<AstNode> {
    Parser::new(tokens).parse()
}

struct Parser {
    tokens: TokenStream,
}

impl Parser {
    const fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: TokenStream::new(tokens),
        }
    }

    fn parse(&self) -> Result<AstNode> {
        let mut body = vec![];

        while !self.tokens.current_is(&TokenKind::Eof) {
            let stmt = self.parse_statement()?;
            body.push(stmt)
        }

        Ok(AstNode::Program { body })
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
    fn parse_block(&self, possible_delimiter: Option<TokenKind>) -> Result<(Vec<AstNode>, Span)> {
        // Expecting "do"

        let start = self.tokens.current().span.start;
        self.tokens.skip(&TokenKind::Do)?;

        // Expecting 0 >= statements, delimited with "end" or
        // "additional_delimiter"

        let mut stmts = vec![];
        loop {
            if let Some(tok) = &possible_delimiter {
                if self.tokens.current_is(tok) {
                    break;
                }
            }

            if self.tokens.current_is(&TokenKind::End) {
                break;
            } else if self.tokens.current_is(&TokenKind::Eof) {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::End,
                    found: TokenKind::Eof,
                    span: self.tokens.current().span,
                });
            }

            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        // Expecting "end" or "additional delimiter"

        let end = self.tokens.current().span.end;

        // skip *only if* delimiter is not "end"
        if self.tokens.current_is(&TokenKind::End) {
            self.tokens.skip(&TokenKind::End)?;
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
    fn parse_statement(&self) -> Result<AstNode> {
        // Check if statement starts with a keyword

        match self.tokens.current_kind() {
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

        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(expr.unwrap_group())
    }

    fn parse_variable_declaration(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        self.tokens.skip(&TokenKind::Var)?;

        // Parse identifier

        let token = self.tokens.current();
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

        self.tokens.advance();

        // Expecting ":" (optional)

        let tn = if self.tokens.current_is(&TokenKind::Colon) {
            self.tokens.skip(&TokenKind::Colon)?;

            Some(Box::new(self.parse_type_notation()?))
        } else {
            None
        };

        // Expecting "="

        self.tokens.skip(&TokenKind::Assign)?;

        let expr = self.parse_expression()?;
        let end = expr.span().end;

        // Expecting ";"

        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::VariableDeclaration {
            id,
            tn,
            val: Box::new(expr.unwrap_group()),
            span: start..end,
        })
    }

    fn parse_type_notation(&self) -> Result<AstNode> {
        let token = self.tokens.current();
        match token.kind {
            TokenKind::Identifier(name) => {
                self.tokens.advance();
                Ok(AstNode::TypeNotation {
                    tn: TypeNotation::Identifier(name),
                    span: token.span.clone(),
                })
            }

            TokenKind::LParen => {
                let start = token.span.start;
                self.tokens.advance();

                // Expecting >= 0 parameter type notation(s)

                let mut params_tn = vec![];
                while !self.tokens.current_is(&TokenKind::RParen) {
                    // Expecting type notation

                    let tn = self.parse_type_notation()?;
                    params_tn.push(tn);

                    let token = self.tokens.current();
                    match token.kind {
                        TokenKind::Comma => {
                            self.tokens.skip(&TokenKind::Comma)?;
                            continue;
                        }
                        TokenKind::RParen => continue,
                        k => {
                            return Err(ParsingError::UnexpectedToken {
                                expect: TokenKind::RParen,
                                found: k.clone(),
                                span: token.span,
                            });
                        }
                    }
                }

                // Expecting ")"

                self.tokens.skip(&TokenKind::RParen)?;

                // Expecting "->"

                self.tokens.skip(&TokenKind::RightPoint)?;

                // Expecting return type notation

                let return_tn = Box::new(self.parse_type_notation()?);
                let end = return_tn.span().end;

                Ok(AstNode::TypeNotation {
                    tn: TypeNotation::Callable {
                        params_tn,
                        return_tn,
                    },
                    span: start..end,
                })
            }

            _ => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Identifier(String::from("foo")),
                found: token.kind.clone(),
                span: token.span,
            }),
        }
    }

    fn parse_conditional_branch(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        let mut end;
        self.tokens.skip(&TokenKind::If)?;

        // Expecting expression

        let cond = Box::new(self.parse_expression()?);

        // Expecting block

        let (body, block_span) = self.parse_block(Some(TokenKind::Else))?;
        end = block_span.end;

        // Expecting >= 0 "else if" or 1 "else"

        let or_else = if self.tokens.current_is(&TokenKind::Else) {
            let else_start = self.tokens.current().span.start;
            self.tokens.skip(&TokenKind::Else)?;

            let token = self.tokens.current();
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

    fn parse_while(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        self.tokens.skip(&TokenKind::While)?;

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

    fn parse_loop_control(&self) -> Result<AstNode> {
        let Token { kind, span, .. } = self.tokens.current();

        // Expecting either "break" or "continue" keyword

        let control = match kind {
            TokenKind::Break => AstNode::Break { span },
            TokenKind::Continue => AstNode::Continue { span },
            _ => unreachable!(),
        };

        self.tokens.advance();

        // Expecting ";"

        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(control)
    }

    fn parse_function_definition(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        self.tokens.skip(&TokenKind::Fn)?;

        // Expecting identifier

        let token = self.tokens.current();
        let id = match token.kind {
            TokenKind::Identifier(name) => {
                self.tokens.advance();
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

        self.tokens.skip(&TokenKind::LParen)?;

        // Followed by >= 0 function parameter declaration(s)

        let mut params = vec![];
        while !self.tokens.current_is(&TokenKind::RParen) {
            // Expecting identifier

            let token = self.tokens.current();
            let id = match token.kind {
                TokenKind::Identifier(name) => {
                    self.tokens.advance();
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

            self.tokens.skip(&TokenKind::Colon)?;

            // Expecting type notation

            let tn = self.parse_type_notation()?;

            // Add to parameter list

            params.push(FunctionParam { id, tn });

            // Expecting either "," or ")"

            let token = self.tokens.current();
            match token.kind {
                TokenKind::Comma => {
                    self.tokens.skip(&TokenKind::Comma)?;
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

        self.tokens.skip(&TokenKind::RParen)?;

        // Expecting return type notation (optional)

        let return_tn = if self.tokens.current_is(&TokenKind::Colon) {
            self.tokens.skip(&TokenKind::Colon)?;
            Some(Box::new(self.parse_type_notation()?))
        } else {
            None
        };

        // Expecting function body

        let (body, body_span) = self.parse_block(None)?;

        Ok(AstNode::FunctionDefinition {
            id,
            params,
            return_tn,
            body,
            span: start..body_span.end,
        })
    }

    fn parse_return_statement(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        let mut end = self.tokens.current().span.end;
        self.tokens.skip(&TokenKind::Return)?;

        // Expecting expression (optional)

        let expr = if self.tokens.current_is(&TokenKind::Semicolon) {
            None
        } else {
            let expr = self.parse_expression()?;
            end = expr.span().end;
            Some(Box::new(expr))
        };

        // Expecting ";"

        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::Return {
            expr,
            span: start..end,
        })
    }

    fn parse_debug_statement(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        self.tokens.skip(&TokenKind::Debug)?;

        // Expecting expression

        let expr = Box::new(self.parse_expression()?);
        let end = expr.span().end;

        // Expecting ";"

        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::Debug {
            expr,
            span: start..end,
        })
    }

    fn parse_expression(&self) -> Result<AstNode> {
        self.parse_assignment()
    }

    fn parse_assignment(&self) -> Result<AstNode> {
        // Parse first term

        let lhs = self.parse_logical_and_or_expression()?;

        // Expecting "=", "+=", "-=", "*=", "/=", or "%=" (optional)

        match self.tokens.current_kind() {
            TokenKind::Assign => {
                self.tokens.skip(&TokenKind::Assign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Assign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::AddAssign => {
                self.tokens.skip(&TokenKind::AddAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::AddAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::SubAssign => {
                self.tokens.skip(&TokenKind::SubAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::SubAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::MulAssign => {
                self.tokens.skip(&TokenKind::MulAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::MulAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::DivAssign => {
                self.tokens.skip(&TokenKind::DivAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::DivAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::ModAssign => {
                self.tokens.skip(&TokenKind::ModAssign)?;

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

    fn parse_logical_and_or_expression(&self) -> Result<AstNode> {
        // Parse first term

        let mut lhs = self.parse_equality_expression()?;

        loop {
            // Expecting "||" or "&&" (both are optional)

            match self.tokens.current_kind() {
                TokenKind::Or => {
                    self.tokens.skip(&TokenKind::Or)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Or {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::And => {
                    self.tokens.skip(&TokenKind::And)?;

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

    fn parse_equality_expression(&self) -> Result<AstNode> {
        // Parse first term

        let mut lhs = self.parse_comparison_expression()?;

        loop {
            // Expecting "==" or "!=" (both are optional)

            match self.tokens.current_kind() {
                TokenKind::Eq => {
                    self.tokens.skip(&TokenKind::Eq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Eq {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Neq => {
                    self.tokens.skip(&TokenKind::Neq)?;

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

    fn parse_comparison_expression(&self) -> Result<AstNode> {
        // Parse first term

        let lhs = self.parse_additive_expression()?;

        // Expecting ">", ">=", "<" or "<=" (all are optional)

        match self.tokens.current_kind() {
            TokenKind::Gt => {
                self.tokens.skip(&TokenKind::Gt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Gte => {
                self.tokens.skip(&TokenKind::Gte)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gte {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lt => {
                self.tokens.skip(&TokenKind::Lt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Lt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lte => {
                self.tokens.skip(&TokenKind::Lte)?;

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

    fn parse_additive_expression(&self) -> Result<AstNode> {
        // Parse first term

        let mut lhs = self.parse_multiplicative_expression()?;

        loop {
            // Expecting "+" or "-" (both are optional)

            match self.tokens.current_kind() {
                TokenKind::Add => {
                    self.tokens.skip(&TokenKind::Add)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Add {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Sub => {
                    self.tokens.skip(&TokenKind::Sub)?;

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

    fn parse_multiplicative_expression(&self) -> Result<AstNode> {
        // Parse first term

        let mut lhs = self.parse_unary_expression()?;

        loop {
            // Expecting "*", "/" or "%" (all are optional)

            match self.tokens.current_kind() {
                TokenKind::Mul => {
                    self.tokens.skip(&TokenKind::Mul)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Mul {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Div => {
                    self.tokens.skip(&TokenKind::Div)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Div {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Mod => {
                    self.tokens.skip(&TokenKind::Mod)?;

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

    fn parse_unary_expression(&self) -> Result<AstNode> {
        //  Prefixed by >= 0 negation or not expression

        if self.tokens.current_is(&TokenKind::Sub) {
            return self.parse_prefix_expression(&TokenKind::Sub);
        } else if self.tokens.current_is(&TokenKind::Not) {
            return self.parse_prefix_expression(&TokenKind::Not);
        }

        // Parse primary expression

        let mut child = self.parse_primary_expression()?;

        // Followed by >= 0 function call, field access, or indexed access

        // TODO: field access and indexed access

        #[allow(clippy::while_let_loop)] // temporary
        loop {
            match self.tokens.current_kind() {
                TokenKind::LParen => {
                    let callee_start = child.span().start;
                    self.tokens.skip(&TokenKind::LParen)?;

                    let args = self.parse_function_call()?;

                    let span = callee_start..self.tokens.current().span.end;
                    self.tokens.skip(&TokenKind::RParen)?;

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

    fn parse_prefix_expression(&self, token: &TokenKind) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        self.tokens.skip(token)?;

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

    fn parse_primary_expression(&self) -> Result<AstNode> {
        let token = self.tokens.current();

        Ok(match token.kind {
            TokenKind::LParen => {
                // Parse group expression

                let lparen_start = token.span.start;
                self.tokens.skip(&TokenKind::LParen)?;

                let expression = self.parse_expression()?;

                let span = lparen_start..self.tokens.current().span.end;
                self.tokens.skip(&TokenKind::RParen)?;

                AstNode::Group {
                    child: Box::new(expression),
                    span,
                }
            }

            // Expecting either identifier or literals
            TokenKind::Identifier(name) => {
                self.tokens.advance();
                AstNode::Identifier {
                    name,
                    span: token.span,
                }
            }
            TokenKind::Integer(n) => {
                self.tokens.advance();
                AstNode::Literal {
                    lit: Literal::Integer(n),
                    span: token.span,
                }
            }
            TokenKind::Float(n) => {
                self.tokens.advance();
                AstNode::Literal {
                    lit: Literal::Float(n),
                    span: token.span,
                }
            }
            TokenKind::BooleanTrue => {
                self.tokens.advance();
                AstNode::Literal {
                    lit: Literal::Boolean(true),
                    span: token.span,
                }
            }
            TokenKind::BooleanFalse => {
                self.tokens.advance();
                AstNode::Literal {
                    lit: Literal::Boolean(false),
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

    fn parse_function_call(&self) -> Result<Vec<AstNode>> {
        // Can have >= 0 arguments

        let mut args = vec![];

        loop {
            // Stop when encounter a closing parentheses

            if self.tokens.current_is(&TokenKind::RParen) {
                return Ok(args);
            }

            // Parse argument

            args.push(self.parse_expression()?);

            // Continue if encounter "," or break out of loop if encounter ")"

            let token = self.tokens.current();
            match token.kind {
                TokenKind::Comma => {
                    self.tokens.skip(&TokenKind::Comma)?;
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
        assert_eq!(result.unwrap(), AstNode::Program { body: vec![expect] });
    }

    fn parse_and_assert_error(input: &str) {
        let tokens = lexer::lex(input).unwrap();
        let result = parse(tokens);

        assert!(result.is_err());
    }

    //
    // Test variable declarations
    //

    #[test]
    fn test_parsing_without_type_notation() {
        parse_and_assert_result(
            "var abc = 123 * x;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("abc"),
                    span: 4..7,
                }),
                tn: None,
                val: Box::new(AstNode::Mul {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(123),
                        span: 10..13,
                    }),
                    rhs: Box::new(AstNode::Identifier {
                        name: String::from("x"),
                        span: 16..17,
                    }),
                    span: 10..17,
                }),
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
                val: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(123),
                        span: 9..12,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(50),
                        span: 15..17,
                    }),
                    span: 9..17,
                }),
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
                val: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 12..15,
                }),
                span: 0..19,
            },
        );
    }

    #[test]
    fn test_parsing_without_initial_value() {
        parse_and_assert_error("var x: Int;");
    }

    #[test]
    fn test_parsing_with_both_type_notation_and_initial_value() {
        parse_and_assert_result(
            "var x: Int = 5;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Identifier(String::from("Int")),
                    span: 7..10,
                })),
                val: Box::new(AstNode::Literal {
                    lit: Literal::Integer(5),
                    span: 13..14,
                }),
                span: 0..14,
            },
        );
    }

    #[test]
    fn test_parsing_function_type_notation() {
        parse_and_assert_result(
            "var x: (Int) -> Void = foo;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Callable {
                        params_tn: vec![AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("Int")),
                            span: 8..11,
                        }],
                        return_tn: Box::new(AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("Void")),
                            span: 16..20,
                        }),
                    },
                    span: 7..20,
                })),
                val: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 23..26,
                }),
                span: 0..26,
            },
        );
    }

    #[test]
    fn test_parsing_nested_function_type_notation() {
        parse_and_assert_result(
            "var x: (Int, Bool) -> (Int,) -> Void = foo;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Callable {
                        params_tn: vec![
                            AstNode::TypeNotation {
                                tn: TypeNotation::Identifier(String::from("Int")),
                                span: 8..11,
                            },
                            AstNode::TypeNotation {
                                tn: TypeNotation::Identifier(String::from("Bool")),
                                span: 13..17,
                            },
                        ],
                        return_tn: Box::new(AstNode::TypeNotation {
                            tn: TypeNotation::Callable {
                                params_tn: vec![AstNode::TypeNotation {
                                    tn: TypeNotation::Identifier(String::from("Int")),
                                    span: 23..26,
                                }],
                                return_tn: Box::new(AstNode::TypeNotation {
                                    tn: TypeNotation::Identifier(String::from("Void")),
                                    span: 32..36,
                                }),
                            },
                            span: 22..36,
                        }),
                    },
                    span: 7..36,
                })),
                val: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 39..42,
                }),
                span: 0..42,
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
                        lit: Literal::Integer(15),
                        span: 3..5,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(10),
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
                        lit: Literal::Integer(1),
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
                    lit: Literal::Boolean(false),
                    span: 3..8,
                }),
                body: vec![],
                or_else: Some(Box::new(AstNode::If {
                    cond: Box::new(AstNode::Literal {
                        lit: Literal::Boolean(false),
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
                    lit: Literal::Boolean(true),
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
                    lit: Literal::Boolean(true),
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
                return_tn: None,
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
                    FunctionParam {
                        id: AstNode::Identifier {
                            name: String::from("x"),
                            span: 7..8,
                        },
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("Int")),
                            span: 10..13,
                        },
                    },
                    FunctionParam {
                        id: AstNode::Identifier {
                            name: String::from("y"),
                            span: 15..16,
                        },
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("Bool")),
                            span: 18..22,
                        },
                    },
                ],
                return_tn: None,
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
                params: vec![FunctionParam {
                    id: AstNode::Identifier {
                        name: String::from("x"),
                        span: 9..10,
                    },
                    tn: AstNode::TypeNotation {
                        tn: TypeNotation::Identifier(String::from("Int")),
                        span: 12..15,
                    },
                }],
                return_tn: None,
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
                return_tn: Some(Box::new(AstNode::TypeNotation {
                    tn: TypeNotation::Identifier(String::from("Int")),
                    span: 10..13,
                })),
                body: vec![AstNode::Return {
                    expr: Some(Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
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
                        lit: Literal::Integer(5),
                        span: 6..7,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            lit: Literal::Integer(5),
                            span: 10..11,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            lit: Literal::Integer(7),
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
                            lit: Literal::Integer(512),
                            span: 6..9,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            lit: Literal::Integer(200),
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
                        lit: Literal::Integer(3),
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
                        lit: Literal::Integer(123),
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
                        lit: Literal::Integer(5),
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
                        lit: Literal::Integer(5),
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
                        lit: Literal::Integer(5),
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
                        lit: Literal::Integer(5),
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
                        lit: Literal::Integer(5),
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
                        lit: Literal::Integer(5),
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
                    lit: Literal::Float(50.0),
                    span: 0..4,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Float(2.0),
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
                        lit: Literal::Integer(123),
                        span: 1..4,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(53),
                        span: 7..9,
                    }),
                    span: 1..9,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Integer(7),
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
                    lit: Literal::Integer(123),
                    span: 0..3,
                }),
                rhs: Box::new(AstNode::Sub {
                    lhs: Box::new(AstNode::Identifier {
                        name: String::from("foo"),
                        span: 7..10,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(50),
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
                lit: Literal::Integer(75),
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
                            lit: Literal::Integer(123),
                            span: 4..7,
                        },
                        AstNode::Add {
                            lhs: Box::new(AstNode::Literal {
                                lit: Literal::Integer(50),
                                span: 9..11,
                            }),
                            rhs: Box::new(AstNode::Literal {
                                lit: Literal::Integer(2),
                                span: 14..15,
                            }),
                            span: 9..15,
                        },
                    ],
                    span: 0..16,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Integer(7),
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
                            lit: Literal::Integer(123),
                            span: 8..11,
                        },
                        AstNode::Literal {
                            lit: Literal::Integer(456),
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
                            lit: Literal::Integer(5),
                            span: 10..11,
                        }),
                        span: 8..12,
                    }),
                    rhs: Box::new(AstNode::Neg {
                        child: Box::new(AstNode::Neg {
                            child: Box::new(AstNode::Literal {
                                lit: Literal::Integer(7),
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
                lit: Literal::Boolean(true),
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
                        lit: Literal::Integer(1),
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
                        span: 5..6,
                    }),
                    span: 0..6,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Boolean(true),
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
                        lit: Literal::Boolean(false),
                        span: 0..5,
                    }),
                    rhs: Box::new(AstNode::Not {
                        child: Box::new(AstNode::Literal {
                            lit: Literal::Boolean(false),
                            span: 10..15,
                        }),
                        span: 9..15,
                    }),
                    span: 0..15,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Boolean(true),
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
