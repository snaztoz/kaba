use super::{
    block::BlockParser, error::ParsingError, state::ParserState, tn::TypeNotationParser, Result,
};
use crate::{
    ast::{AstNode, FunctionParam},
    lexer::token::TokenKind,
};

pub struct FunctionDefinitionParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> FunctionDefinitionParser<'a> {
    pub const fn new(state: &'a ParserState) -> Self {
        Self { state }
    }
}

impl FunctionDefinitionParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;

        // Expecting "fn" keyword
        self.state.tokens.skip(&TokenKind::Fn)?;

        // Expecting symbol
        let sym = self.parse_sym()?;
        let sym_id = self.state.next_id();

        // Expecting "("
        self.state.tokens.skip(&TokenKind::LParen)?;

        // Expecting >= 0 function parameter declaration(s)
        let params = self.parse_params()?;

        // Expecting ")"
        self.state.tokens.skip(&TokenKind::RParen)?;

        // Expecting return type notation (optional)
        let return_tn = self.parse_return_tn()?;

        // Expecting function body
        let block = BlockParser::new(self.state).parse()?;

        Ok(AstNode::FunctionDefinition {
            params,
            sym: Box::new(sym),
            sym_id,
            return_tn: return_tn.map(Box::new),
            body: block.body,
            span: start..block.span.end,
        })
    }

    fn parse_sym(&self) -> Result<AstNode> {
        let sym = match self.state.tokens.current_kind() {
            TokenKind::Symbol(name) => Ok(AstNode::Symbol {
                name,
                span: self.state.tokens.current().span,
            }),

            kind => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Symbol(String::from("function_name")),
                found: kind.clone(),
                span: self.state.tokens.current().span,
            }),
        };

        self.state.tokens.advance();

        sym
    }

    fn parse_params(&self) -> Result<Vec<FunctionParam>> {
        let mut params = vec![];
        while !self.state.tokens.current_is(&TokenKind::RParen) {
            // Expecting symbol
            let sym = self.parse_sym()?;

            // Expecting ":"
            self.state.tokens.skip(&TokenKind::Colon)?;

            // Expecting type notation
            let tn = TypeNotationParser::new(self.state).parse()?;

            params.push(FunctionParam {
                sym,
                sym_id: self.state.next_id(),
                tn,
            });

            // Expecting either "," or ")"

            match self.state.tokens.current_kind() {
                TokenKind::Comma => {
                    self.state.tokens.skip(&TokenKind::Comma)?;
                    continue;
                }

                TokenKind::RParen => continue,

                kind => {
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                        span: self.state.tokens.current().span,
                    });
                }
            }
        }

        Ok(params)
    }

    fn parse_return_tn(&self) -> Result<Option<AstNode>> {
        if !self.state.tokens.current_is(&TokenKind::Colon) {
            return Ok(None);
        }

        self.state.tokens.skip(&TokenKind::Colon)?;

        Ok(Some(TypeNotationParser::new(self.state).parse()?))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, FunctionParam, Literal, TypeNotation},
        parser::test_util::assert_ast,
    };

    #[test]
    fn empty_function_definition() {
        assert_ast(
            "fn foo() {}",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                sym_id: 1,
                params: vec![],
                return_tn: None,
                body: vec![],
                span: 0..11,
            },
        );
    }

    #[test]
    fn function_definition_with_parameters_and_trailing_comma() {
        assert_ast(
            "fn foo(x: int, y: bool,) {}",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                sym_id: 1,
                params: vec![
                    FunctionParam {
                        sym: AstNode::Symbol {
                            name: String::from("x"),
                            span: 7..8,
                        },
                        sym_id: 2,
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Symbol(String::from("int")),
                            span: 10..13,
                        },
                    },
                    FunctionParam {
                        sym: AstNode::Symbol {
                            name: String::from("y"),
                            span: 15..16,
                        },
                        sym_id: 3,
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Symbol(String::from("bool")),
                            span: 18..22,
                        },
                    },
                ],
                return_tn: None,
                body: vec![],
                span: 0..27,
            },
        );
    }

    #[test]
    fn function_definition_with_parameter_and_body() {
        assert_ast(
            "fn write(x: int) { print(x); }",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: String::from("write"),
                    span: 3..8,
                }),
                sym_id: 1,
                params: vec![FunctionParam {
                    sym: AstNode::Symbol {
                        name: String::from("x"),
                        span: 9..10,
                    },
                    sym_id: 2,
                    tn: AstNode::TypeNotation {
                        tn: TypeNotation::Symbol(String::from("int")),
                        span: 12..15,
                    },
                }],
                return_tn: None,
                body: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Symbol {
                        name: String::from("print"),
                        span: 19..24,
                    }),
                    args: vec![AstNode::Symbol {
                        name: String::from("x"),
                        span: 25..26,
                    }],
                    span: 19..27,
                }],
                span: 0..30,
            },
        );
    }

    #[test]
    fn function_definition_with_return_statement() {
        assert_ast(
            "fn foo(): int { return 5; }",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                sym_id: 1,
                params: vec![],
                return_tn: Some(Box::new(AstNode::TypeNotation {
                    tn: TypeNotation::Symbol(String::from("int")),
                    span: 10..13,
                })),
                body: vec![AstNode::Return {
                    expr: Some(Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 23..24,
                    })),
                    span: 16..24,
                }],
                span: 0..27,
            },
        );
    }
}
