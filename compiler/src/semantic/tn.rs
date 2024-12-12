use super::{
    error::{Error, Result},
    state::SharedState,
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

pub struct TypeNotationChecker<'a> {
    node: &'a AstNode,
    state: &'a SharedState,

    void_allowed: bool,
}

impl<'a> TypeNotationChecker<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self {
            node,
            state,
            void_allowed: false,
        }
    }

    pub const fn allow_void(mut self) -> Self {
        self.void_allowed = true;
        self
    }
}

impl TypeNotationChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        // The provided type must exist in the current scope
        if !self.state.ss.has_t(&self.t()) {
            return Err(Error::SymbolDoesNotExist {
                id: self.t().to_string(),
                span: self.span().clone(),
            });
        }

        if !self.void_allowed && self.t().is_void() {
            return Err(Error::VoidTypeVariable {
                span: self.span().clone(),
            });
        }

        Ok(self.t())
    }

    fn t(&self) -> Type {
        Type::from(self.node)
    }

    fn span(&self) -> &Span {
        if let AstNode::TypeNotation { span, .. } = self.node {
            span
        } else {
            unreachable!()
        }
    }
}
