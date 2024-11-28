use super::{
    error::{Error, Result},
    scope::ScopeStack,
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

pub struct TypeNotationChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
    allow_void: bool,
}

impl<'a> TypeNotationChecker<'a> {
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self {
            ss,
            node,
            allow_void: false,
        }
    }

    pub fn new_with_void_allowed(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self {
            ss,
            node,
            allow_void: true,
        }
    }
}

impl TypeNotationChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        // The provided type must exist in the current scope
        if !self.ss.has_type(&self.t()) {
            return Err(Error::TypeNotExist {
                id: self.t().to_string(),
                span: self.span().clone(),
            });
        }

        if !self.allow_void && self.t().is_void() {
            return Err(Error::VoidTypeVariable {
                span: self.span().clone(),
            });
        }

        Ok(self.t())
    }

    fn t(&self) -> Type {
        Type::from_tn(self.node)
    }

    fn span(&self) -> &Span {
        if let AstNode::TypeNotation { span, .. } = self.node {
            span
        } else {
            unreachable!()
        }
    }
}
