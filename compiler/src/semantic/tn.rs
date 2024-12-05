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

    void_allowed: bool,
    auto_sized_array_allowed: bool,
}

impl<'a> TypeNotationChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self {
            ss,
            node,
            void_allowed: false,
            auto_sized_array_allowed: true,
        }
    }

    pub const fn allow_void(mut self) -> Self {
        self.void_allowed = true;
        self
    }

    pub const fn forbid_auto_sized_array(mut self) -> Self {
        self.auto_sized_array_allowed = false;
        self
    }
}

impl TypeNotationChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        // The provided type must exist in the current scope
        if !self.ss.has_t(&self.t()) {
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

        if !self.auto_sized_array_allowed && self.t().is_auto_sized_array() {
            return Err(Error::AutoSizedArrayReturnType {
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
