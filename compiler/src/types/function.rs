use super::{
    body::BodyChecker,
    error::{Error, Result},
    scope::{Scope, ScopeStack},
    typ::Type,
};
use crate::ast::AstNode;
use logos::Span;

/// Checker for function declarations.
///
/// This checker assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the ScopeStack.
pub struct FunctionDeclarationChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionDeclarationChecker<'a> {
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionDeclarationChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let mut params_t = vec![];
        for (_, tn) in self.params() {
            let t = Type::from_type_notation(tn);

            // Parameter type must exist in the current scope
            if !self.ss.has_type(&t) {
                let (id, span) = tn.unwrap_type_notation();
                return Err(Error::TypeNotExist { id, span });
            }

            // Parameter should not have "Void" type
            if t.is_void() {
                return Err(Error::VoidTypeVariable {
                    span: tn.span().clone(),
                });
            }

            params_t.push(t);
        }

        let return_t = self.return_t().map_or(Ok(Type::new("Void")), |tn| {
            let t = Type::from_type_notation(tn);
            if self.ss.has_type(&t) {
                Ok(t)
            } else {
                let (id, span) = tn.unwrap_type_notation();
                Err(Error::TypeNotExist { id, span })
            }
        })?;

        let fn_t = Type::Callable {
            params_t,
            return_t: Box::new(return_t.clone()),
        };

        let (id, id_span) = self.id().unwrap_identifier();
        self.ss
            .save_symbol_or_else(&id, fn_t.clone(), || Error::FunctionAlreadyExist {
                id: id.clone(),
                span: id_span,
            })?;

        Ok(fn_t)
    }

    fn id(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn params(&self) -> &[(AstNode, AstNode)] {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
        } else {
            unreachable!()
        }
    }

    fn return_t(&self) -> Option<&AstNode> {
        if let AstNode::FunctionDefinition { return_t, .. } = self.node {
            return_t.as_deref()
        } else {
            unreachable!()
        }
    }
}

/// Checker for function definition.
///
/// This checker assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the ScopeStack.
pub struct FunctionDefinitionChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionDefinitionChecker<'a> {
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionDefinitionChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let (params_t, return_t) = if let Type::Callable { params_t, return_t } = self.fn_t() {
            (params_t, return_t)
        } else {
            unreachable!()
        };

        let params_id = self.params_id();
        let params = params_id.iter().cloned().zip(params_t.iter());

        // Entering new scope
        self.ss
            .with_scope(Scope::new_function_scope(*return_t.clone()), || {
                for ((id, id_span), t) in params {
                    self.ss.save_symbol_or_else(&id, t.clone(), || {
                        Error::VariableAlreadyExist {
                            id: id.clone(),
                            span: id_span.clone(),
                        }
                    })?;
                }

                // Check function body
                //
                // We do this last in order to accommodate features such as
                // recursive function call.

                let body_t = BodyChecker::new(self.ss, self.node).check()?;

                if !return_t.is_void() && body_t.is_void() {
                    return Err(Error::FunctionNotReturningValue {
                        expect: *return_t.clone(),
                        span: self.id().span().clone(),
                    });
                }

                Ok(())
            })?;

        Ok(Type::new("Void"))
    }

    fn id(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn fn_t(&self) -> Type {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            let id_str = &id.unwrap_identifier().0;
            self.ss.get_symbol_type(id_str).unwrap()
        } else {
            unreachable!()
        }
    }

    fn params_id(&self) -> Vec<(String, Span)> {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
                .iter()
                .map(|p| p.0.unwrap_identifier())
                .collect::<Vec<_>>()
        } else {
            unreachable!()
        }
    }
}
