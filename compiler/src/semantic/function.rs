use super::{
    body::BodyChecker,
    error::{Error, Result},
    scope::{Scope, ScopeStack},
    tn::TypeNotationChecker,
    types::Type,
};
use crate::ast::{AstNode, FunctionParam};
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
        self.check_params_tn()?;
        self.check_return_tn()?;

        let params_t = self.params_t();
        let return_t = Box::new(self.return_t());

        let fn_t = Type::Callable { params_t, return_t };
        self.save_fn_t_to_stack(fn_t.clone())?;

        Ok(fn_t)
    }

    fn check_params_tn(&self) -> Result<()> {
        for FunctionParam { tn, .. } in self.params() {
            TypeNotationChecker::new(self.ss, tn).check()?;
        }

        Ok(())
    }

    fn check_return_tn(&self) -> Result<()> {
        if let Some(tn) = self.return_tn() {
            TypeNotationChecker::new_with_void_allowed(self.ss, tn).check()?;
        }

        Ok(())
    }

    fn params_t(&self) -> Vec<Type> {
        let mut params_t = vec![];
        for FunctionParam { tn, .. } in self.params() {
            params_t.push(Type::from_tn(tn));
        }

        params_t
    }

    fn return_t(&self) -> Type {
        self.return_tn().map_or(Type::new("Void"), Type::from_tn)
    }

    // Save function information to the ScopeStack.
    fn save_fn_t_to_stack(&self, fn_t: Type) -> Result<()> {
        let (id, id_span) = self.id().unwrap_identifier();
        self.ss
            .save_symbol_or_else(&id, fn_t.clone(), || Error::FunctionAlreadyExist {
                id: id.clone(),
                span: id_span,
            })
    }

    fn id(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn params(&self) -> &[FunctionParam] {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
        } else {
            unreachable!()
        }
    }

    fn return_tn(&self) -> Option<&AstNode> {
        if let AstNode::FunctionDefinition { return_tn, .. } = self.node {
            return_tn.as_deref()
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
        let return_t = self.fn_t().unwrap_callable().1;
        self.ss
            .with_scope(Scope::new_function_scope(return_t.clone()), || {
                self.save_params_to_stack(&self.params())?;

                // Check function body
                //
                // We do this last in order to accommodate features such as
                // recursive function call.

                let body_t = BodyChecker::new(self.ss, self.node).check()?;

                if !return_t.is_void() && body_t.is_void() {
                    return Err(Error::FunctionNotReturningValue {
                        expect: return_t,
                        span: self.id().span().clone(),
                    });
                }

                Ok(())
            })?;

        Ok(Type::new("Void"))
    }

    fn save_params_to_stack(&self, params: &[((String, Span), Type)]) -> Result<()> {
        for ((id, id_span), t) in params {
            self.ss
                .save_symbol_or_else(id, t.clone(), || Error::VariableAlreadyExist {
                    id: id.clone(),
                    span: id_span.clone(),
                })?;
        }

        Ok(())
    }

    fn params(&self) -> Vec<((String, Span), Type)> {
        let params_t = self.fn_t().unwrap_callable().0;
        let params_id = self.params_id();

        params_id
            .iter()
            .cloned()
            .zip(params_t.iter().cloned())
            .collect::<Vec<_>>()
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
                .map(|p| p.id.unwrap_identifier())
                .collect::<Vec<_>>()
        } else {
            unreachable!()
        }
    }
}
