use super::ExpressionChecker;
use crate::{
    ast::AstNode,
    semantic::{error::Result, scope::ScopeStack, types::Type},
};

/// Checker for index access expression rule.
pub struct IndexAccessChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> IndexAccessChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl IndexAccessChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let obj_t = ExpressionChecker::new(self.ss, self.obj()).check()?;
        Type::assert_indexable(&obj_t, || self.obj().span().clone())?;

        let index_t = ExpressionChecker::new(self.ss, self.index()).check()?;
        Type::assert_number(&index_t, || self.index().span().clone())?;

        match obj_t {
            Type::Array { elem_t } => Ok(*elem_t.unwrap()),

            _ => unreachable!(),
        }
    }

    fn obj(&self) -> &AstNode {
        if let AstNode::IndexAccess { object, .. } = self.node {
            object
        } else {
            unreachable!()
        }
    }

    fn index(&self) -> &AstNode {
        if let AstNode::IndexAccess { index, .. } = self.node {
            index
        } else {
            unreachable!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::test_util::assert_expression_type;

    #[test]
    fn index_accessing() {
        assert_expression_type(
            "[[1, 2]][0];",
            Type::Array {
                elem_t: Some(Box::new(Type::new("Int"))),
            },
        );
    }
}
