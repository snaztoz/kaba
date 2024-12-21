use super::ExpressionAnalyzer;
use crate::{
    ast::AstNode,
    semantic::{
        error::Result,
        state::SharedState,
        types::{assert, Type},
    },
};

/// Analyzer for index access expression rule.
pub struct IndexAccessAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> IndexAccessAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl IndexAccessAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let obj_t = ExpressionAnalyzer::new(self.obj(), self.state).analyze()?;
        assert::is_indexable(&obj_t, || self.obj().span().clone())?;

        let index_t = ExpressionAnalyzer::new(self.index(), self.state).analyze()?;
        assert::is_number(&index_t, || self.index().span().clone())?;

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
    use crate::semantic::test_util::assert_expr_type;

    #[test]
    fn index_accessing() {
        assert_expr_type(
            "[[1, 2]][0];",
            &[],
            Type::Array {
                elem_t: Some(Box::new(Type::UnboundedInt)),
            },
        );
    }
}
