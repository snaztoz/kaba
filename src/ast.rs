use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<AstNode>,
}

impl Program {
    pub fn into_vec_deque(self) -> VecDeque<AstNode> {
        VecDeque::from(self.statements)
    }
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    VariableDeclaration {
        identifier: String,
        r#type: Option<String>,
        value: Option<Box<AstNode>>,
    },
    ValueAssignment {
        lhs: Box<AstNode>,
        value: Box<AstNode>,
    },
    FunctionCall {
        callee: Box<AstNode>,
        args: Vec<AstNode>,
    },

    Add(Box<AstNode>, Box<AstNode>),
    Sub(Box<AstNode>, Box<AstNode>),
    Mul(Box<AstNode>, Box<AstNode>),
    Div(Box<AstNode>, Box<AstNode>),

    Identifier(String),
    Integer(i64),
}
