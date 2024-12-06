//! This module contains representation of the AST of Kaba program.
//!
//! Root of the tree will always be the [`AstNode::Program`] that may contains
//! `>= 0` statements.

use logos::Span;
use std::fmt::Display;

/// The representation of each node that make up a whole Kaba AST.
#[derive(Debug, PartialEq)]
pub enum AstNode {
    // The root of all other AstNode variants
    Program {
        body: Vec<AstNode>,
    },

    VariableDeclaration {
        id: Box<AstNode>,
        tn: Option<Box<AstNode>>,
        val: Box<AstNode>,
        span: Span,
    },

    If {
        cond: Box<AstNode>,
        body: Vec<AstNode>,
        or_else: Option<Box<AstNode>>,
        span: Span,
    },

    Else {
        body: Vec<AstNode>,
        span: Span,
    },

    While {
        cond: Box<AstNode>,
        body: Vec<AstNode>,
        span: Span,
    },

    Each {
        elem_id: Box<AstNode>,
        iterable: Box<AstNode>,
        body: Vec<AstNode>,
        span: Span,
    },

    Break {
        span: Span,
    },

    Continue {
        span: Span,
    },

    FunctionDefinition {
        id: Box<AstNode>,
        params: Vec<FunctionParam>,
        return_tn: Option<Box<AstNode>>,
        body: Vec<AstNode>,
        span: Span,
    },

    Return {
        expr: Option<Box<AstNode>>,
        span: Span,
    },

    Debug {
        expr: Box<AstNode>,
        span: Span,
    },

    Assign {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    AddAssign {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    SubAssign {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    MulAssign {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    DivAssign {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    ModAssign {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Or {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    And {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Eq {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Neq {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Gt {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Gte {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Lt {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Lte {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Add {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Sub {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Mul {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Div {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Mod {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
        span: Span,
    },

    Not {
        child: Box<AstNode>,
        span: Span,
    },

    Neg {
        child: Box<AstNode>,
        span: Span,
    },

    FunctionCall {
        callee: Box<AstNode>,
        args: Vec<AstNode>,
        span: Span,
    },

    IndexAccess {
        object: Box<AstNode>,
        index: Box<AstNode>,
        span: Span,
    },

    // This variant is only used as the span information holder and then will be
    // removed. It won't be present in the final resulting ASTs.
    Group {
        child: Box<AstNode>,
        span: Span,
    },

    Identifier {
        name: String,
        span: Span,
    },

    TypeNotation {
        tn: TypeNotation,
        span: Span,
    },

    Literal {
        lit: Literal,
        span: Span,
    },
}

impl AstNode {
    pub fn span(&self) -> &Span {
        match self {
            Self::Program { .. } => unreachable!(),

            Self::VariableDeclaration { span, .. }
            | Self::If { span, .. }
            | Self::Else { span, .. }
            | Self::While { span, .. }
            | Self::Each { span, .. }
            | Self::Break { span }
            | Self::Continue { span }
            | Self::FunctionDefinition { span, .. }
            | Self::Return { span, .. }
            | Self::Debug { span, .. }
            | Self::Assign { span, .. }
            | Self::AddAssign { span, .. }
            | Self::SubAssign { span, .. }
            | Self::MulAssign { span, .. }
            | Self::DivAssign { span, .. }
            | Self::ModAssign { span, .. }
            | Self::Or { span, .. }
            | Self::And { span, .. }
            | Self::Eq { span, .. }
            | Self::Neq { span, .. }
            | Self::Gt { span, .. }
            | Self::Gte { span, .. }
            | Self::Lt { span, .. }
            | Self::Lte { span, .. }
            | Self::Add { span, .. }
            | Self::Sub { span, .. }
            | Self::Mul { span, .. }
            | Self::Div { span, .. }
            | Self::Mod { span, .. }
            | Self::Not { span, .. }
            | Self::Neg { span, .. }
            | Self::FunctionCall { span, .. }
            | Self::IndexAccess { span, .. }
            | Self::Group { span, .. }
            | Self::Identifier { span, .. }
            | Self::TypeNotation { span, .. }
            | Self::Literal { span, .. } => span,
        }
    }

    pub const fn is_valid_assignment_lhs(&self) -> bool {
        matches!(self, Self::Identifier { .. }) || matches!(self, Self::IndexAccess { .. })
    }

    pub fn unwrap_identifier(&self) -> (String, Span) {
        if let AstNode::Identifier { name, span } = self {
            (name.clone(), span.clone())
        } else {
            panic!("calling `unwrap_identifier` on non-identifier AstNode: {self:?}")
        }
    }

    pub fn unwrap_group(self) -> AstNode {
        if let AstNode::Group { child, .. } = self {
            // unwrap recursively
            child.unwrap_group()
        } else {
            self
        }
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableDeclaration { .. } => {
                write!(f, "variable declaration")
            }
            Self::If { .. } => {
                write!(f, "`if` statement")
            }
            Self::Else { .. } => {
                write!(f, "`else` statement")
            }
            Self::While { .. } => {
                write!(f, "`while` statement")
            }
            Self::Each { .. } => {
                write!(f, "`each` statement")
            }
            Self::FunctionDefinition { .. } => {
                write!(f, "function definition")
            }
            Self::Break { .. } => {
                write!(f, "`break` statement")
            }
            Self::Continue { .. } => {
                write!(f, "`continue` statement")
            }
            Self::Return { .. } => {
                write!(f, "`return` statement")
            }
            Self::Debug { .. } => {
                write!(f, "`debug` statement")
            }
            Self::Assign { .. } => {
                write!(f, "`assign` expression")
            }
            Self::AddAssign { .. } => {
                write!(f, "`add assign` expression")
            }
            Self::SubAssign { .. } => {
                write!(f, "`sub assign` expression")
            }
            Self::MulAssign { .. } => {
                write!(f, "`mul assign` expression")
            }
            Self::DivAssign { .. } => {
                write!(f, "`div assign` expression")
            }
            Self::ModAssign { .. } => {
                write!(f, "`mod assign` expression")
            }
            Self::Or { .. } => {
                write!(f, "`or` expression")
            }
            Self::And { .. } => {
                write!(f, "`and` expression")
            }
            Self::Eq { .. } => {
                write!(f, "`equal` expression")
            }
            Self::Neq { .. } => {
                write!(f, "`not equal` expression")
            }
            Self::Gt { .. } => {
                write!(f, "`greater than` expression")
            }
            Self::Gte { .. } => {
                write!(f, "`greater than or equal` expression")
            }
            Self::Lt { .. } => {
                write!(f, "`less than` expression")
            }
            Self::Lte { .. } => {
                write!(f, "`less than or equal` expression")
            }
            Self::Add { .. } => {
                write!(f, "`addition` expression")
            }
            Self::Sub { .. } => {
                write!(f, "`subtraction` expression")
            }
            Self::Mul { .. } => {
                write!(f, "`multiplication` expression")
            }
            Self::Div { .. } => {
                write!(f, "`division` expression")
            }
            Self::Mod { .. } => {
                write!(f, "`modulo` expression")
            }
            Self::Not { .. } => {
                write!(f, "`not` expression")
            }
            Self::Neg { .. } => {
                write!(f, "`negation` expression")
            }
            Self::FunctionCall { .. } => {
                write!(f, "function call expression")
            }
            Self::IndexAccess { .. } => {
                write!(f, "index access expression")
            }
            Self::Identifier { .. } => {
                write!(f, "identifier")
            }
            Self::TypeNotation { .. } => {
                write!(f, "type notation")
            }
            Self::Literal { .. } => {
                write!(f, "value literal")
            }

            Self::Program { .. } | Self::Group { .. } => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionParam {
    pub id: AstNode,
    pub tn: AstNode,
}

#[derive(Debug, PartialEq)]
pub enum TypeNotation {
    Identifier(String),
    Array {
        elem_tn: Box<AstNode>,
    },
    Callable {
        params_tn: Vec<AstNode>,
        return_tn: Box<AstNode>,
    },
}

impl Display for TypeNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "{id}"),

            Self::Array { elem_tn } => {
                write!(f, "[]{elem_tn}")
            }

            Self::Callable {
                params_tn,
                return_tn,
            } => {
                let joined = params_tn
                    .iter()
                    .map(|tn| tn.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({joined}) -> {return_tn}")
            }
        }
    }
}

/// The representation of each value that may exists in a Kaba source code,
/// such as integer or string.
#[derive(Debug, PartialEq)]
pub enum Literal {
    // A temporary value while the runtime is still using a tree-walk
    // interpreter mode
    Void,

    Integer(u32),
    Float(f64),
    Boolean(bool),

    Array(Vec<AstNode>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Integer(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Array(arr) => {
                let joined = arr
                    .iter()
                    .map(|tn| tn.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{joined}]")
            }
        }
    }
}
