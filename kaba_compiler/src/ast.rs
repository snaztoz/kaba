//! This module contains representation of the AST of Kaba program.
//!
//! Root of the tree will always be the [`AstNode<'src>::Program`] that may contains
//! `>= 0` statements.

use logos::Span;
use std::fmt::Display;

pub type NodeId = u32;

#[derive(Debug)]
pub struct AstNode<'src> {
    pub id: NodeId,
    pub variant: AstNodeVariant<'src>,
    pub span: Span,
}

impl AstNode<'_> {
    pub const fn is_lval(&self) -> bool {
        matches!(
            self.variant,
            AstNodeVariant::Symbol { .. } | AstNodeVariant::IndexAccess { .. },
        )
    }

    pub const fn is_function_definition(&self) -> bool {
        matches!(self.variant, AstNodeVariant::FunctionDefinition { .. })
    }

    pub const fn is_record_definition(&self) -> bool {
        matches!(self.variant, AstNodeVariant::RecordDefinition { .. })
    }

    pub fn sym(&self) -> &AstNode {
        match &self.variant {
            AstNodeVariant::FunctionDefinition { sym, .. }
            | AstNodeVariant::RecordDefinition { sym, .. }
            | AstNodeVariant::VariableDeclaration { sym, .. } => sym,
            _ => unreachable!(),
        }
    }

    pub fn body(&self) -> &[AstNode] {
        match &self.variant {
            AstNodeVariant::Program { body, .. }
            | AstNodeVariant::FunctionDefinition { body, .. }
            | AstNodeVariant::If { body, .. }
            | AstNodeVariant::Else { body, .. }
            | AstNodeVariant::While { body, .. }
            | AstNodeVariant::Each { body, .. } => body,
            _ => unreachable!(),
        }
    }

    pub fn params(&self) -> &[FunctionParam] {
        if let AstNodeVariant::FunctionDefinition { params, .. } = &self.variant {
            params
        } else {
            unreachable!()
        }
    }

    pub fn return_tn(&self) -> Option<&AstNode> {
        if let AstNodeVariant::FunctionDefinition { return_tn, .. } = &self.variant {
            return_tn.as_deref()
        } else {
            unreachable!()
        }
    }

    pub fn sym_name(&self) -> &str {
        if let AstNodeVariant::Symbol { name } = &self.variant {
            name
        } else {
            unreachable!()
        }
    }

    pub fn into_group_inner(self) -> Self {
        if let AstNodeVariant::Group { expr } = self.variant {
            expr.into_group_inner()
        } else {
            self
        }
    }
}

impl Display for AstNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

impl PartialEq for AstNode<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.variant.eq(&other.variant)
            && self.span.start == other.span.start
            && self.span.end == other.span.end
    }
}

/// The representation of each node that make up a whole Kaba AST.
#[derive(Debug, PartialEq)]
pub enum AstNodeVariant<'src> {
    // The root of all other AstNode<'src> variants
    Program {
        body: Vec<AstNode<'src>>,
    },

    VariableDeclaration {
        sym: Box<AstNode<'src>>,
        tn: Option<Box<AstNode<'src>>>,
        val: Box<AstNode<'src>>,
    },

    If {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        or_else: Option<Box<AstNode<'src>>>,
    },

    Else {
        body: Vec<AstNode<'src>>,
    },

    While {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
    },

    Each {
        elem_sym: Box<AstNode<'src>>,
        iterable: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
    },

    Break,

    Continue,

    FunctionDefinition {
        sym: Box<AstNode<'src>>,
        params: Vec<FunctionParam<'src>>,
        return_tn: Option<Box<AstNode<'src>>>,
        body: Vec<AstNode<'src>>,
    },

    Return {
        expr: Option<Box<AstNode<'src>>>,
    },

    Debug {
        expr: Box<AstNode<'src>>,
    },

    RecordDefinition {
        sym: Box<AstNode<'src>>,
        fields: Vec<RecordField<'src>>,
    },

    Assign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    AddAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    SubAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    MulAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    DivAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    ModAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Or {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    And {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Eq {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Neq {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Gt {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Gte {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Lt {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Lte {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Add {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Sub {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Mul {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Div {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Mod {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Not {
        expr: Box<AstNode<'src>>,
    },

    Neg {
        expr: Box<AstNode<'src>>,
    },

    FunctionCall {
        callee: Box<AstNode<'src>>,
        args: Vec<AstNode<'src>>,
    },

    IndexAccess {
        object: Box<AstNode<'src>>,
        index: Box<AstNode<'src>>,
    },

    // This variant is only used as the span information holder and then will be
    // removed. It won't be present in the final resulting ASTs.
    Group {
        expr: Box<AstNode<'src>>,
    },

    Symbol {
        name: &'src str,
    },

    TypeNotation {
        tn: TypeNotation<'src>,
    },

    Literal {
        lit: Literal<'src>,
    },
}

impl Display for AstNodeVariant<'_> {
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
            Self::RecordDefinition { .. } => {
                write!(f, "record definition")
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
            Self::Symbol { .. } => {
                write!(f, "symbol")
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
pub struct FunctionParam<'src> {
    pub sym: AstNode<'src>,
    pub tn: AstNode<'src>,
}

#[derive(Debug, PartialEq)]
pub struct RecordField<'src> {
    pub sym: AstNode<'src>,
    pub tn: AstNode<'src>,
}

#[derive(Debug, PartialEq)]
pub enum TypeNotation<'src> {
    Symbol(&'src str),

    Array {
        elem_tn: Box<AstNode<'src>>,
    },

    Callable {
        params_tn: Vec<AstNode<'src>>,
        return_tn: Box<AstNode<'src>>,
    },
}

impl Display for TypeNotation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Symbol(sym) => write!(f, "{sym}"),

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
pub enum Literal<'src> {
    // A temporary value while the runtime is still using a tree-walk
    // interpreter mode
    Void,

    Bool(bool),
    Int(i32),
    Float(f32),
    Char(char),
    String(String),

    Array {
        elem_tn: Box<AstNode<'src>>,
        elems: Vec<AstNode<'src>>,
    },
}

impl Display for Literal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),

            Self::Bool(b) => write!(f, "{b}"),
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::String(s) => write!(f, "{s}"),

            Self::Array { elems, .. } => {
                let joined = elems
                    .iter()
                    .map(|tn| tn.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{joined}]")
            }
        }
    }
}
