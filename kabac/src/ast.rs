//! This module contains representation of the AST of Kaba program.
//!
//! Root of the tree will always be the [`AstNode<'src>::Program`] that may contains
//! `>= 0` statements.

use logos::Span;
use std::fmt::Display;

pub type Id = u32;
pub type SymbolId = Id;
pub type ScopeId = Id;

/// The representation of each node that make up a whole Kaba AST.
#[derive(Debug, PartialEq)]
pub enum AstNode<'src> {
    // The root of all other AstNode<'src> variants
    Program {
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
        span: Span,
    },

    VariableDeclaration {
        sym: Box<AstNode<'src>>,
        sym_id: SymbolId,
        tn: Option<Box<AstNode<'src>>>,
        val: Box<AstNode<'src>>,
        span: Span,
    },

    If {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
        or_else: Option<Box<AstNode<'src>>>,
        span: Span,
    },

    Else {
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
        span: Span,
    },

    While {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
        span: Span,
    },

    Each {
        elem_sym: Box<AstNode<'src>>,
        elem_sym_id: SymbolId,
        iterable: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
        span: Span,
    },

    Break {
        span: Span,
    },

    Continue {
        span: Span,
    },

    FunctionDefinition {
        sym: Box<AstNode<'src>>,
        sym_id: SymbolId,
        params: Vec<FunctionParam<'src>>,
        return_tn: Option<Box<AstNode<'src>>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
        span: Span,
    },

    Return {
        expr: Option<Box<AstNode<'src>>>,
        span: Span,
    },

    Debug {
        expr: Box<AstNode<'src>>,
        span: Span,
    },

    RecordDefinition {
        sym: Box<AstNode<'src>>,
        sym_id: SymbolId,
        fields: Vec<RecordField<'src>>,
        span: Span,
    },

    Assign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    AddAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    SubAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    MulAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    DivAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    ModAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Or {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    And {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Eq {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Neq {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Gt {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Gte {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Lt {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Lte {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Add {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Sub {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Mul {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Div {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Mod {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
        span: Span,
    },

    Not {
        expr: Box<AstNode<'src>>,
        span: Span,
    },

    Neg {
        expr: Box<AstNode<'src>>,
        span: Span,
    },

    FunctionCall {
        callee: Box<AstNode<'src>>,
        args: Vec<AstNode<'src>>,
        span: Span,
    },

    IndexAccess {
        object: Box<AstNode<'src>>,
        index: Box<AstNode<'src>>,
        span: Span,
    },

    // This variant is only used as the span information holder and then will be
    // removed. It won't be present in the final resulting ASTs.
    Group {
        expr: Box<AstNode<'src>>,
        span: Span,
    },

    Symbol {
        name: &'src str,
        span: Span,
    },

    TypeNotation {
        tn: TypeNotation<'src>,
        span: Span,
    },

    Literal {
        lit: Literal<'src>,
        span: Span,
    },
}

impl AstNode<'_> {
    pub fn span(&self) -> &Span {
        match self {
            Self::Program { span, .. }
            | Self::VariableDeclaration { span, .. }
            | Self::If { span, .. }
            | Self::Else { span, .. }
            | Self::While { span, .. }
            | Self::Each { span, .. }
            | Self::Break { span }
            | Self::Continue { span }
            | Self::FunctionDefinition { span, .. }
            | Self::Return { span, .. }
            | Self::Debug { span, .. }
            | Self::RecordDefinition { span, .. }
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
            | Self::Symbol { span, .. }
            | Self::TypeNotation { span, .. }
            | Self::Literal { span, .. } => span,
        }
    }

    pub const fn is_lval(&self) -> bool {
        matches!(self, Self::Symbol { .. }) || matches!(self, Self::IndexAccess { .. })
    }

    pub fn sym(&self) -> &AstNode {
        match self {
            Self::FunctionDefinition { sym, .. } | Self::VariableDeclaration { sym, .. } => sym,
            _ => unreachable!(),
        }
    }

    pub fn sym_id(&self) -> SymbolId {
        match self {
            Self::FunctionDefinition { sym_id, .. } | Self::VariableDeclaration { sym_id, .. } => {
                *sym_id
            }
            _ => unreachable!(),
        }
    }

    pub fn body(&self) -> &[AstNode] {
        match self {
            Self::Program { body, .. }
            | AstNode::FunctionDefinition { body, .. }
            | AstNode::If { body, .. }
            | AstNode::Else { body, .. }
            | AstNode::While { body, .. }
            | AstNode::Each { body, .. } => body,
            _ => unreachable!(),
        }
    }

    pub fn scope_id(&self) -> ScopeId {
        match self {
            Self::Program { scope_id, .. }
            | Self::FunctionDefinition { scope_id, .. }
            | Self::If { scope_id, .. }
            | Self::Else { scope_id, .. }
            | Self::While { scope_id, .. }
            | Self::Each { scope_id, .. } => *scope_id,
            _ => unreachable!(),
        }
    }

    pub fn params(&self) -> &[FunctionParam] {
        if let AstNode::FunctionDefinition { params, .. } = self {
            params
        } else {
            unreachable!()
        }
    }

    pub fn return_tn(&self) -> Option<&AstNode> {
        if let AstNode::FunctionDefinition { return_tn, .. } = self {
            return_tn.as_deref()
        } else {
            unreachable!()
        }
    }

    pub fn unwrap_symbol(&self) -> (&str, Span) {
        if let AstNode::Symbol { name, span } = self {
            (name, span.clone())
        } else {
            unreachable!()
        }
    }
}

impl<'src> AstNode<'src> {
    pub fn unwrap_group(self) -> AstNode<'src> {
        if let AstNode::Group { expr, .. } = self {
            // unwrap recursively
            expr.unwrap_group()
        } else {
            self
        }
    }
}

impl Display for AstNode<'_> {
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
    pub sym_id: SymbolId,
    pub tn: AstNode<'src>,
}

#[derive(Debug, PartialEq)]
pub struct RecordField<'src> {
    pub sym: AstNode<'src>,
    pub sym_id: SymbolId,
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
