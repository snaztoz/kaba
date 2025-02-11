//! This module contains representation of the AST of Kaba program.
//!
//! Root of the tree will always be the [`AstNode<'src>::Program`] that may contains
//! `>= 0` statements.

use logos::Span;
use std::fmt::Display;

pub type Id = u32;
pub type SymbolId = Id;
pub type ScopeId = Id;

#[derive(Debug, PartialEq)]
pub struct AstNode<'src> {
    pub variant: AstNodeVariant<'src>,
    pub span: Span,
}

impl AstNode<'_> {
    pub fn unwrap_group(self) -> Self {
        if let AstNodeVariant::Group { expr } = self.variant {
            expr.unwrap_group()
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

/// The representation of each node that make up a whole Kaba AST.
#[derive(Debug, PartialEq)]
pub enum AstNodeVariant<'src> {
    // The root of all other AstNode<'src> variants
    Program {
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
    },

    VariableDeclaration {
        sym: Box<AstNode<'src>>,
        sym_id: SymbolId,
        tn: Option<Box<AstNode<'src>>>,
        val: Box<AstNode<'src>>,
    },

    If {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
        or_else: Option<Box<AstNode<'src>>>,
    },

    Else {
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
    },

    While {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
    },

    Each {
        elem_sym: Box<AstNode<'src>>,
        elem_sym_id: SymbolId,
        iterable: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
    },

    Break,

    Continue,

    FunctionDefinition {
        sym: Box<AstNode<'src>>,
        sym_id: SymbolId,
        params: Vec<FunctionParam<'src>>,
        return_tn: Option<Box<AstNode<'src>>>,
        body: Vec<AstNode<'src>>,
        scope_id: ScopeId,
    },

    Return {
        expr: Option<Box<AstNode<'src>>>,
    },

    Debug {
        expr: Box<AstNode<'src>>,
    },

    RecordDefinition {
        sym: Box<AstNode<'src>>,
        sym_id: SymbolId,
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

impl AstNodeVariant<'_> {
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
            | AstNodeVariant::FunctionDefinition { body, .. }
            | AstNodeVariant::If { body, .. }
            | AstNodeVariant::Else { body, .. }
            | AstNodeVariant::While { body, .. }
            | AstNodeVariant::Each { body, .. } => body,
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
        if let AstNodeVariant::FunctionDefinition { params, .. } = self {
            params
        } else {
            unreachable!()
        }
    }

    pub fn return_tn(&self) -> Option<&AstNode> {
        if let AstNodeVariant::FunctionDefinition { return_tn, .. } = self {
            return_tn.as_deref()
        } else {
            unreachable!()
        }
    }

    pub fn unwrap_symbol(&self) -> &str {
        if let AstNodeVariant::Symbol { name } = self {
            name
        } else {
            unreachable!()
        }
    }
}

// impl<'src> AstNodeVariant<'src> {
//     pub fn unwrap_group(self) -> AstNode<'src> {
//         match self {
//             Self::Group { expr, .. } => expr.unwrap_group(),
//             other => if let
//         }
//     }
// }

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
