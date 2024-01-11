/*! The definition for the System F AST structure,
as well as some utility functions related to it. */

use colored::*;
use std::{
    collections::HashMap,
    fmt,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
};

/// The entire program
#[derive(Debug, PartialEq, Clone)]
pub struct Prog {
    /// Declarations
    pub declarations: HashMap<String, Decl>,
    /// Order of declarations
    pub order: Vec<String>,
}

/// Top level declarations
#[derive(Debug, PartialEq, Clone)]
pub struct Decl {
    pub id: String,
    pub sig: Type,
    pub body: Expr,
}

/// System F types without metadata
#[derive(Debug, PartialEq, Clone)]
pub enum RawType {
    Int,
    Bool,
    /// Unit has one value,
    Unit,
    /// Type variable, introduced by Forall types
    TVar(String),
    /// Product of more than 2 types
    Prod(Vec<Type>),
    /// Function types
    Arrow(Box<Type>, Box<Type>),
    /// Universal types
    Forall(Ident, Box<Type>),
}

/// Expressions without metadata
#[derive(Debug, PartialEq, Clone)]
pub enum RawExpr {
    /// Constants
    Con { val: Constant },
    /// Variables
    Var { id: String },
    /// `let [pat] = [exp] in [body]`
    Let {
        pat: Pattern,
        exp: Box<Expr>,
        body: Box<Expr>,
    },
    /// Recursive functions
    Fix {
        /// triples of (Func name, var name, var type, body, return type)
        funcs: Vec<(Ident, Ident, Type, Type, Expr)>,
        body: Box<Expr>,
    },
    /// Expression function application
    EApp { exp: Box<Expr>, arg: Box<Expr> },
    /// Type concretization, ex. `f [Int]`
    TApp { exp: Box<Expr>, arg: Type },
    /// Tuples, n >= 2
    Tuple { entries: Vec<Expr> },
    /// Binary operations
    Binop {
        lhs: Box<Expr>,
        op: Binary,
        rhs: Box<Expr>,
    },
    /// Functions, ex. `lambda (x: Int). x + 1
    Lambda { arg: (Ident, Type), body: Box<Expr> },
    /// Type abstractions, ex. `any X. (lambda (x: X). x)`
    Any { arg: Ident, body: Box<Expr> },
    /// if [cond] then [t] else [f]
    If {
        cond: Box<Expr>,
        branch_t: Box<Expr>,
        branch_f: Box<Expr>,
    },
}

/// Patterns
#[derive(Debug, PartialEq, Clone)]
pub enum RawPattern {
    /// Not producing binding `_`
    Wildcard,
    /// Variable binding like `x`
    Binding(Ident),
    /// Tuple patterns like `(x: Int, (y: Bool, _: A))`
    Tuple(Vec<Pattern>),
}

/// The type of types : )
#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    pub typ: RawType,
    pub span: Option<Span>,
}

/// Expression with extra metadata
#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub expr: RawExpr,
    pub span: Option<Span>,
}

/// Patterns with span
#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    pub pat: RawPattern,
    pub span: Option<Span>,
}

/// Literals
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Null,
    Integer(i64),
    Boolean(bool),
}

/// Binary operands. No division
#[derive(Debug, PartialEq, Clone)]
pub enum Binary {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
    Gt,
    Ne,
    And,
    Or,
}

/// Identifiers with span
#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Option<Span>,
}

/// Start and end positions
pub type Span = (usize, usize);

////////////////////////////////////////////////////////////////////////
/////////////////////////// Implementations ////////////////////////////
////////////////////////////////////////////////////////////////////////

impl Deref for Expr {
    type Target = RawExpr;
    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}

impl DerefMut for Expr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.expr
    }
}

impl Deref for Type {
    type Target = RawType;
    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl DerefMut for Type {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typ
    }
}

impl Deref for Pattern {
    type Target = RawPattern;
    fn deref(&self) -> &Self::Target {
        &self.pat
    }
}

impl DerefMut for Pattern {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.pat
    }
}

impl Prog {
    pub fn new() -> Prog {
        Prog {
            declarations: HashMap::new(),
            order: vec![],
        }
    }
}

impl Default for Prog {
    fn default() -> Self {
        Self::new()
    }
}

impl RawType {
    /// Whether the typ expression is atomic(doesn't contain smaller types)
    pub fn is_atomic(&self) -> bool {
        use RawType::*;
        matches!(self, Int | Bool | Unit | TVar(_))
    }
}

impl Type {
    pub fn new(typ: RawType) -> Type {
        Type { typ, span: None }
    }
}

impl Expr {
    pub fn new(expr: RawExpr) -> Expr {
        Expr { expr, span: None }
    }
}

impl Binary {
    /// Maps string rep of binops to their enum counterparts
    pub fn of_str(s: &str) -> Binary {
        use Binary::*;
        match s {
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "<" => Lt,
            ">" => Gt,
            "==" => Eq,
            "!=" => Ne,
            "&" => And,
            "|" => Or,
            _ => panic!(" At the Disco"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let typ = &self.typ;
        write!(f, "{typ}")
    }
}

impl Display for RawType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        /// Parenths over composite types to disambiguate parsing precedence
        fn fmt_composite(typ: &RawType, ff: &mut fmt::Formatter) -> fmt::Result {
            if typ.is_atomic() {
                write!(ff, "{typ}")
            } else {
                write!(ff, "({typ})")
            }
        }

        match self {
            RawType::Int => write!(f, "{}", "Int".blue()),
            RawType::Bool => write!(f, "{}", "Bool".blue()),
            RawType::Unit => write!(f, "{}", "Unit".blue()),
            RawType::Prod(typs) => {
                write!(f, "(")?;
                for (i, t) in typs.iter().enumerate() {
                    if i != 0 {
                        write!(f, " * ")?;
                    }
                    fmt_composite(t, f)?;
                }
                write!(f, ")")
            }
            RawType::Arrow(x, y) => {
                fmt_composite(x, f)?;
                write!(f, " -> ")?;
                fmt_composite(y, f)
            }
            RawType::Forall(v, t) => {
                write!(f, "∀ {v}. {t}")
            }
            RawType::TVar(v) => write!(f, "{}", v.blue()),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r = &self.expr;
        write!(f, "{r}")
    }
}

impl Display for RawExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RawExpr::*;
        /// Parenths over composite expressions to disambiguate
        fn atomize(ff: &mut fmt::Formatter, exp: &Expr) -> fmt::Result {
            if matches!(exp.expr, Con { .. } | Var { .. } | Tuple { .. }) {
                write!(ff, "{}", exp)
            } else {
                write!(ff, "({})", exp)
            }
        }

        match self {
            RawExpr::Con { val } => write!(f, "{}", val.to_string().yellow()),
            RawExpr::Var { id } => write!(f, "{}", id.to_string().red()),
            RawExpr::Let { pat, exp, body } => write!(f, "let {pat} = {exp} in {body}"),
            RawExpr::Fix { funcs, body } => {
                let (f_name, v_name, v_typ, ret_typ, exp) = &funcs[0];
                write!(
                    f,
                    "fix {} = λ ({}:{}) : {}. {}",
                    f_name, v_name, v_typ, ret_typ, exp
                )?;
                for (f_name, v_name, v_typ, ret_typ, exp) in funcs.iter().skip(1) {
                    write!(
                        f,
                        " and {} = λ ({}:{}) -> {}. {}",
                        f_name, v_name, v_typ, ret_typ, exp
                    )?;
                }
                write!(f, " in {}", body)
            }
            RawExpr::EApp { exp, arg } => {
                if matches!(exp.expr, EApp { .. }) { write!(f, "{exp}") }
                else { atomize(f, exp) }?;
                write!(f, " ")?;
                atomize(f, arg)
            }
            RawExpr::TApp { exp, arg } => {
                if matches!(exp.expr, TApp { .. }) { write!(f, "{exp}") }
                else { atomize(f, exp) }?;
                write!(f, "[{arg}]")
            },
            RawExpr::Tuple { entries } => {
                write!(f, "(")?;
                for (i, e) in entries.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            RawExpr::Binop { lhs, op, rhs } => {
                atomize(f, lhs)?;
                write!(f, " {op} ")?;
                atomize(f, rhs)
            }
            RawExpr::Lambda { arg: (v, t), body } => {
                write!(f, "λ {}: {}. {}", v.name.red(), t, body)
            }
            RawExpr::Any { arg, body } => {
                write!(f, "Λ {}. {}", arg.name.blue(), body)
            }
            RawExpr::If {
                cond,
                branch_t,
                branch_f,
            } => {
                write!(f, "(if {cond} then {branch_t} else {branch_f})")
            }
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pat = &self.pat;
        write!(f, "{pat}")
    }
}

impl Display for RawPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RawPattern::Tuple(patterns) => {
                write!(f, "(")?;
                for (i, p) in patterns.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{p}")?
                }
                write!(f, ")")
            }
            RawPattern::Binding(v) => write!(f, "{v}"),
            RawPattern::Wildcard => write!(f, "_"),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Integer(i) => write!(f, "{i}"),
            Constant::Boolean(b) => write!(f, "{b}"),
            Constant::Null => write!(f, "null"),
        }
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Binary::*;
        let symbol = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Eq => "==",
            Ne => "!=",
            Lt => "<",
            Gt => ">",
            And => "&",
            Or => "|",
        };
        write!(f, "{symbol}")
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = &self.name;
        write!(f, "{name}")
    }
}
