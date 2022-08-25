/*! The definition for the System F AST structure,
as well as some utility functions related to it. */

use std::{fmt, ops::{Deref, DerefMut}, fmt::{Display, Debug}, collections::HashMap};

/// Just a type alias
type Id = String;

/// The type of types : )
#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Unknown,
    Int,
    Bool,
    /// Unit has one value,
    Unit,
    /// Type variable, introduced by Forall types
    TVar(Id),
    /// Product of more than 2 types
    Prod(Vec<Typ>),
    /// Function types
    Arrow(Box<Typ>, Box<Typ>),
    /// Universal types
    Forall(Id, Box<Typ>)
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
    Add, Sub, Mul,
    Eq, Lt, Gt, Ne,
    And, Or
}

/// Patterns
#[derive(Debug, PartialEq)]
pub enum Pattern {
    Var(Id),
    Tuple(Vec<Pattern>)
}

/// Annotated variables
#[derive(Debug, PartialEq)]
pub struct Annot { pub var: Id, pub typ: Typ }

/// Expressions without typ annotation
#[derive(Debug, PartialEq)]
pub enum RawExpr {
    /// Constants
    Con { val: Constant },
    /// Identifiers aka variables
    Var { id: Id },
    /// `let [annot] = [exp] in [body] end`
    Let { annot: Annot, exp: Box<Expr>, body: Box<Expr> },
    /// Expression function application
    EApp { exp: Box<Expr>, arg: Box<Expr> },
    /// Type concretization, ex. `f[int]`
    TApp { exp: Box<Expr>, arg: Typ },
    /// Tuples, n >= 2
    Tuple { entries: Vec<Expr> },
    /// Pattern matching
    Match { exp: Box<Expr>, clause: (Pattern, Box<Expr>) },
    /// Binary operations
    Binop { lhs: Box<Expr>, op: Binary, rhs: Box<Expr> },
    /// Functions that take eid's as arguments, ex. `lambda x: int. x + 1
    Lambda { args: Vec<Annot>, body: Box<Expr> },
    /// Type abstractions, ex. `any X. (lambda x: X. x)`
    Any { poly: Id, body: Box<Expr> },
    /// if [cond] then [t] else [f]
    If { cond: Box<Expr>, t: Box<Expr>, f: Box<Expr> },
}


/// Expression with type annotation attached
#[derive(Debug, PartialEq)]
pub struct Expr {
    pub expr: RawExpr,
    pub typ: Typ
}

/// Top level function declarations
#[derive(Debug, PartialEq)]
pub struct Decl {
    pub id: Id, pub sig: Typ, pub body: Expr
}


pub type Prog = HashMap<Id, Decl>;


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

impl Expr {
    pub fn new(expr: RawExpr) -> Expr {
        Expr { expr, typ: Typ::Unknown }
    }
}

impl RawExpr {
    pub fn is_atomic(&self) -> bool {
        true
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
            "&&" => And,
            "||" => Or,
            _ => panic!(" At the Disco")
        }
    }
}


impl Typ {
    /// Whether the typ is atomic(doesn't contain smaller types)
    pub fn is_atomic(&self) -> bool {
        use Typ::*;
        matches!(self, Int | Bool | Unit)
    }
}


impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r = &self.expr;
        let t = &self.typ;
        write!(f, "{r} /*{t}*/")
    }
}

impl Display for Annot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r = &self.var;
        let t = &self.typ;
        write!(f, "{r} : {t}")
    }
}

impl Display for RawExpr {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        /// Parenths over composite expressions to disambiguate
        fn fmt_composite(exp: &Expr, ff: &mut fmt::Formatter) -> fmt::Result {
            if exp.is_atomic() {
                write!(ff, "{exp}")
            } else {
                write!(ff, "({exp})")
            }
        }

        match self {
            RawExpr::Con{ val } => write!(f, "{val}"),
            RawExpr::Var{ id } => write!(f, "{id}"),
            RawExpr::Let{ annot, exp, body } => write!(f, "{annot} = {exp} in {body}"),
            RawExpr::EApp { exp, arg } => write!(f, "{exp} {arg}"),
            RawExpr::TApp { exp, arg } => write!(f, "{exp}[{arg}]"),
            RawExpr::Tuple { entries } => {
                write!(f, "(")?;
                for (i, t) in entries.iter().enumerate() {
                    if i != 0 { write!(f, ", ")?; }
                    fmt_composite(t, f)?;
                }
                write!(f, ")")
            },
            _ => write!(f, "TODO")
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Integer(i) => {
                if i < &0 {
                    let i1 = -i;
                    write!(f, "~{i1}")
                } else {
                    write!(f, "{i}")
                }
            },
            Constant::Boolean(b) => write!(f, "{b}"),
            Constant::Null => write!(f, "null")
        }
    }
}

impl Display for Typ {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        /// Parenths over composite types to disambiguate parsing precedence
        fn fmt_composite(typ: &Typ, ff: &mut fmt::Formatter) -> fmt::Result {
            if typ.is_atomic() {
                write!(ff, "{typ}")
            } else {
                write!(ff, "({typ})")
            }
        }

        match self {
            Typ::Int => write!(f, "Int"),
            Typ::Bool => write!(f, "Bool"),
            Typ::Unit => write!(f, "Unit"),
            Typ::Unknown => write!(f, "Unknown"),
            Typ::Prod(typs) => {
                for (i, t) in typs.iter().enumerate() {
                    if i != 0 { write!(f, " * ")?; }
                    fmt_composite(t, f)?;
                }
                write!(f, "")
            },
            Typ::Arrow(x, y) => {
                fmt_composite(x, f)?;
                write!(f, " -> ")?;
                fmt_composite(y, f)
            },
            Typ::Forall(v, t) => {
                write!(f, "∀ {v}. {t}")
            },
            Typ::TVar(v) => write!(f, "{v}")
        }
    }
}
