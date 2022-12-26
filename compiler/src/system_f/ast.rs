/*! The definition for the System F AST structure,
as well as some utility functions related to it. */

use std::{fmt, ops::{Deref, DerefMut}, fmt::{Display, Debug}, collections::HashMap};


/// The entire program
#[derive(Debug, PartialEq, Clone)]
pub struct Prog {
    /// Declarations
    pub declarations: HashMap<String, Decl>,
    /// Order of declarations
    pub order: Vec<String>
}


/// Top level declarations
#[derive(Debug, PartialEq, Clone)]
pub struct Decl {
    pub id: String,
    pub sig: Type,
    pub body: Expr
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
    Forall(String, Box<Type>),
    /// Type application
    App(Box<Type>, Box<Type>),
}


/// Expressions without metadata
#[derive(Debug, PartialEq, Clone)]
pub enum RawExpr {
    /// Constants
    Con { val: Constant },
    /// Stringentifiers aka variables
    Var { id: String },
    /// `let [annot] = [exp] in [body] end`
    Let { annot: Annot, exp: Box<Expr>, body: Box<Expr> },
    /// Expression function application
    EApp { exp: Box<Expr>, arg: Box<Expr> },
    /// Type concretization, ex. `f[int]`
    TApp { exp: Box<Expr>, arg: Type },
    /// Tuples, n >= 2
    Tuple { entries: Vec<Expr> },
    /// Pattern matching
    Match { exp: Box<Expr>, clause: (Pattern, Box<Expr>) },
    /// Binary operations
    Binop { lhs: Box<Expr>, op: Binary, rhs: Box<Expr> },
    /// Functions that take eid's as arguments, ex. `lambda x: int. x + 1
    Lambda { args: Vec<Annot>, body: Box<Expr> },
    /// Type abstractions, ex. `any X. (lambda x: X. x)`
    Any { poly: String, body: Box<Expr> },
    /// if [cond] then [t] else [f]
    If { cond: Box<Expr>, t: Box<Expr>, f: Box<Expr> },
}


/// The type of types : )
#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    pub typ: RawType,
    pub span: Option<Span>
}


/// Expression with extra metadata
#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub expr: RawExpr,
    pub span: Option<Span>
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
#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Var(String),
    Wildcard,
    Tuple(Vec<Pattern>)
}


/// Type-annotated variables
#[derive(Debug, PartialEq, Clone)]
pub struct Annot { pub var: String, pub typ: Type }


/// Start and end positions
#[derive(Debug, PartialEq, Clone)]
pub struct Span { start: usize, end: usize }


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

impl Prog {
    pub fn new() -> Prog {
	Prog { declarations: HashMap::new(), order: vec![] }
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
	Span { start, end }
    }
}

impl RawType {
    /// Whether the typ expression is atomic(doesn't contain smaller types)
    pub fn is_atomic(&self) -> bool {
        use RawType::*;
        matches!(self, Int | Bool | Unit | TVar(_))
    }
}

impl RawExpr {
    pub fn is_atomic(&self) -> bool {
	false
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
            "&&" => And,
            "||" => Or,
            _ => panic!(" At the Disco")
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
            RawType::Int => write!(f, "Int"),
            RawType::Bool => write!(f, "Bool"),
            RawType::Unit => write!(f, "Unit"),
            RawType::Prod(typs) => {
                for (i, t) in typs.iter().enumerate() {
                    if i != 0 { write!(f, " * ")?; }
                    fmt_composite(t, f)?;
                }
                write!(f, "")
            },
            RawType::Arrow(x, y) => {
                fmt_composite(x, f)?;
                write!(f, " -> ")?;
                fmt_composite(y, f)
            },
            RawType::Forall(v, t) => {
                write!(f, "∀ {v}. {t}")
            },
            RawType::TVar(v) => write!(f, "{v}"),
	    RawType::App(t1, t2) => write!(f, "{t1} {t2}")
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
	    RawExpr::Match { exp: _, clause: _ } => {
		panic!("TODO")
	    },
            RawExpr::Binop { lhs, op, rhs } => {
		write!(f, "({lhs}) {op} ({rhs})")
	    },
	    RawExpr::Lambda { args, body } => {
		write!(f, "λ ")?;
		for (i, a) in args.iter().enumerate() {
                    if i != 0 { write!(f, ", ")?; }
                    write!(f, "{a}")?;
                }
                write!(f, ". {body}")
	    },
	    RawExpr::Any { poly, body } => {
		write!(f, "Λ {poly}. {body}")
	    },
	    RawExpr::If { cond, t: tt, f: ff } => {
		write!(f, "if {cond} then {tt} else {ff}")
	    }
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
	    And => "&&",
	    Or => "||"
	};
	write!(f, "{symbol}")
    }
}

impl Display for Annot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r = &self.var;
        let t = &self.typ;
        write!(f, "{r}: {t}")
    }
}