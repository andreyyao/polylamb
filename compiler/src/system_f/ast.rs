
/// Just a type alias
type Id = String;

/// The type of types : )
#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Unknown,
    Int,
    Bool,
    Unit,
    TVar(Id),
    Prod(Vec<Typ>),
    Arrow(Box<Typ>, Box<Typ>),
    Forall(Vec<Id>, Box<Typ>)
}


/// Literals
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Unit,
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
pub struct Annot { pub eid: Id, pub typ: Typ }


/// Expressions
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Constants 
    Con { val: Constant, typ: Typ },
    /// Identifiers aka variables
    Var { id: Id, typ: Typ },
    /// Let  in body end
    Let { annot: Annot, exp: Box<Expr>, body: Box<Expr>, typ: Typ },
    /// Expression function application
    EApp { exp: Box<Expr>, arg: Box<Expr>, typ: Typ },
    /// Type concretization, ex. `f[int]`
    TApp { exp: Box<Expr>, arg: Typ, typ: Typ },
    /// Tuples, n >= 2
    Tuple { entries: Vec<Expr>, typ: Typ },
    /// Pattern matching
    Match { exp: Box<Expr>, clauses: Vec<(Pattern, Expr)>, typ: Typ },
    /// Binary operations
    Binop { lhs: Box<Expr>, op: Binary, rhs: Box<Expr>, typ: Typ },
    /// Functions that take eid's as arguments, ex. `lambda x: int. x + 1
    Lambda { args: Vec<Annot>, body: Box<Expr>, typ: Typ },
    /// Type abstractions, ex. `any X. (lambda x: X. x)`
    Any { args: Vec<Id>, body: Box<Expr>, typ: Typ },
    /// if [cond] then [t] else [f]
    If { cond: Box<Expr>, t: Box<Expr>, f: Box<Expr>, typ: Typ },
}


/// Top level function declarations
#[derive(Debug, PartialEq)]
pub struct Decl {
    pub id: Id, pub sig: Typ, pub body: Expr
}


pub type Prog = Vec<Decl>;



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


// impl Typ {
//     /// Returns true if one can compare types of this kind.
//     /// panics if input type is `Unknown`
//     pub fn is_equality_type(&self) -> bool {
// 	match self {
// 	    Typ::Unknown => panic!(), // Shouldn't happen
// 	    Typ::Int => true,
// 	    Typ::Bool => true,
// 	    Typ::Unit => true,
// 	    Typ::Prod(ts) => ts.iter().all(|t| t.is_equality_type()),
// 	    Typ::Arrow(_, _) => false
// 	}
//     }
// }
