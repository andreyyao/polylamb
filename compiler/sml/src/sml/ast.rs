
type Id = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Unknown,
    Int,
    Bool,
    Unit,
    // Poly(String), // Polymorphic types
    // PolyEq(String), // Equality types
    Prod(Vec<Typ>),
    Arrow(Box<Typ>, Box<Typ>)
}

// /* Parameters, like [a : t]*/
// #[derive(Debug, PartialEq)]
// pub struct Param { id: Id, typ: Typ }

/// Literals ///
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Integer(i64),
    Boolean(bool)
}

/// Binary operands
/* Binary operators. Note "-" only function as a binop */
#[derive(Debug, PartialEq, Clone)]
pub enum Binary {
    Add, Sub, Mul,
    Eq, Lt, Gt, Le, Ge, Ne,
    Andalso, Orelse
}

/// Annotated variables
#[derive(Debug, PartialEq)]
pub struct Annot { pub var: Id, pub typ: Typ }

/// Value bindings, like `val x = 2` or something
#[derive(Debug, PartialEq)]
pub struct ValBind { pub id: Id, pub exp: Expr, pub rec: bool }

/// Expressions
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Constants 
    Con { constnt: Constant, typ: Typ },
    /// Identifiers aka variables
    Var { id: Id, typ: Typ },
    /// Function application
    App { fun: Box<Expr>, arg: Box<Expr>,/* foralls: Vec<(Id, Typ)>,*/ typ: Typ },
    /// `Let valbind+ in body end`
    Let { bindings: Vec<ValBind>, body: Box<Expr>, typ: Typ },
    /// Tuples, n >= 2
    Tuple { entries: Vec<Expr>, typ: Typ },
    /// Binary operations
    Binop { op: Binary, lhs: Box<Expr>, rhs: Box<Expr>, typ: Typ },
    /// Anonymous functions
    Lambda { args: Vec<Annot>, body: Box<Expr>,/* foralls: Vec<Typ>,*/ typ: Typ },
    /// `if b then e1 else e2`
    Branch { cond: Box<Expr>, br_t: Box<Expr>, br_f: Box<Expr>, typ: Typ },
}

pub type Prog = Vec<ValBind>;


impl Expr {
    /// `e.typ()` is the type of this expression `e`
    pub fn typ(&self) -> &Typ {
	match self {
	    | Expr::Con { typ, .. }
	    | Expr::Var { typ, .. }
	    | Expr::App { typ, .. }
	    | Expr::Let { typ, .. }
	    | Expr::Tuple { typ, .. }
	    | Expr::Binop { typ, .. }
	    | Expr::Lambda { typ, .. }
	    | Expr::Branch { typ, .. } => typ
	}
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
	    "=" => Eq,
	    "<" => Lt,
	    ">" => Gt,
	    "<=" => Le,
	    ">=" => Ge,
	    "<>" => Ne,
	    "andalso" => Andalso,
	    "orelse" => Orelse,
	    _ => panic!(" At the Disco")
	}
    }
}


impl Typ {
    /// Returns true if one can compare types of this kind.
    /// panics if input type is `Unknown`
    pub fn is_equality_type(&self) -> bool {
	match self {
	    Typ::Unknown => panic!(), // Shouldn't happen
	    Typ::Int => true,
	    Typ::Bool => true,
	    Typ::Unit => true,
	    Typ::Prod(ts) => ts.iter().all(|t| t.is_equality_type()),
	    Typ::Arrow(_, _) => false
	}
    }
}
