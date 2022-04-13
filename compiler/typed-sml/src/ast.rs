
pub type Id = String;

#[derive(Debug, PartialEq)]
pub enum Typ {
    Unknown,
    Ident(String),
    Tuple(Vec<Typ>),
    Arrow(Box<Typ>, Box<Typ>)
}

impl Default for Typ {
    fn default() -> Self { Typ::Unknown }
}

// /* Parameters, like [a : t]*/
// #[derive(Debug, PartialEq)]
// pub struct Param { id: Id, typ: Typ }

/* Literals */
#[derive(Debug, PartialEq)]
pub enum Constant {
    Unit,
    Integer(i64),
    Boolean(bool)
}

/* Binary operators. Note "-" only function as a binop */
#[derive(Debug, PartialEq, Clone)]
pub enum Binary {
    Add, Sub, Mul,
    Eq, Lt, Gt, Le, Ge, Ne,
    Andalso, Orelse
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

/// Annotated variable
#[derive(Debug, PartialEq)]
pub struct Annot { pub var: Id, pub typ: Typ }

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Constants 
    Con{ constnt: Constant, typ: Typ },
    /// Identifiers
    Var{ id: Id, typ: Typ },
    // /// Unary operations
    // Unop{ op: Unary, kid: Box<Expr>, typ: Typ },
    /// Tuples, duh
    Tuple{ coords: Vec<Expr>, typ: Typ },
    /// Binary operations
    Binop{ op: Binary, lhs: Box<Expr>, rhs: Box<Expr>, typ: Typ },
    Lambda{ args: Vec<Annot>, body: Box<Expr>, typ: Typ },
    Branch{ cond: Box<Expr>, br_t: Box<Expr>, br_f: Box<Expr>, typ: Typ },
    /// Function application
    App { fun: Box<Expr>, arg: Box<Expr>, typ: Typ } 
    // Let { bindings: Vec<Binding>, body: Box<Expr> }
}


impl Expr {

    // /* Immediate children of this expression */
    // fn children(&self) -> Vec<&Expr> {
    // 	match self {
    // 	    Data::Lit{ constnt: _ } => vec![],
    // 	    Data::Unop{ op: _, kid} => vec![kid],
    // 	    Data::Binop{ op: _, lhs, rhs } => vec![lhs, rhs],
    // 	    Data::Lambda{ args: _, body } => vec![body],
    // 	    Data::Branch{ cond, br_t, br_f } =>vec![cond, br_t, br_f],
    // 	    Data::Let { bindings: _, body } => vec![exp, body]
    // 	}
    // }
}
