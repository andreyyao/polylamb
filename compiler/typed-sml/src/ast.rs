
type Id = String;

#[derive(Debug, PartialEq)]
pub enum Typ {
    Unknown,
    Int,
    Bool,
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
    Integer(i64),
    Boolean(bool)
}

/* Unary operators */
#[derive(Debug, PartialEq)]
pub enum Unary {
    Not, Neg
}

/* Binary operators. Note "-" only function as a binop */
#[derive(Debug, PartialEq)]
pub enum Binary {
    Add, Sub, Mul, Mod,
    Eq, Lt, Gt, Le, Ge, Ne,
    Andalso, Orelse
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Constants 
    Con{ constnt: Constant, typ: Typ },
    /// Identifiers
    Var{ id: Id, typ: Typ },
    /// Unary operations
    Unop{ op: Unary, kid: Box<Expr>, typ: Typ },
    /// Tuples, duh
    Tuple{ args: Vec<Expr>, typ: Typ },
    /// Binary operations
    Binop{ op: Binary, lhs: Box<Expr>, rhs: Box<Expr>, typ: Typ },
    Lambda{ args: Vec<Id>, body: Box<Expr>, typ: Typ },
    Branch{ cond: Box<Expr>, br_t: Box<Expr>, br_f: Box<Expr>, typ: Typ },
    /// Function application
    App { fun: Id, arg: Box<Expr>, typ: Typ } 
    // Let { bindings: Vec<Binding>, body: Box<Expr> }
}

// pub struct Binding { arg: Param, exp: Box<Expr> }


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
