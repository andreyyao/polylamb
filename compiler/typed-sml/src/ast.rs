type Id = String;
type Tipe = String;

/* Parameters, like [a : t]*/
pub struct Param { id: Id, typ: Tipe }

//Literals
pub enum Constant {
    Integer(i64),
    Boolean(bool)
}

pub enum Unary {
    Not, Neg
}

pub enum Binary {
    Add, Sub, Mul, Div,
    Eq, Lt, Gt, Le, Ge, Ne,
    Andalso, Orelse
}


pub enum Data {
    Const{ constnt: Constant },
    Unop{ op: Unary, kid: Box<Expr> },
    Binop{ op: Binary, lhs: Box<Expr>, rhs: Box<Expr> },
    Lambda{ args: Vec<Param>, body: Box<Expr> },
    Branch{ cond: Box<Expr>, br_t: Box<Expr>, br_f: Box<Expr> },
    Let { arg: Param, exp: Box<Expr>, env: Box<Expr> }
}


pub struct Expr { exp: Data, typ: Tipe }


impl Data {

    /* Returns an immutable vector of immediate children */
    fn children(&self) -> Vec<&Expr> {
	match self {
	    Data::Const{ constnt: _ } => vec![],
	    Data::Unop{ op: _, kid} => vec![kid],
	    Data::Binop{ op: _, lhs, rhs } => vec![lhs, rhs],
	    Data::Lambda{ args: _, body } => vec![body],
	    Data::Branch{ cond, br_t, br_f } =>vec![cond, br_t, br_f],
	    Data::Let { arg: _, exp, env } => vec![exp, env]
	}
    }
}
