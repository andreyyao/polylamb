type Id = String;
type Tipe = String;

/* Parameters, like [a : t]*/
pub struct Param { id: Id, typ: Tipe }

/* Literals */
pub enum Constant {
    Integer(i64),
    Boolean(bool)
}

/* Unary operators */
pub enum Unary {
    Not, Neg
}

/* Binary operators. Note "-" only function as a binop */
pub enum Binary {
    Add, Sub, Mul, Div,
    Eq, Lt, Gt, Le, Ge, Ne,
    Andalso, Orelse
}

pub enum Data {
    /* Constants */
    Con{ constnt: Constant },
    /* Unary operations */
    Unop{ op: Unary, kid: Box<Expr> },
    /* Binary operations */
    Binop{ op: Binary, lhs: Box<Expr>, rhs: Box<Expr> },
    Lambda{ args: Vec<Param>, body: Box<Expr> },
    Branch{ cond: Box<Expr>, br_t: Box<Expr>, br_f: Box<Expr> },
    App { fun: Id, args: Vec<Expr> } 
    // Let { bindings: Vec<Binding>, body: Box<Expr> }
}

// pub struct Binding { arg: Param, exp: Box<Expr> }


pub struct Expr { exp: Data, typ: Tipe }


impl Data {

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
