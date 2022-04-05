
type Id<'a> = &'a str;
type Tipe<'a> = &'a str;

pub enum Constant {
    Integer(i64),
    Boolean(bool)
}

pub enum Binop {
    Add, Sub, Mul, Div,
    Eq, Lt, Gt, Le, Ge, Ne,
    Andalso, Orelse
}

pub enum Unop {
    Not, Neg
}

pub enum Data<'a> {
    Const(Constant),
    UnoExpr{ op: Unop, kid: Box<Expr<'a>> },
    BinExpr{ op: Binop, lhs: Box<Expr<'a>>, rhs: Box<Expr<'a>> },
    Lambda{ args: Vec<Id<'a>>, body: Box<Expr<'a>> },
    Branch{ cond: Box<Expr<'a>>, br_t: Box<Expr<'a>>, br_f: Box<Expr<'a>> }
}

pub struct Expr<'a> { exp: Data<'a>, typ: Tipe<'a> }
