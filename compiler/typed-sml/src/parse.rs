lalrpop_mod!(pub parser); // synthesized by LALRPOP

pub mod utils {
    use crate::ast::*;

    pub fn make_binop(l: Expr, op: &str, r: Expr) -> Expr {
	Expr::Binop { op: Binary::of_str(op),
		      lhs: Box::new(l),
		      rhs: Box::new(r),
		      typ: Typ::Unknown }
    }
}
