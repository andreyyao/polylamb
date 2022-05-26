use crate::sml::ast::{Typ, Expr, Binary, Constant};
use crate::util::Context;


/// Type annotates an expression
pub fn typecheck_expr (expr: &mut Expr) {
    let context = Context::new();
    type_expr(expr, &mut context);
}


// TODO throw proper error instead of panicking
/// Type annotates an expression. Returns `typ` which is the type of `expr`
/// Panics if `expr` is not well-typed
fn type_expr(expr: &mut Expr, context: &mut Context<Typ>) {
    match expr {
	Expr::Con { constnt, typ } => {
	    *typ = match constnt {
		Constant::Integer(_i) => Typ::Int,
		Constant::Boolean(_b) => Typ::Bool
	    }
	},
	Expr::Var { id, typ } => {
	    *typ = context.get(id).to_owned()
	},
	// Coercion TODO take care of polymorphism
	Expr::App { fun, arg, typ, .. } => {
	    type_expr(fun, context);
	    type_expr(arg, context);
	    *typ = if let Typ::Arrow(at, bt) = fun.typ() {
		if *arg.typ() == **at {
		    (*bt.as_ref()).clone()
		} else {
		    panic!();
		}
	    } else {
		panic!();
	    }
	},
	Expr::Let { bindings, body, typ } => {
	    for valbind in bindings {
		context.enter();
		type_expr(&mut valbind.exp, context);
		context.exeunt();
		context.bind(&valbind.id, valbind.exp.typ());
	    }
	    type_expr(body, context);
	    *typ = body.typ().clone();
	},
	Expr::Tuple { entries, typ } => {
	    let mut typs = vec![];
	    for entry in entries {
		context.enter();
		type_expr(entry, context);
		typs.push(entry.typ().clone());
		context.exeunt();
	    }
	    *typ = Typ::Prod(typs);
	},
	Expr::Binop { op, lhs, rhs, typ } => {
	    use Binary::*;
	    context.enter();
	    type_expr(lhs, context);
	    context.exeunt();
	    context.enter();
	    type_expr(rhs, context);
	    context.exeunt();
	    let typ_l = lhs.typ();
	    let typ_r = rhs.typ();
	    *typ = match op {
		Add | Sub | Mul => {
		    match (typ_l, typ_r) {
			(Typ::Int, Typ::Int) => Typ::Int,
			_ => panic!()
		    }
		},
		Lt | Gt | Le | Ge => {
		    match (typ_l, typ_r) {
			(Typ::Int, Typ::Int) => Typ::Bool,
			_ => panic!()
		    }
		},
		Andalso | Orelse => {
		    match (typ_l, typ_r) {
			(Typ::Bool, Typ::Bool) => Typ::Bool,
			_ => panic!()
		    }
		},
		Eq | Ne => {
		    if typ_l.is_equality_type() && typ_l == typ_r {
			Typ::Bool
		    } else {
			panic!()
		    }
		}
	    }
	},
	Expr::Lambda { args, body, typ, .. } => {
	    for arg in args {
		context.bind(&arg.var, &arg.typ);
	    }
	    type_expr(body, context);
	    *typ = body.typ().clone()
	},
	Expr::Branch { cond, br_t, br_f, typ } => {
	    context.enter();
	    type_expr(cond, context);
	    context.exeunt();
	    let t_cond = cond.typ();
	    if matches!(t_cond, Typ::Bool) {
		context.enter();
		type_expr(br_t, context);
		context.exeunt();
		context.enter();
		type_expr(br_f, context);
		context.exeunt();
		if br_t.typ() == br_f.typ() {
		    *typ = br_t.typ().clone()
		} else {
		    panic!()
		}
	    } else {
		panic!()
	    }
	}
    }
}
