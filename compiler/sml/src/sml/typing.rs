use crate::sml::ast::{Typ, Expr, Binary, Constant};
use std::{collections::HashMap};

/// Typing context
pub struct Context {
    /// Binding from value id to type, basically
    vid2typ: HashMap<String, Typ>,
    /// Binding from polymorphic type variables to type
    tid2typ: HashMap<String, Typ>,
    /// What to revert to. `None` means to remove it
    /// from `bindings`
    v_diffs: Vec<Vec<(String, Option<Typ>)>>,
    t_diffs: Vec<Vec<(String, Option<Typ>)>>
}


impl Context {

    /// Initialize
    pub fn init(&mut self) {
	self.vid2typ = HashMap::new();
	self.tid2typ = HashMap::new();
	self.v_diffs = vec![];
	self.t_diffs = vec![];
    }

    pub fn typ_of_vid(&self, id: &str) -> &Typ {
	self.vid2typ.get(id).unwrap()
    }

    pub fn typ_of_tid(&self, id: &str) -> &Typ {
	self.tid2typ.get(id).unwrap()
    }

    /// Bind a vid to the current context
    pub fn bind_vid(&mut self, vid: &str, typ: &Typ) {
	let old = self.vid2typ.insert(vid.to_string(), typ.clone());
	let top = self.v_diffs.last_mut().unwrap();
	top.push((vid.to_string(), old));
    }

    /// Bind a tid to the current context
    pub fn bind_tid(&mut self, tid: &str, typ: &Typ) {
	let old = self.tid2typ.insert(tid.to_string(), typ.clone());
	let top = self.t_diffs.last_mut().unwrap();
	top.push((tid.to_string(), old));
    }

    /// Enters a local context
    pub fn enter(&mut self) {
	self.v_diffs.push(vec![]);
	self.t_diffs.push(vec![]);
    }

    /// Exits from a local context and reverts to parent context
    pub fn exeunt(&mut self) {
	let v_diff = self.v_diffs.pop().unwrap();
	for (name, bind) in v_diff {
	    match bind {
		Some(typ) => self.vid2typ.insert(name, typ),
		None => self.vid2typ.remove(&name)
	    };
	};
	let t_diff = self.t_diffs.pop().unwrap();
	for (name, bind) in t_diff {
	    match bind {
		Some(typ) => self.tid2typ.insert(name, typ),
		None => self.tid2typ.remove(&name)
	    };
	}
    }
}


/// Type coercion before type checking function application
/// This maps type variables to appropriate stuff if possible
/// It tries to fit `candidate` into `constraint` and adds the
/// Appropriate mappings into `context` if possible.
/// Panics if not possible
fn coerce(candidate: &Typ, constraint: &Typ, context: &mut Context) {
    use Typ::*;
    match (candidate, constraint) {
	(Int, Int) | (Bool, Bool) | (Unit, Unit) => { },
	(PolyEq(s1), PolyEq(s2)) | (PolyEq(s1), Poly(s2)) => {
	    context.bind_tid(s2, &PolyEq(s1.to_string()));
	},
	(Poly(s1), Poly(s2)) => {
	    context.bind_tid(s2, &Poly(s1.to_string()));
	},
	(Tuple)
    }
}


// TODO throw proper error instead of panicking
/// Type annotates an expression. Returns `typ` which is the type of `expr`
/// Panics if `expr` is not well-typed
pub fn type_expr(expr: &mut Expr, context: &mut Context) {
    match expr {
	Expr::Con { constnt, typ } => {
	    *typ = match constnt {
		Constant::Unit => Typ::Unit,
		Constant::Integer(_i) => Typ::Int,
		Constant::Boolean(_b) => Typ::Bool
	    }
	},
	Expr::Var { id, typ } => {
	    *typ = context.typ_of_vid(id).to_owned()
	},
	// Coercion TODO take care of polymorphism
	Expr::App { fun, arg, typ } => {
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
		context.bind_vid(&valbind.id, valbind.exp.typ());
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
	    *typ = Typ::Tuple(typs);
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
	Expr::Lambda { args, body, typ } => {
	    for arg in args {
		context.bind_vid(&arg.var, &arg.typ);
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


