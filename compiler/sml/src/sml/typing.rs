use crate::sml::ast::{Typ, Expr, Binary, Constant};
use std::collections::{HashSet, HashMap};

/// Typing context
pub struct Context {
    // Top-level type identifiers' bindings stay fixed
    default: HashSet<String>,
    // All the bindings, basically
    bindings: HashMap<String, Typ>,
    /// What to revert to. `None` means to remove it
    /// from `bindings`
    diffs: Vec<Vec<(String, Option<Typ>)>>
}


impl Context {

    /// Initialize
    pub fn init(&mut self) {
	self.default = HashSet::new();
	self.bindings = HashMap::new();
	self.diffs = vec![];
    }

    pub fn get(&self, id: &String) -> &Typ {
	self.bindings.get(id).unwrap()
    }

    /// Bind a to the current context
    pub fn bind(&mut self, id: &String, typ: &Typ) {
	if self.default.contains(id) {
	    // Panics when trying to change a default binding
	    // TODO throw proper error instead
	    panic!();
	} else {
	    let old = self.bindings.insert(id.clone(), typ.clone());
	    let top = self.diffs.last_mut().unwrap();
	    top.push((id.clone(), old));
	}
    }

    /// Enters a local context
    pub fn enter(&mut self) {
	self.diffs.push(vec![]);
    }

    /// Exits from a local context and reverts to parent context
    pub fn exeunt(&mut self) {
	let diff = self.diffs.pop().unwrap();
	for (name, bind) in diff {
	    match bind {
		Some(typ) => self.bindings.insert(name, typ),
		None => self.bindings.remove(&name)
	    };
	}
    }
}

// TODO throw proper error instead of panicking
/// Type annotates an expression. Returns `typ` which is the type of `expr`
/// Panics if `expr` is not well-typed
pub fn type_expr<'a>(expr: &'a mut Expr, context: &mut Context) -> &'a Typ {
    match expr {
	Expr::Con { constnt, typ } => {
	    *typ = match constnt {
		Constant::Unit => Typ::Unit,
		Constant::Integer(_i) => Typ::Int,
		Constant::Boolean(_b) => Typ::Bool
	    };
	    typ
	},
	Expr::Var { id, typ } => {
	    *typ = context.get(id).to_owned();
	    typ
	},
	// Coercion
	Expr::App { fun, arg, typ } => {
	    let fun_typ = type_expr(fun, context);
	    let arg_typ = type_expr(arg, context);
	    *typ = if let Typ::Arrow(at, bt) = fun_typ {
		if *arg_typ == **at {
		    (*bt.as_ref()).clone()
		} else {
		    panic!();
		}
	    } else {
		panic!();
	    };
	    typ
	},
	Expr::Let { bindings, body, typ } => {
	    for valbind in bindings {
		context.enter();
		let tipe = type_expr(&mut valbind.exp, context);
		context.exeunt();
		context.bind(&valbind.id, tipe);
	    }
	    *typ = type_expr(body, context).clone();
	    typ
	},
	Expr::Tuple { entries, typ } => {
	    let mut typs = vec![];
	    for entry in entries {
		context.enter();
		typs.push(type_expr(entry, context).clone());
		context.exeunt();
	    }
	    *typ = Typ::Tuple(typs);
	    typ
	},
	Expr::Binop { op, lhs, rhs, typ } => {
	    use Binary::*;
	    context.enter();
	    let typ_l = type_expr(lhs, context);
	    context.exeunt();
	    context.enter();
	    let typ_r = type_expr(rhs, context);
	    context.exeunt();
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
	    };
	    typ    
	},
	Expr::Lambda { args, body, typ } => {
	    for arg in args {
		context.bind(&arg.var, &arg.typ);
	    }
	    *typ = type_expr(body, context).clone();
	    typ
	},
	Expr::Branch { cond, br_t, br_f, typ } => {
	    context.enter();
	    let t_cond = type_expr(cond, context);
	    context.exeunt();
	    if matches!(t_cond, Typ::Bool) {
		context.enter();
		let t_true = type_expr(br_t, context);
		context.exeunt();
		context.enter();
		let t_false = type_expr(br_f, context);
		context.exeunt();
		if t_true == t_false {
		    *typ = t_true.clone();
		    typ
		} else {
		    panic!()
		}
	    } else {
		panic!()
	    }
	}
    }
}


