use crate::parsing::ast::{Typ, Expr, Constant};
use std::collections::{HashSet, HashMap};

/// Typing context
struct Context {
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

    /// Bind a variable to the current context
    pub fn bind(&mut self, id: String, typ: Typ) {
	if self.default.contains(&id) {
	    // Panics when trying to assign to default tid
	    // TODO throw proper error instead
	    panic!();
	} else {
	    let old = self.bindings.insert(id.clone(), typ);
	    let top = self.diffs.last_mut().unwrap();
	    top.push((id, old));
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
/// Type annotates an expression
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
	Expr::Let { bindings, body } => {
	    
	}
    }
}


