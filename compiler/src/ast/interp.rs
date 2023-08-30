use crate::ast::ast::{Binary, Constant, Expr, RawExpr, RawPattern, RawType};
use crate::util::persistent::{adventure, Snapshot};
/** Interpreting for the System F AST */
use std::collections::HashMap;
use std::fmt::Display;

use super::ast::{Decl, Prog};
use super::error::TypeError;
use super::semant::{check_decl, check_expr};

// /** Evaluates `expr` under `store` */
// pub fn eval_expr(expr: &Expr, store: &mut Snapshot<Store>) -> Result<RawExpr, TypeError> {
//     let mut ctxt = Snapshot::new(store.current().typ_store.clone());
//     ctxt.enter();
//     check_expr(&expr, &mut ctxt, &mut Snapshot::default())?;
//     ctxt.exeunt();
//     Ok(eval(store, expr))
// }

/** Evaluates `decl` under current `store`, and add value to `store` */
pub fn eval_decl(decl: &Decl, store: &Store) -> Result<(), TypeError> {
    // let mut ctxt = Snapshot::new(store.current().typ_store.clone());
    // ctxt.enter();
    // check_decl(decl, &mut ctxt)?;
    // ctxt.exeunt();
    // let body = eval(store, &decl.body.expr);
    // let curr = store.current();
    // curr.val_store.insert(decl.id.clone(), body);
    // curr.typ_store.insert(decl.id.clone(), decl.sig.typ.clone());
    // Ok(())
    todo!()
}

// /** Evaluates program */
// pub fn eval_prog(prog: &Prog) -> Result<(), TypeError> {
//     let mut store = Snapshot::default();
//     for id in &prog.order {
//         let decl = &prog.declarations[id];
//         eval_decl(decl, &mut store)?;
//     }
//     Ok(())
// }

pub fn eval_closed_expr(expr: &Expr) -> RawExpr {
    let store = HashMap::default();
    eval(&store, expr).into()
}

impl Into<RawExpr> for Value {
    fn into(self) -> RawExpr {
        match self {
            Value::VConst(v) => RawExpr::Con { val: v },
            Value::VTuple(vs) => {
		let es = vs.iter().map(|v| Expr::new(Into::<RawExpr>::into(v.clone()))).collect();
		RawExpr::Tuple { entries: es }
	    },
            Value::VClosure(v, _) => v,
	    Value::VAny(v, _) => v
        }
    }
}

type Environment = HashMap<String, Value>;

#[derive(Clone)]
enum Value {
    VConst(Constant),
    VTuple(Vec<Value>),
    VClosure(RawExpr, Environment),
    VAny(RawExpr, Environment)
}

/** The evaluation function that returns the value of `expr` under the `store`, while potentially updating `store` with new bindings. */
fn eval(env: &Environment, expr: &RawExpr) -> Value {
    use RawExpr::*;
    use Value::*;
    // println!("-------------------------------");
    // println!("Store:\n{}", store);
    // println!("Evaluating: {}", expr);
    // println!("-------------------------------");
    match &expr {
        // Constants being constants
        Con { val } => Value::VConst(val.clone()),
        // Yeah
        Var { id } => env[id].clone(),
        Let { pat, exp, body } => {
            todo!()
        }
        EApp { exp, arg } => match eval(env, exp) {
            Value::VClosure(
                Lambda {
                    arg: (id, _),
                    body,
                },
                e,
            ) => {
                let b = eval(env, arg);
                let mut map = e.clone();
                map.insert(id.name, b);
                eval(&map, &body.expr)
            }
            _ => panic!("{}", TYPE_ERR_MSG),
        },
        // TODO properly apply
        TApp { .. } => { todo!() }
        Tuple { entries } => {
            let neu = entries.iter().map(|e| (eval(env, e))).collect();
            Value::VTuple(neu)
        }
        Binop { lhs, op, rhs } => {
            use Binary::*;
            use Constant::*;
            match op {
                // Integer arguments
                Add | Sub | Mul | Eq | Lt | Gt | Ne => {
                    let lhs_nf = eval(env, lhs);
                    let rhs_nf = eval(env, rhs);
                    if let (VConst(Integer(l)), VConst(Integer(r))) = (&lhs_nf, &rhs_nf) {
                        VConst(match op {
                            Add => Integer(l + r),
                            Sub => Integer(l - r),
                            Mul => Integer(l * r),
                            Eq => Boolean(l == r),
                            Lt => Boolean(l < r),
                            Gt => Boolean(l > r),
                            Ne => Boolean(l != r),
                            // Unreachable
                            _ => panic!(),
                        })
                    } else {
                        panic!()
                    }
                }
                _ => {
                    let lhs_nf = eval(env, lhs);
                    let rhs_nf = eval(env, rhs);
                    if let (VConst(Boolean(l)), VConst(Boolean(r))) = (&lhs_nf, &rhs_nf) {
                        match op {
			    And => VConst(Boolean(l & r)),
			    Or => VConst(Boolean(l | r)),
                            _ => panic!(),
                        }
                    } else { panic!() }

                }
            }
        }
        Lambda { .. } => VClosure(expr.clone(), env.clone()),
        Any { .. } => VAny(expr.clone(), env.clone()),
        If {
            cond,
            branch_t,
            branch_f,
        } => {
            if let VConst(Constant::Boolean(b)) = eval(&env, cond) {
                if b {
                    eval(env, branch_t)
                } else {
                    eval(env, branch_f)
                }
            } else {
                panic!("{}", TYPE_ERR_MSG)
            }
        }
    }
}

/// Pattern matches `pat` recursively and binds to `exp`
// fn bind_pat(exp: &RawExpr, pat: &RawPattern, store: &mut Snapshot<Store>) {
// match (exp, pat) {
//     (RawExpr::Tuple { entries }, RawPattern::Tuple(patterns)) => {
//         // Since we type check beforehand, these two vectors must have the same length
//         for (e, p) in entries.iter().zip(patterns) {
//             bind_pat(e, p, store)
//         }
//     }
//     (_, RawPattern::Wildcard(_)) => (),
//     (_, RawPattern::Binding(id, typ)) => {
//         let value = eval(store, exp);
//         let curr = store.current();
//         curr.val_store.insert(id.to_string(), value);
//         curr.typ_store.insert(id.to_string(), typ.typ.clone());
//     }
//     _ => panic!("{}", TYPE_ERR_MSG),
// }

// // Returns Some(ref), where `ref` is where the variable `v` occurs inside pattern `p`. None otherwise.
// fn find_binding<'a>(p: &'a mut Pattern, v: &'a str) -> Option<&'a mut String> {
//     match &mut p.pat {
// 	RawPattern::Wildcard(_) => None,
// 	RawPattern::Binding(u, _) => {
// 	    if u.name == v { Some(&mut u.name) }
// 	    else { None }
// 	}
// 	RawPattern::Tuple(pats) => {
// 	    for pat in pats {
// 		let cont = find_binding(pat, v);
// 		if cont.is_some() { return cont }
// 	    }
// 	    return None
// 	}
//     }
// }

// impl RawPattern {
//     /// Whether `self` contains the variable `var`
//     fn contains_var(&self, var: &str) -> bool {
// 	match self {
// 	    RawPattern::Wildcard(_) => false,
// 	    RawPattern::Binding(v, _) => v.name == var,
// 	    RawPattern::Tuple(pats) => pats.iter().any(|p| p.contains_var(var)),
// 	}
//     }
// }

// /// Returns `true` iff `var` is a free variable somewhere in `expression`
// fn free_var(var: &str, expression: &RawExpr) -> bool {
//     use RawExpr::*;
//     match expression {
//         Con { .. } => false,
//         Var { id } => id == var,
//         Let { pat, exp, body } => {
// 	    free_var(var, exp) |
// 	    (!&pat.contains_var(var) & free_var(var, body))
// 	}
//         EApp { exp, arg } => {
// 	    free_var(var, exp) | free_var(var, arg)
// 	}
//         TApp { exp, .. } => free_var(var, exp),
//         Tuple { entries } => entries.iter().any(|e| free_var(var, e)),
//         Binop { lhs, op: _, rhs } => {
// 	    free_var(var, lhs) | free_var(var, rhs)
// 	}
//         Lambda { arg, body } => {
// 	    (arg.0.name != var) & free_var(var, body)
// 	}
//         Any { arg: _, body } =>
// 	    free_var(var, body),
//         If { cond, branch_t, branch_f } => {
// 	    free_var(var, cond) |
// 	    free_var(var, branch_t) |
// 	    free_var(var, branch_f)
// 	}
//     }
// }

// /** Performs capture-avoiding substitution
//     `expression`: The expression to perform substitute on
//     `var`: The variable to substitute
//     `val`: The value to sub for
//  */
// fn subst(expression: &mut RawExpr, var: &str, val: &RawExpr) {
//     use RawExpr::*;

//     match expression {
//         Con { .. } => (),
//         Var { id } => {
//             if id == var {
//                 *expression = val.clone();
//             } else {
//                 ()
//             }
//         },
// 	// Since `let x = e1 in e2` is syntactic sugar for `(\x. e2) e1` in STLC, we want to substitute e1, which is `exp` here.
//         Let { pat, exp, body } => {
// 	    subst(exp, var, val);
// 	    // Do nothing if same variable is bound in `pat`
// 	    if pat.contains_var(var) {
// 		()
// 	    } else {

// 	    }
// 	}
//         EApp { exp, arg } => {
// 	    subst(exp, var, val);
// 	    subst(arg, var, val)
// 	}
//         TApp { exp, .. } => {
// 	    subst(exp, var, val)
// 	}
//         Tuple { entries } => entries
//             .iter_mut()
//             .for_each(|e| subst(e, var, val)),
//         Binop { lhs, op: _, rhs } => {
//             subst(lhs, var, val);
//             subst(rhs, var, val)
//         }
//         Lambda { arg, body } => todo!(),
//         Any { arg: _, body } => {
// 	    subst(body, var, val)
// 	}
//         If {
//             cond,
//             branch_t,
//             branch_f,
//         } => {
// 	    subst(cond, var, val);
// 	    subst(branch_t, var, val);
// 	    subst(branch_f, var, val)
// 	}
//     }
// }

// fn substitute_type(exp: RawExpr, var: &str, typ: &RawType) -> RawExpr {
//     todo!()
// }

const TYPE_ERR_MSG: &str =
    "Type mismatch during interpretation. This shouldn't happen. Did you typecheck?";

/** A struct representing the "Store"s.
`val_store` is mapping from variable names to its (value, type) pair
`typ_store` maps type variable names to types */
#[derive(Clone, Default)]
pub struct Store {
    val_store: HashMap<String, RawExpr>,
    typ_store: HashMap<String, RawType>,
}

impl Display for Store {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, v) in &self.val_store {
            let t = &self.typ_store[k];
            writeln!(f, "{k} : {t} := {v}")?
        }
        write!(f, "")
    }
}

impl Store {
    fn get_val(&self, key: &str) -> Option<&RawExpr> {
        self.val_store.get(key)
    }
}
