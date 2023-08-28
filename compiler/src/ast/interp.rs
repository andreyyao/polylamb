use crate::ast::ast::{Binary, Constant, Expr, RawExpr, RawPattern, RawType};
use crate::util::persistent::{adventure, Snapshot};
/** Interpreting for the System F AST */
use std::collections::HashMap;
use std::fmt::Display;

use super::ast::{Decl, Prog};
use super::error::TypeError;

/** Evaluates `expr` under `store` */
pub fn eval_expr(expr: &RawExpr, store: &mut Snapshot<Store>) -> RawExpr {
    eval(store, expr)
}

/** Evaluates `decl` under current `store`, and add value to `store` */
pub fn eval_decl(decl: &Decl, store: &mut Snapshot<Store>) -> Result<(), TypeError> {
    let body = eval(store, &decl.body.expr);
    store.current()
        .val_store
        .insert(decl.id.clone(), (body, decl.sig.typ.clone()));
    Ok(())
}

/** Evaluates program */
pub fn eval_prog(prog: &Prog) -> Result<(), TypeError> {
    let mut store = Snapshot::default();
    for id in &prog.order {
        let decl = &prog.declarations[id];
        eval_decl(decl, &mut store)?;
    }
    Ok(())
}

pub fn eval_closed_expr(expr: &RawExpr) -> RawExpr {
    let mut store = Snapshot::default();
    eval_expr(expr, &mut store)
}

/** The evaluation function that returns the value of `expr` under the store `store`, while potentially updating `store` with new bindings. */
fn eval(store: &mut Snapshot<Store>, expr: &RawExpr) -> RawExpr {
    use RawExpr::*;
    match &expr {
        // Constants being constants
        Con { val: _ } => expr.clone(),
        // Yeah
        Var { id } => match store.current().get_val(id) {
            Some(p) => p.0.clone(),
            None => Var { id: id.clone() },
        },
        Let { pat, exp, body } => {
            bind_pat(exp, pat, store);
            eval(store, body)
        }
        EApp { exp, arg } => {
            let func = eval(store, exp);
            let param = eval(store, arg);
            // lhs needs to be a value, which is a lambda expression by strength reduction
            match func {
                Lambda {
                    arg: (var, typ),
                    body,
                } => {
                    //Update the store
                    store.current().val_store.insert(var.name, (param, typ.typ));
                    eval(store, &body.expr)
                }
                _ => panic!("{}", TYPE_ERR_MSG),
            }
        }
        TApp { exp, arg } => {
            if let Any { arg: t, body } = eval(store, exp) {
                store.current().typ_store.insert(t.name, arg.typ.clone());
                eval(store, &body.expr)
            } else {
                panic!("{}", TYPE_ERR_MSG)
            }
        }
        Tuple { entries } => {
            let neu = entries.iter().map(|e| Expr::new(eval(store, e))).collect();
            Tuple { entries: neu }
        }
        Binop { lhs, op, rhs } => {
            use Binary::*;
            use Constant::*;
            match op {
                // Integer arguments
                Add | Sub | Mul | Eq | Lt | Gt | Ne => {
                    let lhs_nf = eval(store, lhs);
                    let rhs_nf = eval(store, rhs);
                    if let (Con { val: Integer(l) }, Con { val: Integer(r) }) = (&lhs_nf, &rhs_nf) {
                        match op {
                            Add => Con {
                                val: Integer(l + r),
                            },
                            Sub => Con {
                                val: Integer(l - r),
                            },
                            Mul => Con {
                                val: Integer(l * r),
                            },
                            Eq => Con {
                                val: Boolean(l == r),
                            },
                            Lt => Con {
                                val: Boolean(l < r),
                            },
                            Gt => Con {
                                val: Boolean(l > r),
                            },
                            Ne => Con {
                                val: Boolean(l != r),
                            },
                            // Unreachable
                            _ => panic!(),
                        }
                    } else {
                        Binop {
                            lhs: Box::new(Expr::new(lhs_nf)),
                            op: op.clone(),
                            rhs: Box::new(Expr::new(rhs_nf)),
                        }
                    }
                }
                _ => {
                    let lhs_nf = eval(store, lhs);
                    let rhs_nf = eval(store, rhs);
                    if let (Con { val: Boolean(l) }, Con { val: Boolean(r) }) = (&lhs_nf, &rhs_nf) {
                        match op {
                            And => Con {
                                val: Boolean(l & r),
                            },
                            Or => Con {
                                val: Boolean(l | r),
                            },
                            _ => panic!(),
                        }
                    } else {
                        Binop {
                            lhs: Box::new(Expr::new(lhs_nf)),
                            op: op.clone(),
                            rhs: Box::new(Expr::new(rhs_nf)),
                        }
                    }
                }
            }
        }
        // For lambda, substitute body, except for variable that is abstracted
        Lambda { arg, body } => {
            store.enter();
            store.current().val_store.remove(&arg.0.name);
            let body_new = eval(store, body);
            store.exeunt();
            Lambda {
                arg: arg.clone(),
                body: Box::new(Expr::new(body_new)),
            }
        }
        Any { arg, body } => {
            store.enter();
            store.current().typ_store.remove(&arg.name);
            let body_new = eval(store, body);
            store.exeunt();
            Any {
                arg: arg.clone(),
                body: Box::new(Expr::new(body_new)),
            }
        }
        If {
            cond,
            branch_t,
            branch_f,
        } => {
            adventure!(cond_new, eval(store, cond), store);
            // If terminates, it should normalize to boolean constant
            if let Con {
                val: Constant::Boolean(b),
            } = cond_new
            {
                if b {
                    eval(store, branch_t)
                } else {
                    eval(store, branch_f)
                }
            } else {
                adventure!(branch_t_new, eval(store, branch_t), store);
                adventure!(branch_f_new, eval(store, branch_f), store);
                If {
                    cond: Box::new(Expr::new(cond_new)),
                    branch_t: Box::new(Expr::new(branch_t_new)),
                    branch_f: Box::new(Expr::new(branch_f_new)),
                }
            }
        }
    }
}

/// Pattern matches `pat` recursively and binds to `exp`
fn bind_pat(exp: &RawExpr, pat: &RawPattern, store: &mut Snapshot<Store>) {
    match (exp, pat) {
        (_, RawPattern::Wildcard(_)) => { },
        (_, RawPattern::Binding(id, typ)) => {
            store.enter();
            let value = eval(store, exp);
            store.exeunt();
            store.current()
                .val_store
                .insert(id.to_string(), (value, typ.typ.clone()));
        }
        (RawExpr::Tuple { entries }, RawPattern::Tuple(patterns)) => {
            // Since we type check beforehand, these two vectors must have the same length
            for (e, p) in entries.iter().zip(patterns) {
                bind_pat(e, p, store)
            }
        }
        _ => panic!("{}", TYPE_ERR_MSG),
    }
}

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
    val_store: HashMap<String, (RawExpr, RawType)>,
    typ_store: HashMap<String, RawType>,
}

impl Display for Store {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, (v, t)) in &self.val_store {
            writeln!(f, "{k} : {t} := {v}")?
        }
        write!(f, "")
    }
}

impl Store {
    fn get_val(&self, key: &str) -> Option<&(RawExpr, RawType)> {
        self.val_store.get(key)
    }
}
