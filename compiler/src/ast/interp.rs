use crate::ast::ast::{Binary, Constant, Expr, RawExpr, RawPattern};
use crate::ast::semant::substitute;
use crate::util::persistent::Snapshot;
use std::cell::RefCell;
/** Interpreting for the System F AST */
use std::collections::{BTreeMap, HashSet};
use std::fmt::Display;
use std::rc::Rc;

use super::ast::{Decl, Prog, RawType};
use super::error::TypeError;
use super::semant::{check_decl, check_expr, Context};

/** Evaluates `expr` under `env` */
pub fn eval_expr(
    expr: &Expr,
    context: &Context,
    environment: &Environment,
) -> Result<Value, TypeError> {
    let mut ctxt = Snapshot::new(context.clone());
    check_expr(&expr, &mut ctxt, &mut Snapshot::default())?;
    Ok(eval(environment, expr))
}

/** Evaluates `decl` under current `environment` */
pub fn eval_decl(
    decl: &Decl,
    context: &mut Context,
    environment: &mut Environment,
) -> Result<(), TypeError> {
    check_decl(&decl, context)?;
    environment.insert(decl.id.clone(), eval(environment, &decl.body));
    Ok(())
}

/** Evaluates program */
pub fn eval_prog(prog: &Prog) -> Result<(), TypeError> {
    let mut env = Environment::default();
    let mut ctxt = Context::default();
    for id in &prog.order {
        let decl = &prog.declarations[id];
        eval_decl(decl, &mut ctxt, &mut env)?;
    }
    Ok(())
}

pub fn eval_closed_expr(expr: &Expr) -> RawExpr {
    let store = Default::default();
    eval(&store, expr).into()
}

impl Into<RawExpr> for Value {
    fn into(self) -> RawExpr {
        match self {
            Value::VConst(v) => RawExpr::Con { val: v },
            Value::VTuple(vs) => {
                let es = vs
                    .iter()
                    .map(|v| Expr::new(Into::<RawExpr>::into(v.clone())))
                    .collect();
                RawExpr::Tuple { entries: es }
            }
            Value::VClosure(v, _) => v,
            Value::VAny(v, _) => v,
        }
    }
}

pub type Environment = BTreeMap<String, Value>;

pub type TEnv = BTreeMap<String, RawType>;

#[derive(Clone, Debug)]
pub enum Value {
    VConst(Constant),
    VTuple(Vec<Value>),
    VClosure(RawExpr, Rc<RefCell<Environment>>),
    VAny(RawExpr, Rc<RefCell<Environment>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::VConst(c) => write!(f, "{}", c),
            Value::VTuple(vs) => {
                write!(f, "(")?;
                write!(f, "{}", vs[0])?;
                for val in vs.iter().skip(1) {
                    write!(f, ", {}", val)?
                }
                write!(f, ")")
            }
            Value::VClosure(e, c) | Value::VAny(e, c) => {
                write!(f, "{}", e)?;
                if !(**c).borrow().is_empty() {
                    write!(f, " <<with closure>>")
                    // write!(f, " where {{")?;
                    // for (k, v) in c {
                    // 	// Only need to print the free variables in closure environment
                    // 	if free_var(k, e) {
                    // 	    write!(f, "{} := {}; ", k, v)?
                    // 	}
                    // }
                    // write!(f, "}}")
                } else {
                    Ok(())
                }
            }
        }
    }
}

/** The evaluation function that returns the value of `expr` under the `store`, while potentially updating `store` with new bindings. */
fn eval(env: &Environment, expr: &RawExpr) -> Value {
    use RawExpr::*;
    use Value::*;
    // println!("Evaluating {} in {:?}", expr, env.keys());
    match &expr {
        // Constants being constants
        Con { val } => Value::VConst(val.clone()),
        // Yeah
        Var { id } => env[id].clone(),
        Let { pat, exp, body } => {
            let mut new_env = env.clone();
            let tup = eval(env, exp);
            bind_pat(&tup, pat, &mut new_env);
            eval(&new_env, body)
        }
        Fix { funcs, body } => {
            let new_env = Rc::new(RefCell::new(env.clone()));
            for (f, v, t, _, bod) in funcs {
                let lam = RawExpr::Lambda {
                    arg: (v.clone(), t.clone()),
                    body: Box::new(bod.clone()),
                };
                let closure = VClosure(lam, new_env.clone());
                new_env.borrow_mut().insert(f.name.clone(), closure);
            }
            let res = eval(&(*new_env).borrow().clone(), body);
            res
        }
        EApp { exp, arg } => match eval(env, exp) {
            Value::VClosure(Lambda { arg: (id, _), body }, e) => {
                let b = eval(env, arg);
                let mut map = (*e).borrow().clone();
                map.insert(id.name, b);
                eval(&map, &body.expr)
            }
            _ => panic!("\n{}\n{:?}\n", expr, env),
        },
        // TODO properly apply
        TApp { exp, arg } => {
            if let VAny(Any { arg: tvar, mut body }, env2) = eval(env, exp) {
		subst(&mut body, tvar.name.as_str(), arg);
                eval(&(*env2).borrow(), &body)
            } else {
                panic!("{}", TYPE_ERR_MSG)
            }
        }
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
                    } else {
                        panic!()
                    }
                }
            }
        }
        Lambda { .. } => VClosure(expr.clone(), Rc::new(RefCell::new(env.clone()))),
        Any { .. } => VAny(expr.clone(), Rc::new(RefCell::new(env.clone()))),
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
fn bind_pat(clo: &Value, pat: &RawPattern, env: &mut Environment) {
    match (clo, pat) {
        (Value::VTuple(entries), RawPattern::Tuple(patterns)) => {
            // Since we type check beforehand, these two vectors must have the same length
            for (e, p) in entries.iter().zip(patterns) {
                bind_pat(e, p, env)
            }
        }
        (_, RawPattern::Wildcard(_)) => (),
        (_, RawPattern::Binding(id, _)) => {
            env.insert(id.name.clone(), clo.clone());
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

impl RawPattern {
    /// Whether `self` contains the variable `var`
    fn _bindings<'ast>(&'ast self) -> Vec<&'ast str> {
        match self {
            RawPattern::Wildcard(_) => vec![],
            RawPattern::Binding(v, _) => vec![v.name.as_str()],
            RawPattern::Tuple(pats) => pats
                .iter()
                .fold(vec![], |acc, p| [p._bindings(), acc].concat()),
        }
    }
}

/// Returns a set of free variables
fn _fv<'ast>(expression: &'ast RawExpr) -> HashSet<&'ast str> {
    use RawExpr::*;
    match expression {
        Con { .. } => HashSet::new(),
        Var { id } => HashSet::from([id.as_str()]),
        Let { pat, exp, body } => _fv(exp)
            .union(&(&_fv(body) - &pat._bindings().into_iter().collect()))
            .copied()
            .collect(),
        Fix { funcs, body } => {
            let mut set = HashSet::new();
            funcs.iter().for_each(|(_, v, _, _, bod)| {
                let mut fun_set = _fv(&bod);
                fun_set.remove(v.name.as_str());
                set.extend(_fv(&bod));
            });
            set.extend(_fv(&body));
            funcs.iter().for_each(|(f, _, _, _, _)| {
                set.remove(f.name.as_str());
            });
            set
        }
        EApp { exp, arg } => _fv(exp).union(&_fv(arg)).copied().collect(),
        TApp { exp, .. } => _fv(exp),
        Tuple { entries } => {
            let mut set = HashSet::new();
            entries.iter().for_each(|e| set.extend(_fv(e)));
            set
        }
        Binop { lhs, op: _, rhs } => _fv(lhs).union(&_fv(rhs)).copied().collect(),
        Lambda { arg, body } => {
            let mut set = _fv(body);
            set.remove(arg.0.name.as_str());
            set
        }
        Any { arg: _, body } => _fv(body),
        If {
            cond,
            branch_t,
            branch_f,
        } => _fv(cond)
            .union(&_fv(branch_t))
            .copied()
            .collect::<HashSet<_, _>>()
            .union(&_fv(branch_f))
            .copied()
            .collect(),
    }
}

/// Replaces all occurrences of `tvar` in `exp` with `typ`
fn subst(e: &mut RawExpr, tvar: &str, target: &RawType) {
    use RawExpr::*;
    fn subst_pat(pat: &mut RawPattern, tvar: &str, target: &RawType) {
	match pat {
	    RawPattern::Wildcard(_) => (),
	    RawPattern::Binding(_, t) => substitute(tvar, target, t),
	    RawPattern::Tuple(pats) => pats.iter_mut().for_each(|p| subst_pat(p, tvar, target))
	}
    }

    match e {
        Con { .. } => (),
        Var { .. } => (),
        Let { pat, exp, body } => {
	    subst_pat(pat, tvar, target);
	    subst(exp, tvar, target);
	    subst(body, tvar, target)
	}
        Fix { funcs, body } => {
	    for (_, _, t, ret, bod) in funcs {
		substitute(tvar, target, t);
		substitute(tvar, target, ret);
		subst(bod, tvar, target)
	    }
	    subst(body, tvar, target)
	}
        EApp { exp, arg } => {
	    subst(exp, tvar, target);
	    subst(arg, tvar, target)
	}
        TApp { exp, arg } => {
	    subst(exp, tvar, target);
	    substitute(tvar, target, arg)
	}
        Tuple { entries } => {
	    entries.iter_mut().for_each(|en| subst(en, tvar, target))
	}
        Binop { lhs, op: _, rhs } => {
	    subst(lhs, tvar, target);
	    subst(rhs, tvar, target)
	}
        Lambda { arg: (_, t), body } => {
	    substitute(tvar, target, &mut t.typ);
	    subst(body, tvar, target)
	}
        Any { arg, body } => {
	    // Only sub in this case
	    if arg.name != tvar { subst(body, tvar, target) }
	}
        If { cond, branch_t, branch_f } => {
	    subst(cond, tvar, target);
	    subst(branch_t, tvar, target);
	    subst(branch_f, tvar, target)
	}
    }
}

const TYPE_ERR_MSG: &str =
    "Type mismatch during interpretation. This shouldn't happen. Did you typecheck?";
