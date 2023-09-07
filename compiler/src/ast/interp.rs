use crate::ast::ast::{Binary, Constant, Expr, RawExpr, RawPattern};
use crate::util::persistent::Snapshot;
use std::cell::RefCell;
/** Interpreting for the System F AST */
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::rc::Rc;

use super::ast::{Decl, Prog};
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
    environment.insert(decl.id.clone(), Rc::new(RefCell::new(eval(environment, &decl.body))));
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
    let store = HashMap::default();
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

pub type Environment = HashMap<String, Rc<RefCell<Value>>>;

#[derive(Clone, Debug)]
pub enum Value {
    VConst(Constant),
    VTuple(Vec<Value>),
    VClosure(RawExpr, Environment),
    VAny(RawExpr, Environment),
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
                if !c.is_empty() {
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
        Var { id } => env[id].borrow().clone(),
        Let { pat, exp, body } => {
            let mut new_env = env.clone();
            let tup = eval(env, exp);
            bind_pat(&tup, pat, &mut new_env);
            eval(&new_env, body)
        }
        Fix { funcs, body } => {
            let mut new_env = env.clone();
            for (f, v, t, _, bod) in funcs {
                let lam = RawExpr::Lambda {
                    arg: (v.clone(), t.clone()),
                    body: Box::new(bod.clone()),
                };
                let closure = eval(&new_env, &lam);
                new_env.insert(f.name.clone(), Rc::new(RefCell::new(closure)));
            }
	    for (f, _, _, _, _) in funcs {
		let clo_f = new_env[f.name.as_str()].clone();
		extend(&clo_f, &new_env)
            }
            eval(&new_env, body)
        }
        EApp { exp, arg } => match eval(env, exp) {
            Value::VClosure(Lambda { arg: (id, _), body }, e) => {
                let b = eval(env, arg);
                let mut map = e.clone();
                map.insert(id.name, Rc::new(RefCell::new(b)));
                eval(&map, &body.expr)
            }
            _ => panic!("\n{}\n{:?}\n", expr, env),
        },
        // TODO properly apply
        TApp { exp, arg } => {
            if let VAny(Any { arg, body }, mut env2) = eval(env, exp) {
                eval(&mut env2, &body)
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
            env.insert(id.name.clone(), Rc::new(RefCell::new(clo.clone())));
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

/// Extends the closure value with the environment
fn extend(val: &Rc<RefCell<Value>>, env: &Environment) {
    let mut borrow_mut_val = RefCell::borrow_mut(val);
    match *borrow_mut_val {
        Value::VConst(_) | Value::VTuple(_) => (),
        Value::VClosure(_, ref mut en) | Value::VAny(_, ref mut en) => en.extend(env.clone()),
    }
}

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
