use crate::ast::ast::{Binary, Constant, Expr, RawExpr, RawPattern, Decl, Prog, RawType};
use crate::ast::error::TypeError;
use crate::ast::semant::{check_decl, check_expr, Context, substitute};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use im::{hashmap::HashMap, hashset::HashSet};

pub type Environment = HashMap<String, Value>;

#[derive(Clone, Debug)]
pub enum Value {
    VConst(Constant),
    VTuple(Vec<Value>),
    VClosure(RawExpr, Rc<RefCell<Environment>>),
    VAny(RawExpr, Rc<RefCell<Environment>>),
}

/** Evaluates `expr` under `env` */
pub fn eval_expr(
    expr: &Expr,
    context: &Context,
    environment: &Environment,
) -> Result<Value, TypeError> {
    let mut ctxt = context.clone();
    check_expr(&expr, &ctxt, &HashSet::default())?;
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

pub fn eval_closed_expr(expr: &Expr) -> Value {
    eval(&Environment::default(), expr)
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

/** The evaluation function that returns the value of `expr` under the `env`, while potentially updating `env` with new bindings. */
fn eval(env: &Environment, expr: &RawExpr) -> Value {
    use RawExpr::*;
    use Value::*;
    // println!("Evaluating {} in {:?}", expr, env.keys());
    match expr {
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
            if let VAny(
                Any {
                    arg: tvar,
                    mut body,
                },
                env2,
            ) = eval(env, exp)
            {
                // subst(&mut body, tvar.name.as_str(), arg);
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

// fn filter_env(expr: &RawExpr, env: &Environment) -> Environment {
//     let fv = fv(expr);
//     env.clone()
//         .into_iter()
//         .filter(|(k, _)| fv.contains(k.as_str()))
//         .collect()
// }

/// Pattern matches `pat` recursively and binds to `exp`
fn bind_pat(clo: &Value, pat: &RawPattern, env: &mut Environment) {
    match (clo, pat) {
        (Value::VTuple(entries), RawPattern::Tuple(patterns)) => {
            // Since we type check beforehand, these two vectors must have the same length
            for (e, p) in entries.iter().zip(patterns) {
                bind_pat(e, p, env)
            }
        }
        (_, RawPattern::Wildcard) => (),
        (_, RawPattern::Binding(id)) => {
            env.insert(id.name.clone(), clo.clone());
        }
        _ => panic!("{}", TYPE_ERR_MSG),
    }
}

impl RawPattern {
    /// Whether `self` contains the variable `var`
    fn bindings<'ast>(&'ast self) -> Vec<&'ast str> {
        match self {
            RawPattern::Wildcard => vec![],
            RawPattern::Binding(v) => vec![v.name.as_str()],
            RawPattern::Tuple(pats) => pats
                .iter()
                .fold(vec![], |acc, p| [p.bindings(), acc].concat()),
        }
    }
}

// /// Returns a set of free variables
// fn fv<'ast>(expression: &'ast RawExpr) -> HashSet<&'ast str> {
//     use RawExpr::*;
//     match expression {
//         Con { .. } => HashSet::new(),
//         Var { id } => HashSet::from([id.as_str()]),
//         Let { pat, exp, body } => fv(exp)
//             .union(&(&fv(body) - &pat.bindings().into_iter().collect()))
//             .copied()
//             .collect(),
//         Fix { funcs, body } => {
//             let mut set = HashSet::new();
//             funcs.iter().for_each(|(_, v, _, _, bod)| {
//                 let mut fun_set = fv(&bod);
//                 fun_set.remove(v.name.as_str());
//                 set.extend(fv(&bod));
//             });
//             set.extend(fv(&body));
//             funcs.iter().for_each(|(f, _, _, _, _)| {
//                 set.remove(f.name.as_str());
//             });
//             set
//         }
//         EApp { exp, arg } => fv(exp).union(&fv(arg)).copied().collect(),
//         TApp { exp, .. } => fv(exp),
//         Tuple { entries } => {
//             let mut set = HashSet::new();
//             entries.iter().for_each(|e| set.extend(fv(e)));
//             set
//         }
//         Binop { lhs, op: _, rhs } => fv(lhs).union(&fv(rhs)).copied().collect(),
//         Lambda { arg, body } => {
//             let mut set = fv(body);
//             set.remove(arg.0.name.as_str());
//             set
//         }
//         Any { arg: _, body } => fv(body),
//         If {
//             cond,
//             branch_t,
//             branch_f,
//         } => fv(cond)
//             .union(&fv(branch_t))
//             .copied()
//             .collect::<HashSet<_, _>>()
//             .union(&fv(branch_f))
//             .copied()
//             .collect(),
//     }
// }

// /// Replaces all occurrences of `tvar` in `exp` with `typ`
// fn subst(e: &mut RawExpr, tvar: &str, target: &RawType) {
//     use RawExpr::*;
//     fn subst_pat(pat: &mut RawPattern, tvar: &str, target: &RawType) {
//         match pat {
//             RawPattern::Wildcard => (),
//             RawPattern::Binding(v) => substitute(tvar, target, t),
//             RawPattern::Tuple(pats) => pats.iter_mut().for_each(|p| subst_pat(p, tvar, target)),
//         }
//     }

//     match e {
//         Con { .. } => (),
//         Var { .. } => (),
//         Let { pat, exp, body } => {
//             subst_pat(pat, tvar, target);
//             subst(exp, tvar, target);
//             subst(body, tvar, target)
//         }
//         Fix { funcs, body } => {
//             for (_, _, t, ret, bod) in funcs {
//                 substitute(tvar, target, t);
//                 substitute(tvar, target, ret);
//                 subst(bod, tvar, target)
//             }
//             subst(body, tvar, target)
//         }
//         EApp { exp, arg } => {
//             subst(exp, tvar, target);
//             subst(arg, tvar, target)
//         }
//         TApp { exp, arg } => {
//             subst(exp, tvar, target);
//             substitute(tvar, target, arg)
//         }
//         Tuple { entries } => entries.iter_mut().for_each(|en| subst(en, tvar, target)),
//         Binop { lhs, op: _, rhs } => {
//             subst(lhs, tvar, target);
//             subst(rhs, tvar, target)
//         }
//         Lambda { arg: (_, t), body } => {
//             substitute(tvar, target, &mut t.typ);
//             subst(body, tvar, target)
//         }
//         Any { arg, body } => {
//             // Only sub in this case
//             if arg.name != tvar {
//                 subst(body, tvar, target)
//             }
//         }
//         If {
//             cond,
//             branch_t,
//             branch_f,
//         } => {
//             subst(cond, tvar, target);
//             subst(branch_t, tvar, target);
//             subst(branch_f, tvar, target)
//         }
//     }
// }

const TYPE_ERR_MSG: &str =
    "Type mismatch during interpretation. This shouldn't happen. Did you typecheck?";
