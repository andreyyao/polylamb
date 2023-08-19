use crate::system_f::ast::{Binary, Constant, Expr, RawExpr, RawPattern, RawType};
use crate::util::persistent::{Persist, adventure};
/** Interpreting for the System F AST */
use std::collections::HashMap;
use std::fmt::Display;

use anyhow::{anyhow, Result};

const TYPE_ERR_MSG: &str =
    "Type mismatch during interpretation. This shouldn't happen. Did you typecheck?";

/** A struct representing the "store"s.
`val_store` is mapping from variable names to its (value, type) pair
`typ_store` maps type variable names to types */
#[derive(Clone)]
pub struct Environment {
    val_store: HashMap<String, (RawExpr, RawType)>,
    typ_store: HashMap<String, RawType>,
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, (v, t)) in &self.val_store {
            write!(f, "{k} : {t} := {v}\n")?
        }
        write!(f, "")
    }
}

impl Environment {
    fn get_val(&self, key: &str) -> Result<(&RawExpr, &RawType)> {
        if let Some((v, t)) = self.val_store.get(key) {
            Ok((&v, &t))
        } else {
            Err(anyhow!(format!("Undefined variable {key}.")))
        }
    }

    fn new() -> Self {
        Environment {
            val_store: HashMap::new(),
            typ_store: HashMap::new(),
        }
    }
}

/// Pattern matches `pat` recursively and binds to `exp`
fn bind_pat(exp: &RawExpr, pat: &RawPattern, env: &mut Persist<Environment>) -> Result<()> {
    match (exp, pat) {
        (_, RawPattern::Wildcard(_)) => Ok(()),
        (_, RawPattern::Binding(id, typ)) => {
            let value = eval(env, exp)?;
            env.current()
                .val_store
                .insert(id.to_string(), (value, typ.typ.clone()));
            Ok(())
        }
        (RawExpr::Tuple { entries }, RawPattern::Tuple(patterns)) => {
            // Since we type check beforehand, these two vectors must have the same length
            for (e, p) in entries.iter().zip(patterns) {
                bind_pat(&e, &p, env)?
            }
            Ok(())
        }
        _ => panic!("{}", TYPE_ERR_MSG),
    }
}

/** Evaluates `expr` under an empty environment */
pub fn eval_expr(expr: &RawExpr) -> Result<RawExpr> {
    let mut env = Persist::new(Environment::new());
    eval(&mut env, expr)
}

/** The evaluation function that returns the value of `expr` under the environment `env`,
 while potentially updating `env` with new bindings. */
fn eval(env: &mut Persist<Environment>, expr: &RawExpr) -> Result<RawExpr> {
    use RawExpr::*;
    match &expr {
        // Constants being constants
        Con { val: _ } => Ok(expr.clone()),
        // Yeah, straight like dat
        Var { id } => env.current().get_val(id).map(|pair| pair.0.clone()),
        Let { pat, exp, body } => {
            env.enter();
            // Add the bindings produced by the pattern
            bind_pat(exp, pat, env)?;
            env.exeunt();
            // Eval body with new bindings added
            eval(env, body)
        }
        EApp { exp, arg } => {
            let func = eval(env, exp)?;
            let param = eval(env, arg)?;
            // lhs needs to be a value, which is a lambda expression by strength reduction
            match func {
                Lambda {
                    arg: (var, typ),
                    body,
                } => {
                    //Update the store
                    env.current().val_store.insert(var.name, (param, typ.typ));
                    eval(env, &body.expr)
                }
                _ => Err(anyhow!(TYPE_ERR_MSG)),
            }
        }
        TApp { exp, arg } => {
            if let Any { arg: t, body } = eval(env, exp)? {
                env.current().typ_store.insert(t.name, arg.typ.clone());
                eval(env, &body.expr)
            } else {
                Err(anyhow!(TYPE_ERR_MSG))
            }
        }
        Tuple { entries } => {
            let neu = Iterator::collect::<Result<Vec<_>>>(
                entries.iter().map(|e| eval(env, e).map(|re| Expr::new(re))),
            )?;
            Ok(Tuple { entries: neu })
        }
        Binop { lhs, op, rhs } => {
            use Binary::*;
            use Constant::*;
            match op {
                // Integer arguments
                Add | Sub | Mul | Eq | Lt | Gt | Ne => {
                    let lhs_nf = eval(env, lhs)?;
                    let rhs_nf = eval(env, rhs)?;
                    if let (Con { val: Integer(l) }, Con { val: Integer(r) }) = (lhs_nf, rhs_nf) {
                        let res = match op {
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
                        };
                        Ok(res)
                    } else {
                        Err(anyhow!(TYPE_ERR_MSG))
                    }
                }
                _ => {
                    let lhs_nf = eval(env, lhs)?;
                    let rhs_nf = eval(env, rhs)?;
                    if let (Con { val: Boolean(l) }, Con { val: Boolean(r) }) = (lhs_nf, rhs_nf) {
                        let res = match op {
                            And => Con {
                                val: Boolean(l & r),
                            },
                            Or => Con {
                                val: Boolean(l | r),
                            },
                            _ => panic!(),
                        };
                        Ok(res)
                    } else {
                        Err(anyhow!(TYPE_ERR_MSG))
                    }
                }
            }
        }
        // For a reduced term, nothing happens
        Lambda { .. } | Any { .. } => Ok(expr.clone()),
        If {
            cond,
            branch_t,
            branch_f,
        } => {
	    adventure!(cond_nf, eval(env, cond), env);
            // If terminates, it should normalize to boolean constant
            if let Ok(Con {
                val: Constant::Boolean(b),
            }) = cond_nf
            {
                if b {
                    eval(env, branch_t)
                } else {
                    eval(env, branch_f)
                }
            } else {
                Err(anyhow!(TYPE_ERR_MSG))
            }
        }
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
