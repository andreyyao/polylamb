use crate::system_f::ast::{
    Binary, Constant, Decl, Expr, Ident, Pattern, Prog, RawExpr, RawPattern, RawType, Span, Type,
};
use crate::util::persistent::Persist;
/** Interpreting for the System F AST */
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use anyhow::{anyhow, Result};

use super::semant::Context;

const TYPE_ERR_MSG: &str =
    "Type mismatch during interpretation. This shouldn't happen. Did you typecheck?";

/// A struct representing the "store" sigma, mapping from variable names to values and types
#[derive(Clone)]
pub struct Environment {
    store: HashMap<String, (RawExpr, RawType)>,
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, (v, t)) in &self.store {
            write!(f, "{k} : {t} := {v}\n")?
        }
        write!(f, "")
    }
}

impl Environment {
    fn get(&self, key: &str) -> Result<(&RawExpr, &RawType)> {
        if let Some((v, t)) = self.store.get(key) {
            Ok((&v, &t))
        } else {
            Err(anyhow!(format!("Undefined variable {key}.")))
        }
    }

    fn new() -> Self {
        Environment {
            store: HashMap::new(),
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
                .store
                .insert(id.to_string(), (value, typ.typ.clone()));
            Ok(())
        }
        (RawExpr::Tuple { entries }, RawPattern::Tuple(patterns)) => {
            // Since we type check beforehand, this two vectors must have the same length
            for (e, p) in entries.iter().zip(patterns) {
                bind_pat(&e, &p, env)?
            }
            Ok(())
        }
        _ => panic!("{}", TYPE_ERR_MSG),
    }
}

/// The evaluation function that returns the value of
pub fn eval(env: &mut Persist<Environment>, expr: &RawExpr) -> Result<RawExpr> {
    use RawExpr::*;
    match &expr {
        // Constants being constants
        Con { val: _ } => Ok(expr.clone()),
        // Yeah, straight like dat
        Var { id } => env.current().get(id).map(|pair| pair.0.clone()),
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
            match func {
                Lambda {
                    args,
                    body,
                    ret_typ,
                } => {
                    todo!()
                }
                _ => Err(anyhow!(TYPE_ERR_MSG)),
            }
        }
        TApp { exp, arg } => todo!(),
        Tuple { entries } => {
	    let neu = Iterator::collect::<Result<Vec<_>>>(entries.iter().map(|e| { eval(env, e).map(|re| Expr::new(re)) }))?;
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
                                val: Integer(l + r)
                            },
                            Sub => Con {
				val: Integer(l - r)
			    },
                            Mul => Con {
				val: Integer(l * r)
			    },
                            Eq => Con {
				val: Boolean(l == r)
			    },
                            Lt => Con {
				val: Boolean(l < r)
			    },
                            Gt => Con {
				val: Boolean(l > r)
			    },
                            Ne => Con {
				val: Boolean(l != r)
			    },
			    // Unreachable
			    _ => panic!()
                        };
                        Ok(res)
                    } else {
                        Err(anyhow!(TYPE_ERR_MSG))
                    }
                },
		_ => {
		    let lhs_nf = eval(env, lhs)?;
                    let rhs_nf = eval(env, rhs)?;
                    if let (Con { val: Boolean(l) }, Con { val: Boolean(r) }) = (lhs_nf, rhs_nf) {
                        let res = match op {
			    And => Con {
				val: Boolean(l & r)
			    },
			    Or => Con {
				val: Boolean(l | r)
			    },
			    _ => panic!()
			};
			Ok(res)
		    } else {
			Err(anyhow!(TYPE_ERR_MSG))
		    }
		}
            }
        },
	// For a reduced term, nothing happens
        Lambda { .. }| Any { .. } => Ok(expr.clone()),
        If {
            cond,
            branch_t,
            branch_f,
        } => {
            // If terminates, it should normalize to boolean constant
            if let Ok(Con {
                val: Constant::Boolean(b),
            }) = eval(env, cond)
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

/// Performs capture-avoiding substitution of `val` for `var` inside `exp`
fn substitute_expr(exp: RawExpr, var: &str, val: &RawExpr) -> RawExpr {
    match exp {
        RawExpr::Con { val } => todo!(),
        RawExpr::Var { id } => todo!(),
        RawExpr::Let { pat, exp, body } => todo!(),
        RawExpr::EApp { exp, arg } => todo!(),
        RawExpr::TApp { exp, arg } => todo!(),
        RawExpr::Tuple { entries } => todo!(),
        RawExpr::Binop { lhs, op, rhs } => todo!(),
        RawExpr::Lambda {
            args,
            body,
            ret_typ,
        } => todo!(),
        RawExpr::Any { arg, body } => todo!(),
        RawExpr::If {
            cond,
            branch_t,
            branch_f,
        } => todo!(),
    }
}

fn substitute_type(exp: RawExpr, var: &str, typ: &RawType) -> RawExpr {
    todo!()
}
