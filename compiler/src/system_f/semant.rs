/*! Typee checking for the System F AST.
Everything is straightforward, but do note that
we use capture-avoiding substitution for type variables. */

use crate::{util::context::Context, system_f::ast::Binary};
use super::ast::{RawType, Type, RawExpr, Expr, Decl, Prog, Pattern, Constant};


/** Typee-Checks the expression `expr`. `ctxt` is a map
from expression variables to types. */
fn check<'a>(expr: &RawExpr, ctxt: &mut Context<RawType>) -> RawType {
    use RawExpr::*;
    ctxt.enter();
    let typ = match expr {
        Con { val } => match val {
            Constant::Integer(_) => RawType::Int,
            Constant::Boolean(_) => RawType::Bool,
            Constant::Null => RawType::Unit
        },
        Var { id } => ctxt.get(&id).clone(),
        Let { annot, exp, body } => {
            ctxt.bind(&annot.var, &annot.typ);
            let typ = check(&exp.expr, ctxt);
            // TODO change assert to error
            assert_eq!(typ, annot.typ.typ);
	    typ
        },
        EApp { exp, arg } => {
            let exp_t = check(&exp.expr, ctxt);
            let arg_t = check(&arg.expr, ctxt);
            match exp_t {
                RawType::Arrow(t1, t2) => {
                    if t1.typ == arg_t { t2.as_ref().clone().typ }
                    else { panic!("Oh no") }
                },
                _ => panic!("Expected function type")
            }
        },
        TApp { exp, arg } => {
            let exp_t = check(&exp.expr, ctxt);
            match exp_t {
                RawType::Forall(tvar, mut typ) => {
                    substitute(&tvar, &arg, typ.as_mut());
                    typ.typ
                },
                _ => panic!("Hmm")
            }
        },
        Tuple { entries } =>
            RawType::Prod(entries.iter().map(|e| Type::new(check(e, ctxt))).collect()),
        Match { exp, clause } => {
            let typ = check(exp, ctxt);
            /// Updates the context based on patterns
            fn fit_pats(pat: &Pattern, typ: &RawType, ctxt: &mut Context<RawType>) {
                match (pat, typ) {
                    (Pattern::Var(id), _) => ctxt.bind(id, typ),
                    (Pattern::Tuple(pats), RawType::Prod(typs)) => {
                        assert_eq!(pats.len(), typs.len());
                        for (p, t) in pats.iter().zip(typs.iter()) {
                            fit_pats(p, t, ctxt)
                        }
                    },
                    _ => panic!("Pattern checking failed")
                }
            }
            fit_pats(&clause.0, &typ, ctxt);
            check(&clause.1, ctxt)
        },
        Binop { lhs, op, rhs } => {
            use Binary::*;
            let lt = check(&lhs.expr, ctxt);
            let rt = check(&rhs.expr, ctxt);
            match (lt, rt) {
                (RawType::Int, RawType::Int) => match op {
                    Add | Sub | Mul => RawType::Int,
                    Eq | Ne | Gt | Lt => RawType::Bool,
                    _ => panic!("Expecting bool args for AND/OR")
                },
                (RawType::Bool, RawType::Bool) => match op {
                    And | Or => RawType::Bool,
                    _ => panic!("Expecting int args for binop, got bool")
                },
                _ => panic!("Wrong binop types")
            }
        },
        Lambda { args, body } => {
            for a in args {
                ctxt.bind(&a.var, &a.typ)
            }
            check(body, ctxt)
        },
        Any { body, .. } => check(body, ctxt),
        If { cond, t, f } => {
            let typ_cond = check(cond, ctxt);
            let typ_t = check(t, ctxt);
            let typ_f = check(f, ctxt);
            assert_eq!(typ_cond, RawType::Bool);
            assert_eq!(typ_t, typ_f);
            typ_t
        }
    };
    ctxt.exeunt();
    typ
}

pub fn check_expr(expr: &mut Expr) {
    let mut ctxt = Context::<RawType>::new();
    let typ = check(&expr.expr, &mut ctxt);
}

// pub fn check_decl(decl: Decl) {

// }

// pub fn check_prog(prog: Prog) {

// }

/** Capture-avoiding substitution
`tvar`: The type variable to replace
`target`: The type to replace with
`typ`: The type in which to perform the replacement
*/
fn substitute(tvar: &String, target: &RawType, typ: &mut RawType) {
    use RawType::*;
    match typ {
        TVar(var) if var == tvar => { *typ = target.clone() },
        Prod(typs) =>
            for t in typs {
                substitute(tvar, target, t)
            },
        Arrow(v, t) => {
            substitute(tvar, target, v);
            substitute(tvar, target, t);
        },
        Forall(v, t) if v != tvar =>
            substitute(tvar, target, t),
        _ => {}
    }
}