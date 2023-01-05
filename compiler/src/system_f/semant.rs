/*! Type checking for the System F AST. */

use std::collections::HashMap;

use super::ast::{Constant, Decl, Expr, Pattern, Prog, RawExpr, RawType, Type};
use crate::{system_f::ast::Binary, util::persistent::Persistent};

// /** Type-checks the expression `expr`. `ctxt` is a map from expression variables to types.
//  * Returns: The raw type of the checked `expr` */
// fn check<'a>(expr: &RawExpr, ctxt: &mut Persistent<RawType>) -> RawType {
//     use RawExpr::*;
//     ctxt.enter();
//     let typ = match expr {
//         Con { val } => match val {
//             Constant::Integer(_) => RawType::Int,
//             Constant::Boolean(_) => RawType::Bool,
//             Constant::Null => RawType::Unit,
//         },
//         Var { id } => ctxt.get(&id).clone(),
//         Let { pat, exp, body } => {
//             ctxt.bind(&annot.var, &annot.typ);
//             let typ = check(&exp.expr, ctxt);
//             // TODO change assert to error
//             assert_eq!(typ, annot.typ.typ);
//             typ
//         }
//         EApp { exp, arg } => {
//             let exp_t = check(&exp.expr, ctxt);
//             let arg_t = check(&arg.expr, ctxt);
//             match exp_t {
//                 RawType::Arrow(t1, t2) => {
//                     if t1.typ == arg_t {
//                         t2.as_ref().clone().typ
//                     } else {
//                         panic!("Oh no")
//                     }
//                 }
//                 _ => panic!("Expected function type"),
//             }
//         }
//         TApp { exp, arg } => {
//             let exp_t = check(&exp.expr, ctxt);
//             match exp_t {
//                 RawType::Forall(tvar, mut typ) => {
//                     substitute(&tvar, &arg, typ.as_mut());
//                     typ.typ
//                 }
//                 _ => panic!("Hmm"),
//             }
//         }
//         Tuple { entries } => {
//             RawType::Prod(entries.iter().map(|e| Type::new(check(e, ctxt))).collect())
//         }
//         Binop { lhs, op, rhs } => {
//             use Binary::*;
//             let lt = check(&lhs.expr, ctxt);
//             let rt = check(&rhs.expr, ctxt);
//             match (lt, rt) {
//                 (RawType::Int, RawType::Int) => match op {
//                     Add | Sub | Mul => RawType::Int,
//                     Eq | Ne | Gt | Lt => RawType::Bool,
//                     _ => panic!("Expecting bool args for AND/OR"),
//                 },
//                 (RawType::Bool, RawType::Bool) => match op {
//                     And | Or => RawType::Bool,
//                     _ => panic!("Expecting int args for binop, got bool"),
//                 },
//                 _ => panic!("Wrong binop types"),
//             }
//         }
//         Lambda { args, body } => {
//             for (var, typ) in args {
//                 ctxt.bind(&var, &typ)
//             }
//             check(body, ctxt)
//         }
//         Any { body, .. } => check(body, ctxt),
//         If { cond, t, f } => {
//             let typ_cond = check(cond, ctxt);
//             let typ_t = check(t, ctxt);
//             let typ_f = check(f, ctxt);
//             assert_eq!(typ_cond, RawType::Bool);
//             assert_eq!(typ_t, typ_f);
//             typ_t
//         }
//     };
//     ctxt.exeunt();
//     typ
// }

// pub fn check_expr(expr: &mut Expr) {
//     let mut ctxt = Persistent::<RawType>::new();
//     let typ = check(&expr.expr, &mut ctxt);
// }

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
        TVar(var) if var == tvar => *typ = target.clone(),
        Prod(typs) => {
            for t in typs {
                substitute(tvar, target, t)
            }
        }
        Arrow(v, t) => {
            substitute(tvar, target, v);
            substitute(tvar, target, t);
        }
        Forall(v, t) if v == tvar => substitute(tvar, target, t),
        _ => {}
    }
}

impl RawType {
    /** Alpha equivalence of types */
    pub fn alpha_equiv(typ1: &RawType, typ2: &RawType) -> bool {
        // De Brujin indices mappings for bound type variables.
        #[derive(Clone, Default)]
        struct Env {
            de_brujin_1: HashMap<String, u16>,
            de_brujin_2: HashMap<String, u16>,
            current_1: u16,
            current_2: u16,
        }

        // Helper function for alpha_equiv
        fn alpha_equiv_help(typ1: &RawType, typ2: &RawType, ctxt: &mut Persistent<Env>) -> bool {
            use RawType::*;
            match (typ1, typ2) {
                (Int, Int) | (Bool, Bool) | (Unit, Unit) => true,
                (TVar(v1), TVar(v2)) => {
                    let curr = ctxt.current();
                    let m1 = &curr.de_brujin_1;
                    let m2 = &curr.de_brujin_2;
                    let cont1 = m1.contains_key(v1);
                    let cont2 = m2.contains_key(v2);
                    if cont1 && cont2 {
                        // Bound type variables must have same indices
                        m1[v1] == m2[v2]
                    } else if !(cont1 || cont2) {
                        // Free type variables must be literally equal
                        v1 == v2
                    } else {
                        false
                    }
                }
                (Prod(ts1), Prod(ts2)) => {
                    ts1.iter().zip(ts2.iter()).all(|(t1, t2): (&Type, &Type)| {
                        ctxt.enter();
                        let alpha_eq = alpha_equiv_help(&t1.typ, &t2.typ, ctxt);
                        ctxt.exeunt();
                        alpha_eq
                    })
                }
                (Arrow(a1, b1), Arrow(a2, b2)) => {
                    ctxt.enter();
                    let alpha_eq_a = alpha_equiv_help(a1, a2, ctxt);
                    ctxt.exeunt();
                    ctxt.enter();
                    let alpha_eq_b = alpha_equiv_help(b1, b2, ctxt);
                    ctxt.exeunt();
                    alpha_eq_a && alpha_eq_b
                }
                (Forall(tv1, b1), Forall(tv2, b2)) => {
                    let context = ctxt.current();
                    context
                        .de_brujin_1
                        .insert(tv1.to_owned(), context.current_1);
                    context
                        .de_brujin_2
                        .insert(tv2.to_owned(), context.current_2);
                    context.current_1 += 1;
                    context.current_2 += 1;
                    alpha_equiv_help(b1, b2, ctxt)
                }
                _ => false,
            }
        }
        let env = Env::default();
        alpha_equiv_help(typ1, typ2, &mut Persistent::new(env))
    }
}
