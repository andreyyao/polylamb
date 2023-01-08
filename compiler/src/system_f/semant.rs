/*! Type checking for the System F AST. */

use std::collections::HashMap;

use crate::system_f::ast::{Binary, Constant, Decl, Expr, Pattern, Prog, RawExpr, RawType, Type};
use crate::system_f::error::TypeError;
use crate::util::persistent::{adventure, Persist};
use annotate_snippets::snippet::{AnnotationType, SourceAnnotation};

/** Type-checks the expression `expr`. `ctxt` is a map from expression variables to types.
 * Returns: The raw type of the checked `expr` */
pub fn check<'a>(
    expr: &'a Expr,
    val_ctxt: &mut Persist<HashMap<&'a String, RawType>>,
) -> Result<RawType, TypeError> {
    use RawExpr::*;
    use RawType::*;
    let raw_exp = &expr.expr;
    match raw_exp {
        Con { val } => match val {
            Constant::Integer(_) => Ok(Int),
            Constant::Boolean(_) => Ok(Bool),
            Constant::Null => Ok(Unit),
        },
        Var { id } => match val_ctxt.current().get(id) {
            Some(typ) => Ok(typ.clone()),
            None => Err(TypeError {
                title: "Unbound variable",
                annot_type: AnnotationType::Error,
                annotations: vec![SourceAnnotation {
                    range: expr.span.unwrap(),
                    label: "this variable hasn't been defined",
                    annotation_type: AnnotationType::Error,
                }],
            }),
        },
        Let { pat, exp, body } => {
            panic!("TODO")
        }
        EApp { exp, arg } => {
            let exp_t = check(&exp, val_ctxt)?;
            let arg_t = check(&arg, val_ctxt)?;
            match exp_t {
                RawType::Arrow(t1, t2) => {
                    if alpha_equiv(&t1.typ, &arg_t) {
                        Ok(t2.typ)
                    } else {
                        Err(TypeError {
                            title: "Mismatched Types",
                            annot_type: AnnotationType::Error,
                            annotations: vec![SourceAnnotation {
                                range: arg.span.unwrap(),
                                label: "function argument has wrong type",
                                annotation_type: AnnotationType::Error,
                            }],
                        })
                    }
                }
                _ => Err(TypeError {
                    title: "Illegal application",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: exp.span.unwrap(),
                        label: "cannot apply arguments to non-functions",
                        annotation_type: AnnotationType::Error,
                    }],
                }),
            }
        }
        TApp { exp, arg } => {
            let exp_t = check(exp, val_ctxt)?;
            match exp_t {
                RawType::Forall(tvar, typ) => {
                    let mut t = typ.typ.clone();
                    substitute(&tvar, &arg, &mut t);
                    Ok(t)
                }
                _ => Err(TypeError {
                    title: "Illegal type specialization",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: exp.span.unwrap(),
                        label: "this expression doesn't have `∀` type",
                        annotation_type: AnnotationType::Error,
                    }],
                }),
            }
        }
        Tuple { entries } => {
            let mut typs = vec![];
            for e in entries {
                adventure!(typ, Type::new(check(e, val_ctxt)?), val_ctxt);
                typs.push(typ)
            }
            Ok(RawType::Prod(typs))
        }
        Binop { lhs, op, rhs } => {
            use Binary::*;
            use RawType::*;
            fn err(msg: &'static str, span: (usize, usize)) -> TypeError {
                TypeError {
                    title: "Mismatched Types",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: span,
                        label: msg,
                        annotation_type: AnnotationType::Error,
                    }],
                }
            }
            adventure!(type_l, check(lhs, val_ctxt)?, val_ctxt);
            adventure!(type_r, check(rhs, val_ctxt)?, val_ctxt);
            match op {
                Add | Sub | Mul | Eq | Ne | Gt | Lt => {
                    let err_msg = "expected to have type `Int`";
                    match (type_l, type_r) {
                        (Int, Int) => Ok(match op {
                            Add | Sub | Mul => Int,
                            _ => Bool,
                        }),
                        (Int, _) => Err(err(err_msg, rhs.span.unwrap())),
                        _ => Err(err(err_msg, lhs.span.unwrap())),
                    }
                }
                And | Or => {
                    let err_msg = "expected to have type `Bool`";
                    match (type_l, type_r) {
                        (Bool, Bool) => Ok(Bool),
                        (Bool, _) => Err(err(err_msg, rhs.span.unwrap())),
                        _ => Err(err(err_msg, lhs.span.unwrap())),
                    }
                }
            }
        }
        Lambda { args, body } => {
            panic!("TODO")
        }
        Any { body, .. } => {
            panic!("TODO")
        }
        If {
            cond,
            branch_t,
            branch_f,
        } => {
            let cond_typ = check(cond, val_ctxt)?;
            let t_typ = check(branch_t, val_ctxt)?;
            let f_typ = check(branch_f, val_ctxt)?;
            match cond_typ {
                Bool => {
                    if alpha_equiv(&t_typ, &f_typ) {
                        Ok(t_typ)
                    } else {
                        Err(TypeError {
                            title: "Non uniform types in conditional branches",
                            annot_type: AnnotationType::Error,
                            annotations: vec![SourceAnnotation {
                                range: branch_f.span.unwrap(),
                                label: "this branch has different type from that of the true-branch",
                                annotation_type: AnnotationType::Error,
                            }],
                        })
                    }
                }
                _ => Err(TypeError {
                    title: "Mismatched type for if condition",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: cond.span.unwrap(),
                        label: "if condition expected to have type `Bool`",
                        annotation_type: AnnotationType::Error,
                    }],
                }),
            }
        }
    }
}

// pub fn check_expr(expr: &mut Expr) {
//     let mut ctxt = Persistent::<RawType>::new();
//     let typ = check(&expr.expr, &mut ctxt);
// }

// Check closed expression
pub fn check_closed_expr(expr: &Expr) -> Result<RawType, TypeError> {
    let mut ctxt = Persist::new(HashMap::new());
    check(expr, &mut ctxt)
}

pub fn check_decl(decl: Decl) {}

pub fn check_prog(prog: Prog) {}

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

/** Alpha equivalence of types */
pub fn alpha_equiv(typ1: &RawType, typ2: &RawType) -> bool {
    // De Brujin indices mappings for bound type variables.
    #[derive(Clone, Default)]
    struct Env<'src> {
        de_brujin_1: HashMap<&'src String, u16>,
        de_brujin_2: HashMap<&'src String, u16>,
        current_1: u16,
        current_2: u16,
    }

    // Helper function for alpha_equiv
    fn alpha_equiv_help<'src>(
        typ1: &'src RawType,
        typ2: &'src RawType,
        ctxt: &mut Persist<Env<'src>>,
    ) -> bool {
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
            (Prod(ts1), Prod(ts2)) => ts1.iter().zip(ts2.iter()).all(|(t1, t2): (&Type, &Type)| {
                adventure!(alpha_eq, alpha_equiv_help(&t1.typ, &t2.typ, ctxt), ctxt);
                alpha_eq
            }),
            (Arrow(a1, b1), Arrow(a2, b2)) => {
                // Compare alpha equality of two types
                adventure!(eq_a, alpha_equiv_help(a1, a2, ctxt), ctxt);
                adventure!(eq_b, alpha_equiv_help(b1, b2, ctxt), ctxt);
                eq_a && eq_b
            }
            (Forall(tv1, b1), Forall(tv2, b2)) => {
                let context = ctxt.current();
                context.de_brujin_1.insert(tv1, context.current_1);
                context.de_brujin_2.insert(tv2, context.current_2);
                context.current_1 += 1;
                context.current_2 += 1;
                alpha_equiv_help(b1, b2, ctxt)
            }
            _ => false,
        }
    }
    let env = Env::default();
    alpha_equiv_help(typ1, typ2, &mut Persist::new(env))
}
