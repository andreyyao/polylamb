/*! Type checking for the System F AST. */

use std::collections::{HashMap, HashSet};

use crate::system_f::ast::{
    Binary, Constant, Decl, Expr, Ident, Pattern, Prog, RawExpr, RawPattern, RawType, Span, Type,
};
use crate::system_f::error::TypeError;
use crate::util::persistent::{adventure, Persist};
use annotate_snippets::snippet::{AnnotationType, SourceAnnotation};

/** Mapping of variable names to list of (type, binding site). Latest element is most recent */
pub type Context<'a> = Persist<HashMap<&'a String, (RawType, Span)>>;

/** Type-checks the expression `expr`. `ctxt` is a map from expression variables to types.
Returns: The raw type of the checked `expr`, or `TypeError`
# Arguments
 * `expr`: The expression to check
 * `val_ctxt`: Persistent mapping from variable names to (raw type, location of binding).
 Invariance: each value in map, if exists, has length at least 1
 * `typ_vars`: Set of declared type variables */
pub fn check_expr<'a>(
    expr: &'a Expr,
    val_ctxt: &mut Context<'a>,
    typ_vars: &mut Persist<HashSet<&'a String>>,
) -> Result<RawType, TypeError> {
    use RawExpr::*;
    use RawType::*;
    match &expr.expr {
        Con { val } => match val {
            Constant::Integer(_) => Ok(Int),
            Constant::Boolean(_) => Ok(Bool),
            Constant::Null => Ok(Unit),
        },
        Var { id } => match val_ctxt.current().get(id) {
            Some((typ, _)) => Ok(typ.clone()),
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
            val_ctxt.enter();
            let exp_typ = match &pat.pat {
                // Functions can refer to itself in body for recursion
                RawPattern::Binding(ident, Type { typ, .. }) => {
                    match &typ {
                        Arrow(_, _) => {
                            val_ctxt
                                .current()
                                .insert(&ident.name, (typ.clone(), pat.span.unwrap()));
                        }
                        _ => (),
                    };
                    check_expr(exp, val_ctxt, typ_vars)?
                }
                _ => check_expr(exp, val_ctxt, typ_vars)?,
            };
            val_ctxt.exeunt();
            let mut vars: HashSet<&'a String> = HashSet::new();
            let pat_typ = traverse_pat(pat, &mut vars, val_ctxt)?;
            if alpha_equiv(&exp_typ, &pat_typ) {
                check_expr(body, val_ctxt, typ_vars)
            } else {
                Err(TypeError {
                    title: "Mismatched types in pattern",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: pat.span.unwrap(),
                        label: "this pattern has wrong type",
                        annotation_type: AnnotationType::Error,
                    }],
                })
            }
        }
        EApp { exp, arg } => {
            let exp_t = check_expr(&exp, val_ctxt, typ_vars)?;
            let arg_t = check_expr(&arg, val_ctxt, typ_vars)?;
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
            let exp_t = check_expr(exp, val_ctxt, typ_vars)?;
            match exp_t {
                RawType::Forall(tvar, typ) => {
                    let mut t = typ.typ.clone();
                    substitute(&tvar.name, &arg, &mut t);
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
                adventure!(typ, Type::new(check_expr(e, val_ctxt, typ_vars)?), val_ctxt);
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
            adventure!(type_l, check_expr(lhs, val_ctxt, typ_vars)?, val_ctxt);
            adventure!(type_r, check_expr(rhs, val_ctxt, typ_vars)?, val_ctxt);
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
            // Current variable names
            let mut vars: HashSet<&'a String> = HashSet::new();
            let typs = args
                .iter()
                .map(|p| traverse_pat(p, &mut vars, val_ctxt))
                .collect::<Result<Vec<RawType>, TypeError>>()?;
            let base_typ = check_expr(body, val_ctxt, typ_vars)?;
            let typ = typs.iter().rev().fold(base_typ, |acc, ele| {
                Arrow(
                    Box::new(Type::new(ele.clone())),
                    Box::new(Type::new(acc.clone())),
                )
            });
            // Fold to get nested arrow types
            Ok(typ)
        }
        Any { poly, body } => {
            typ_vars.current().insert(&poly.name);
            let typ = check_expr(body, val_ctxt, typ_vars)?;
            let poly_copy = Ident {
                name: poly.name.clone(),
                span: None,
            };
            Ok(Forall(poly_copy, Box::new(Type::new(typ))))
        }
        If {
            cond,
            branch_t,
            branch_f,
        } => {
            // Check the three branches independently
            adventure!(c_typ, check_expr(cond, val_ctxt, typ_vars)?, val_ctxt);
            adventure!(t_typ, check_expr(branch_t, val_ctxt, typ_vars)?, val_ctxt);
            adventure!(f_typ, check_expr(branch_f, val_ctxt, typ_vars)?, val_ctxt);
            match c_typ {
                Bool => {
                    if alpha_equiv(&t_typ, &f_typ) {
                        Ok(t_typ)
                    } else {
                        Err(TypeError {
                            title: "Non uniform types in conditional branches",
                            annot_type: AnnotationType::Error,
                            annotations: vec![SourceAnnotation {
                                range: branch_f.span.unwrap(),
                                label:
                                    "this branch has different type from that of the true-branch",
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

pub fn check_decl(decl: Decl) {}

pub fn check_prog(prog: Prog) {}

// Check closed expression
pub fn check_closed_expr(expr: &Expr) -> Result<RawType, TypeError> {
    let mut ctxt = Persist::new(HashMap::new());
    let mut tvars = Persist::new(HashSet::new());
    check_expr(expr, &mut ctxt, &mut tvars)
}

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
        Forall(v, t) if &v.name == tvar => substitute(tvar, target, t),
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
                context.de_brujin_1.insert(&tv1.name, context.current_1);
                context.de_brujin_2.insert(&tv2.name, context.current_2);
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

/** Returns `Ok(t)`, where `t` is type for `pat`, when no duplicates,
or `Err(te)` where te is the type error, when yes duplicates.
Adds binding to `ctxt` along the way
# Arguments
 * `pat`: The pattern to check
 * `vars`: Set of vars already seen in `pat`
 * `ctxt`: The typing Context */
fn traverse_pat<'a>(
    pat: &'a Pattern,
    vars: &mut HashSet<&'a String>,
    ctxt: &mut Context<'a>,
) -> Result<RawType, TypeError> {
    match &pat.pat {
        RawPattern::Binding(ident, typ) => {
            let seen = !vars.insert(&ident.name);
            if seen {
                Err(TypeError {
                    title: "Conflicting argument names",
                    annot_type: AnnotationType::Error,
                    annotations: vec![
                        SourceAnnotation {
                            range: pat.span.unwrap(),
                            label: "variable bound multiple times in pattern",
                            annotation_type: AnnotationType::Error,
                        },
                        SourceAnnotation {
                            range: ctxt.current().get(&ident.name).unwrap().1,
                            label: "it was already defined here",
                            annotation_type: AnnotationType::Help,
                        },
                    ],
                })
            } else {
                ctxt.current()
                    .insert(&ident.name, (typ.typ.clone(), ident.span.unwrap().clone()));
                Ok(typ.typ.clone())
            }
        }
        RawPattern::Tuple(pats) => {
            let typs = pats
                .iter()
                .map(|p| traverse_pat(p, vars, ctxt))
                .collect::<Result<Vec<RawType>, TypeError>>()?;
            Ok(RawType::Prod(
                typs.iter().map(|t| Type::new(t.clone())).collect(),
            ))
        }
        RawPattern::Wildcard(typ) => Ok(typ.typ.clone()),
    }
}
