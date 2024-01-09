/*! Type checking for the System F AST. */

use std::iter::zip;

use crate::ast::ast::{
    Binary, Constant, Decl, Expr, Ident, Pattern, Prog, RawExpr, RawPattern, RawType, Type,
};
use crate::ast::error::TypeError;
use annotate_snippets::snippet::{AnnotationType, SourceAnnotation};
use im::hashmap::HashMap;
use im::hashset::HashSet;

/** Mapping of variable names to types. Latest element is most recent */
pub type Context = HashMap<String, RawType>;

/** Type-checks the expression `expr`.
Returns: The raw type of the checked `expr`, or `TypeError`
# Arguments
 * `expr`: The expression to check
 * `val_ctxt`: Snapshots of mapping from variable names to raw type.
 * `typ_vars`: Set of declared type variables */
pub fn check_expr(
    expr: &Expr,
    val_ctxt: &Context,
    typ_vars: &HashSet<String>,
) -> Result<RawType, TypeError> {
    use RawExpr::*;
    use RawType::*;
    match &expr.expr {
        Con { val } => match val {
            Constant::Integer(_) => Ok(Int),
            Constant::Boolean(_) => Ok(Bool),
            Constant::Null => Ok(Unit),
        },
        Var { id } => match val_ctxt.get(id.as_str()) {
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
            let exp_typ = check_expr(exp, val_ctxt, typ_vars)?;
            let mut ctxt1 = val_ctxt.clone();
            traverse_pat(&pat, &mut HashSet::new(), &mut ctxt1, &exp_typ)?;
            check_expr(body, &ctxt1, typ_vars)
        }
        Fix { funcs, body } => {
            let mut ctxt1 = val_ctxt.clone();
            // Add the function signatures to context first
            for (fun, _, typ, ret, _) in funcs {
                let fun_typ = RawType::Arrow(Box::new(typ.clone()), Box::new(ret.clone()));
                ctxt1.insert(fun.name.clone(), fun_typ);
            }
            // Now type check each function definition
            for (_, var, typ, ret, def) in funcs {
                let mut ctxt2 = ctxt1.clone();
                ctxt2.insert(var.name.clone(), typ.typ.clone());
                let checked_typ = check_expr(def, &ctxt2, typ_vars)?;
                if !equivalent(&checked_typ, &ret) {
                    return Err(TypeError {
                        title: "Mismatched Types",
                        annot_type: AnnotationType::Error,
                        annotations: vec![SourceAnnotation {
                            range: body.span.unwrap(),
                            label: "fixpoint body's type doesn't match annotation",
                            annotation_type: AnnotationType::Error,
                        }],
                    });
                };
            }
            check_expr(body, &ctxt1, typ_vars)
        }
        EApp { exp, arg } => {
            let exp_t = check_expr(exp, val_ctxt, typ_vars)?;
            let arg_t = check_expr(arg, val_ctxt, typ_vars)?;
            match exp_t {
                RawType::Arrow(t1, t2) => {
                    if equivalent(&t1.typ, &arg_t) {
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
            let typs = entries
                .iter()
                .map(|e| Result::map(check_expr(e, val_ctxt, typ_vars), Type::new))
                .collect::<Result<Vec<Type>, TypeError>>()?;
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
            let typ_l = check_expr(lhs, val_ctxt, typ_vars)?;
            let typ_r = check_expr(rhs, val_ctxt, typ_vars)?;
            match op {
                Add | Sub | Mul | Eq | Ne | Gt | Lt => {
                    let err_msg = "expected to have type `Int`";
                    match (typ_l, typ_r) {
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
                    match (typ_l, typ_r) {
                        (Bool, Bool) => Ok(Bool),
                        (Bool, _) => Err(err(err_msg, rhs.span.unwrap())),
                        _ => Err(err(err_msg, lhs.span.unwrap())),
                    }
                }
            }
        }
        Lambda { arg, body } => {
            let mut ctxt1 = val_ctxt.clone();
            let (id, typ) = arg;
            let bound = ctxt1.insert(id.name.clone(), typ.typ.clone());
            if bound.is_some() {
                return Err(TypeError {
                    title: "Redefinition of variables",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: id.span.unwrap(),
                        label: "attempting to declare a bound variable",
                        annotation_type: AnnotationType::Error,
                    }],
                });
            }
            let body_typ = check_expr(body, &ctxt1, typ_vars)?;
            Ok(Arrow(Box::new(typ.clone()), Box::new(Type::new(body_typ))))
        }
        Any { arg, body } => {
            let mut tvars1 = typ_vars.clone();
            tvars1.insert(arg.name.clone());
            let typ = check_expr(body, val_ctxt, &tvars1)?;
            let poly_copy = Ident {
                name: arg.name.clone(),
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
            let c_typ = check_expr(cond, val_ctxt, typ_vars)?;
            let t_typ = check_expr(branch_t, val_ctxt, typ_vars)?;
            let f_typ = check_expr(branch_f, val_ctxt, typ_vars)?;
            match c_typ {
                Bool => {
                    if equivalent(&t_typ, &f_typ) {
                        Ok(t_typ)
                    } else {
                        Err(TypeError {
                            title: "Non uniform types in conditional branches",
                            annot_type: AnnotationType::Error,
                            annotations: vec![SourceAnnotation {
                                range: branch_f.span.unwrap(),
                                label: "true and false branches must have same types",
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

/** Type-checks the declaration `decl`. `val_ctxt` is a the context up to all the previous declarations.
If the checked type of the `decl` body matches the `decl` signature, then this adds the pair of (`decl` id, signature) to `val_ctxt`.
Returns: `Ok` if everything is fine, or `TypeError` otherwise.
# Arguments
 * `decl`: The declaration to check
 * `val_ctxt`: Persistent mapping from variable names to raw type */
pub fn check_decl<'ast>(decl: &'ast Decl, ctxt: &mut Context) -> Result<(), TypeError> {
    let val_ctxt = ctxt.clone();
    let typ_vars = HashSet::default();
    let check_result = check_expr(&decl.body, &val_ctxt, &typ_vars);
    match check_result {
        Ok(typ) => {
            if equivalent(&typ, &decl.sig.typ) {
                ctxt.insert(decl.id.clone(), typ);
                Ok(())
            } else {
                Err(TypeError {
                    title: "Mismatched type in declaration",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: decl.body.span.unwrap(),
                        label: "expression type differs from declaration signature",
                        annotation_type: AnnotationType::Error,
                    }],
                })
            }
        }
        Err(te) => Err(te),
    }
}

/** Type-checks the program `prog` starting from an empty typing context.
Returns: `Ok` if everything is fine, or `TypeError` otherwise.
# Arguments
 * `prog`: The prog to check */
pub fn check_prog<'ast>(prog: &'ast Prog) -> Result<(), TypeError> {
    for id in &prog.order {
        check_decl(&prog.declarations[id], &mut Default::default())?
    }
    Ok(())
}

// Check closed expression
pub fn check_closed_expr(expr: &Expr) -> Result<RawType, TypeError> {
    let ctxt = HashMap::default();
    let tvars = HashSet::default();
    check_expr(expr, &ctxt, &tvars)
}

/** Substitution
`tvar`: The type variable to replace
`target`: The type to replace with
`typ`: The type in which to perform the replacement */
pub fn substitute(tvar: &str, target: &RawType, typ: &mut RawType) {
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
        Forall(v, t) if &v.name != tvar => substitute(tvar, target, t),
        _ => {}
    }
}

/** Equivalence of types. No alpha equivalence to make life easier. */
pub fn equivalent<'src>(typ1: &'src RawType, typ2: &'src RawType) -> bool {
    use RawType::*;
    match (typ1, typ2) {
        (Int, Int) | (Bool, Bool) | (Unit, Unit) => true,
        (TVar(v1), TVar(v2)) => v1 == v2,
        (Prod(ts1), Prod(ts2)) => ts1
            .iter()
            .zip(ts2.iter())
            .all(|(t1, t2)| equivalent(t1, t2)),
        (Arrow(a1, b1), Arrow(a2, b2)) => equivalent(a1, a2) && equivalent(b1, b2),
        (Forall(tv1, b1), Forall(tv2, b2)) => tv1.name == tv2.name && equivalent(b1, b2),
        _ => false,
    }
}

/** Returns `Ok(())` and inserts relevant bindings to `ctxt` when no duplicates,
or `Err(te)` where te is the type error, when yes duplicates.
Adds binding to `ctxt` along the way
# Arguments
 * `pat`: The pattern to check
 * `vars`: Set of vars already seen in `pat`
 * `ctxt`: The typing Context
 * `typ` : The type to be destructed */
fn traverse_pat(
    pat: &Pattern,
    vars: &mut HashSet<String>,
    ctxt: &mut Context,
    typ: &RawType,
) -> Result<(), TypeError> {
    match &pat.pat {
        RawPattern::Binding(ident) => {
            let seen = vars.insert(ident.name.clone());
            if seen.is_some() {
                Err(TypeError {
                    title: "Conflicting argument names",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: pat.span.unwrap(),
                        label: "variable bound multiple times in pattern",
                        annotation_type: AnnotationType::Error,
                    }],
                })
            } else {
                ctxt.insert(ident.name.clone(), typ.clone());
                Ok(())
            }
        }
        RawPattern::Wildcard => Ok(()),
        RawPattern::Tuple(pats) =>
            match typ {
                RawType::Prod(ts) if pats.len() == ts.len() => {
                    zip(pats.iter(), ts.iter())
                        .map(|(p, t)| traverse_pat(p, vars, ctxt, t))
                        .collect::<Result<Vec<()>, TypeError>>()?;
                    Ok(())
                }
                _ => Err(TypeError {
                    title: "Malformed pattern assignment",
                    annot_type: AnnotationType::Error,
                    annotations: vec![SourceAnnotation {
                        range: pat.span.unwrap(),
                        label: "pattern expected with same number of entries as product type",
                        annotation_type: AnnotationType::Error,
                    }],
                })
            }
    }
}
