/*! Type checking for the System F AST.
Everything is straightforward, but do note that
we use capture-avoiding substitution for type variables. */

use crate::util::context::Context;
use super::ast::{RawExpr, Typ, Decl, Prog, Constant};


pub fn check(expr: &RawExpr, ctxt: &mut Context<Typ>) -> Typ {
    use RawExpr::*;
    match expr {
        Con { val } => match val {
            Constant::Integer(_) => Typ::Int,
            Constant::Boolean(_) => Typ::Bool,
            Constant::Null => Typ::Unit
        },
        Var { id } => ctxt.get(&id).clone(),
        Let { annot, exp, body } => {
            ctxt.enter();
            ctxt.bind(&annot.var, &annot.typ);
            let typ = check(&exp.expr, ctxt);
            // TODO change assert to error
            assert_eq!(typ, annot.typ);
            ctxt.exeunt();
            body.typ.clone()
        },
        EApp { exp, arg } => {
            let exp_t = check(&exp.expr, ctxt);
            let arg_t = check(&arg.expr, ctxt);
            match exp_t {
                Typ::Arrow(t1, t2) => {
                    if t1.as_ref() == &arg_t { t2.as_ref().clone() }
                    else { panic!("Oh no") }
                },
                _ => panic!("Expected function type")
            }
        },
        _ => Typ::Int
    }
}

pub fn check_decl(decl: Decl) {

}

pub fn check_prog(prog: Prog) {

}