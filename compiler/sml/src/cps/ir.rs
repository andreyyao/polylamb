use crate::sml::ast::{Constant, Binary};

type Id = String;

/// types
#[derive(Debug, PartialEq)]
pub enum Typ {
    Int,
    Bool,
    Poly(String),
    Tuple(Vec<Typ>),
    /// First arg is list of polymorphic type vars. Second is argument type
    Arrow(Vec<Id>, Box<Typ>),
}


// Stuff like `x: int`
#[derive(Debug, PartialEq)]
pub struct Annot { id: Id, typ: Typ }

#[derive(Debug, PartialEq)]
pub enum Decl {
    ValueDecl(Id, Value),
    BinopDecl(Id, Value, Binary, Value)
}


#[derive(Debug, PartialEq)]
pub enum Value {
    /// Constants 
    Con { constnt: Constant },
    /// Identifiers aka variables
    Var { id: Id, typ: Typ },
    /// Tuples, n >= 2
    Tuple { entries: Vec<Expr> },
    /// Anonymous functions
    Lambda { forallapp: Vec<Typ>, args: Vec<Annot>, body: Box<Expr> }
}


/// Expressions
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Function application
    App { fun: Box<Value>, typs: Vec<Typ>, arg: Box<Value> },
    /// `Let valbind in body end`
    Let { binding: Decl, body: Box<Expr> },
    /// Halting
    Halt { typ: Typ, value: Value },
    /// `if b then e1 else e2`
    Branch { cond: Box<Expr>, br_t: Box<Expr>, br_f: Box<Expr> },
}
