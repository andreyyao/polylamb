use crate::system_f::ast;

type Id = String;

/// types
#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Int,
    Bool,
    Unit,
    /* Type variable introduced by Forall types */
    TVar(Id),
    /* Product type */
    Prod(Vec<Typ>),
    /* Continuations, no return type */
    Cont(Box<Typ>),
    /* Universal types */
    Forall(Vec<Id>, Box<Typ>)
}


// Stuff like `x: int`
#[derive(Debug, PartialEq, Clone)]
pub struct Annot { id: Id, typ: Typ }

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    ValueDecl(Id, Value),
    BinopDecl(Id, Value, ast::Binary, Value)
}


/// Values cannot be beta reduced further
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// Identifiers aka variables
    Var { id: Id, typ: Typ },
    /// Constants
    Con { constant: ast::Constant, typ: Typ },
    /// Tuples, n >= 2
    Tuple { entries: Vec<Value>, typ: Typ },
    /// Anonymous functions
    Lambda { arg: Vec<Annot>, body: Box<Expr>, typ: Typ }
}


/// Expressions
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// `Let valbind in body end`
    Let { binding: Decl, body: Box<Expr>, typ: Typ },
    /// Function application
    App { fun: Box<Value>, arg: Box<Value> },
    /// Halting
    Halt { typ: Typ, value: Value },
    /// `if b then e1 else e2`
    Branch { cond: Box<Value>, br_t: Box<Expr>, br_f: Box<Expr>, typ: Typ },
}


impl Typ {

    /// Converts from ast type to continuation
    fn to_cont(typ: &ast::Typ) -> Typ {
        Typ::Cont(Box::new(Typ::from_ast(typ)))
    }

    /// Translates from ast types
    pub fn from_ast(typ: &ast::Typ) -> Typ {
        match typ {
            ast::Typ::Unknown => panic!("Encountered expr with unchecked type"),
            ast::Typ::Int => Typ::Int,
            ast::Typ::Bool => Typ::Bool,
            ast::Typ::Unit => Typ::Unit,
            ast::Typ::Prod(ts) => {
                let irts = ts.iter().map(|t| Typ::from_ast(t)).collect();
                Typ::Prod(irts)
            },
            ast::Typ::Arrow(t1, t2) => {
                let irt1 = Typ::from_ast(t1);
                let irt2 = Typ::to_cont(t2);
                Typ::Cont(Box::new(Typ::Prod(vec![irt1, irt2])))
            },
            _ => Typ::Int
        }
    }
}