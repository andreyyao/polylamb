use crate::sml::ast;

type Id = String;

/// types
#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Int,
    Bool,
    Unit,
    // Poly(String),
    Prod(Vec<Typ>),
    Cont(Box<Typ>) // Continuations
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
    Con { constnt: ast::Constant, typ: Typ },
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
            ast::Typ::Unknown => panic!(),
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
            }
        }
    }
}


/// `cont` is the continuation
pub fn from_ast(expr: &ast::Expr, cont: &Value, ctyp: &Typ, unique: &mut i32) -> Expr {
    use ast::Expr as Axpr;

    match expr {
        Axpr::Var { id, typ } => {
            let var = Value::Var{ id: id.to_string(),
                                  typ: Typ::from_ast(typ) };
            Expr::App { fun: Box::new(cont.clone()),
                        arg: Box::new(var) }
        },
        Axpr::Con { constnt, typ } => {
            let con = Value::Con { constnt: constnt.clone(),
                                   typ: Typ::from_ast(typ) };
            Expr::App { fun: Box::new(cont.clone()),
                        arg: Box::new(con) }
        },
        // TODO change this
        _ => Expr::App { fun: Box::new(Value::Var {id: "TODO".to_string(), typ: Typ::Bool }),
                         arg: Box::new(Value::Var {id: "TODO".to_string(), typ: Typ::Bool }) }
        // Axpr::Lambda { args, body, typ } => {
            // *unique += 1;
            // let (t_arg, t_ret) = match typ {
            //  ast::Typ::Arrow(x, y) => (x, y),
            //  _ => panic!()
            // };
            // let cont_typ = Typ::from_ast(t_ret);
            // let cont_arg = Annot {
            //  id: format!("{}@{}", "cont_temp", unique),
            //  typ: cont_typ };
            // let body_cps =
            // let lambda = Value::Lambda {
            //  arg: (), body: (), typ: ()
            // };
        // }

    }
}
