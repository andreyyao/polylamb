use crate::ast::ast::{self, Constant};

type Id = String;

/// types
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Bool,
    Unit,
    // Type variable
    TVar(Id),
    // Tuple types
    Prod(Vec<Type>),
    // Continuations, no return type. Cont()
    Cont(Box<Type>),
    // Universal types
    Forall(Id, Box<Type>),
    // Existential types, for typing closures
    Exists(Id, Box<Type>)
}

/// Bindings
#[derive(Debug, PartialEq, Clone)]
pub enum Bind {
    ValueBind(Id, Value),
    ProjBind(Id, u16, Value),
    BinopBind(Id, Value, ast::Binary, Value),
}

/// Values cannot be beta reduced further
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// Variables
    Var(Id),
    /// Constants
    Con(Constant),
    /// Tuples, n >= 2
    Tuple(Vec<Value>),
    /// Possibly recursive functions
    Fixpoints {
        arg: Vec<(Id, Type)>,
        body: Box<Expr>,
        typ: Type,
    },
}

/// Expressions
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// `Let valbind in body end`
    Let {
        binding: Bind,
        body: Box<Expr>,
        typ: Type,
    },
    /// Function application
    App { fun: Box<Value>, arg: Box<Value> },
    /// Halting
    Halt { typ: Type, value: Value },
    /// `if b then e1 else e2`
    Branch {
        cond: Box<Value>,
        br_t: Box<Expr>,
        br_f: Box<Expr>,
        typ: Type,
    },
}

// impl Typ {
//     /// Converts from ast type to continuation
//     fn to_cont(typ: &ast::Typ) -> Typ {
//         Typ::Cont(Box::new(Typ::from_ast(typ)))
//     }

//     /// Translates from ast types
//     pub fn from_ast(typ: &ast::RawType) -> Typ {
//         match typ {
//             ast::RawType::Int => Typ::Int,
//             ast::RawType::Bool => Typ::Bool,
//             ast::RawType::Unit => Typ::Unit,
//             ast::RawType::Prod(ts) => {
//                 let irts = ts.iter().map(|t| Typ::from_ast(t)).collect();
//                 Typ::Prod(irts)
//             },
//             ast::RawType::Arrow(t1, t2) => {
//                 let irt1 = Typ::from_ast(t1);
//                 let irt2 = Typ::to_cont(t2);
//                 Typ::Cont(Box::new(Typ::Prod(vec![irt1, irt2])))
//             },
//             _ => Typ::Int
//         }
//     }
// }
