use std::fmt;
use crate::ast;
use crate::lex;
use logos::Lexer;

/// Returns the (l, r) binding power of `op`
/// Higher power binds tighter
fn binary_binding_power(op: ast::Binary) -> (u8, u8) {
    use ast::Binary::*;
    match op {
	Mul | Mod => (13, 14),
        Add | Sub => (11, 12),
	Eq | Lt | Gt | Le | Ge | Ne => (7, 8),
	Andalso | Orelse => (5, 6)
    }
}

/// `parse_expr` parses the remaining tokens in `lexer` based on `min_bp`.
/// Adapted from [here](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
/// # Arguments:
/// * `lexer`: The token stream
/// * `min_bp`: Minimum binding power
/// # Returns:
/// The parsed AST
fn parse_expr(lexer: &mut Lexer<lex::Token>, min_bp: u8) -> ast::Expr {
    use lex::Token::*;
    use ast::Expr;
    let mut lhs = match lexer.next().unwrap() {
        Ident => Expr::Var { id: lexer.slice().to_string(),
			     typ: Default::default() },
        LParen => {
            let exp = parse_expr(lexer, 0);
            assert_eq!(lexer.next(), Some(RParen));
            exp
        }
        Not => {
            let exp = parse_expr(lexer, 15); //Binding power of function app
            Expr::Unop { op: ast::Unary::Not,
			 kid: Box::new(exp),
			 typ: Default::default() }
        }
	//Function App binds stronger than * and mod
	Neg => {
            let exp = parse_expr(lexer, 15); 
            Expr::Unop { op: ast::Unary::Neg,
			 kid: Box::new(exp),
			 typ: Default::default() }
        }
	//If binds weaker than andalso, =, <>, etc
	If => {
	    let exp_b = parse_expr(lexer, 4);
	    assert_eq!(lexer.next(), Some(Then));
	    let exp_t = parse_expr(lexer, 4);
	    assert_eq!(lexer.next(), Some(Else));
	    let exp_f = parse_expr(lexer, 4);
	    Expr::Branch {
		cond: Box::new(exp_b),
		br_t: Box::new(exp_t),
		br_f: Box::new(exp_f),
		typ: Default::default()
	    }
	}
        t => panic!("bad token: {:?}", t),
    };
    
    lhs
    
}
