lalrpop_mod!(pub parser); // synthesized by LALRPOP
use crate::{ast, lex::{LexerWrap, Token}};
use logos::Logos;

pub mod utils {
    use crate::ast::*;

    pub fn make_binop(l: Expr, op: &str, r: Expr) -> Expr {
	Expr::Binop { op: Binary::of_str(op),
		      lhs: Box::new(l),
		      rhs: Box::new(r),
		      typ: Typ::Unknown }
    }
}

pub fn parse_expr(input: &str) -> Result<ast::Expr, lalrpop_util::ParseError<usize, Token<'_>, usize>> {
    let lexer = LexerWrap{ lexer: Token::lexer(input) };
    parser::ExprParser::new().parse(lexer)
}
