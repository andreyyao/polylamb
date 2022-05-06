lalrpop_mod!(pub parser, "/parsing/parser.rs"); // synthesized by LALRPOP
use crate::parsing::{ast, lex::{LexerWrap, Token}};

pub mod utils {
    use crate::parsing::ast::*;

    pub fn make_binop(l: Expr, op: &str, r: Expr) -> Expr {
	Expr::Binop { op: Binary::of_str(op),
		      lhs: Box::new(l),
		      rhs: Box::new(r),
		      typ: Typ::Unknown }
    }
}

type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, usize>;

pub fn parse_expr(input: &str) -> Result<ast::Expr, ParseError> {
    let lexer = LexerWrap::new(input);
    parser::ValExprParser::new().parse(lexer)
}

pub fn parse_valbind(input: &str) -> Result<ast::ValBind, ParseError> {
    let lexer = LexerWrap::new(input);
    parser::ValBindParser::new().parse(lexer)
}

pub fn parse_prog(input: &str) -> Result<ast::Prog, ParseError> {
    let lexer = LexerWrap::new(input);
    parser::ProgParser::new().parse(lexer)
}
