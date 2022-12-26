lalrpop_mod!(pub parser, "/system_f/parser.rs"); // synthesized by LALRPOP
use super::{ast, lex::{LexerWrap, Token}};

pub mod utils {
    use crate::system_f::ast::*;

    pub fn make_binop(l: Expr, op: &str, r: Expr) -> RawExpr {
        RawExpr::Binop {
            op: Binary::of_str(op),
            lhs: Box::new(l),
            rhs: Box::new(r)
        }
    }
}

type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, usize>;

/// Parses a value expression
pub fn parse_val_expr(input: &str) -> Result<ast::Expr, ParseError> {
    let lexer = LexerWrap::new(input);
    parser::ValExprParser::new().parse(lexer)
}

/// Parses a type expression
pub fn parse_typ_expr(input: &str) -> Result<ast::Type, ParseError> {
    let lexer = LexerWrap::new(input);
    parser::TypExprParser::new().parse(lexer)
}

/// Parses a function declaration
pub fn parse_decl(input: &str) -> Result<ast::Decl, ParseError> {
    let lexer = LexerWrap::new(input);
    parser::DeclParser::new().parse(lexer)
}

/// Parses a source file
pub fn parse_prog(input: &str) -> Result<ast::Prog, ParseError> {
    let lexer = LexerWrap::new(input);
    parser::ProgParser::new().parse(lexer)
}
