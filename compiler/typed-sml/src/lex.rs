use crate::ast;
use logos::{Logos, Lexer};

/// Callback for infix operator tokens 
fn token_infix(lex: &mut Lexer<Token>) -> Option<ast::Binary> {
    let slice : &str = lex.slice();
    use ast::Binary::*;
    use Option::*;
    Some (match slice {
	"+" => Add,
	"-" => Sub,
	"*" => Mul,
	"mod" => Mod,
	"=" => Eq,
	"<" => Lt,
	">" => Gt,
	"<=" => Le,
	">=" => Ge,
	"<>" => Ne,
	"andalso" => Andalso,
	"orelse" => Orelse,
	_ => panic!(" At the Disco")
    })
}

/// Callback for bool literal tokens
fn token_bool_lit(lex: &mut Lexer<Token>) -> Option<bool> {
    let slice : &str = lex.slice();
    let n = slice.parse::<bool>();
    match n {
	Result::Ok(b) => Some(b),
	Result::Err(_) => panic!("Expected bool literal")
    }
}

/// Callback for int literal tokens
fn token_int_lit(lex: &mut Lexer<Token>) -> Option<i64> {
    let slice : &str = lex.slice();
    let n = if slice.starts_with("0x") {
	let slice2 = slice.trim_start_matches("0x");
	i64::from_str_radix(slice2, 16)
    }
    else {
	slice.parse::<i64>()
    };
    match n {
	Result::Ok(i) => Some(i),
	Result::Err(_) => panic!("Expected integer literal")
    }
}

#[derive(Logos, Debug, PartialEq)]
pub enum Token {

    #[token(":")] Colon,

    #[token(",")] Comma,

    #[token("->")] Arrow,
    
    #[token("(")] LParen,

    #[token(")")] RParen,
    
    #[token("~")] Neg,

    #[token("not")] Not,

    #[regex(r"\*|\-|\+|=||<|>|<>|<=|>=|andalso|orelse", token_infix)]
    Infix(ast::Binary),

    /// Identifiers
    #[regex(r"[a-zA-Z][0-9a-zA-Z_']*")] Ident,

    /// Integer literals
    #[regex(r"(0x[0-9A-F]+)|([0-9]+)", token_int_lit)] IntLit(i64),

    /// Bool literals
    #[regex(r"true|false", token_bool_lit)] BoolLit(bool),

    /***** Keywords *****/
    #[token("if")] If,
    #[token("then")] Then,
    #[token("else")] Else,
    #[token("val")] Val,
    #[token("rec")] Rec,
    #[token("fn")] Fn,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[error]
    #[regex(r#"[ \t\n\f]+"#, logos::skip)]
    Error,
}
