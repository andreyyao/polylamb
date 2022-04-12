use std::{fmt, ops::{Deref, DerefMut}};
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

//TODO make the identifiers borrow names instead of copying them
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {

    #[token(":")] Colon,

    #[token(",")] Comma,

    #[token("=")] Equal,

    #[token("->")] Arrow,
    
    #[token("(")] LParen,

    #[token(")")] RParen,
    
    #[token("~")] Neg,

    #[token("not")] Not,

    #[token("*")] Mul,
    
    /// Precedence 7
    #[regex(r"mod", token_infix)]
    Infix7(ast::Binary),

    /// Precedence 6
    #[regex(r"\-|\+", token_infix)]	
    Infix6(ast::Binary),

    /// Precedence 4
    #[regex(r"<|>|<>|<=|>=|andalso|orelse", token_infix)]
    Infix4(ast::Binary),

    /// Identifiers
    #[regex(r"[a-zA-Z][0-9a-zA-Z_']*", |lex| lex.slice().to_string())]
    Ident(String),

    /// Integer literals
    #[regex(r"(0x[0-9A-F]+)|([0-9]+)", token_int_lit)]
    IntLit(i64),

    /// Bool literals
    #[regex(r"true|false", token_bool_lit)]
    BoolLit(bool),

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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

pub type LexicalError = usize;

// Wraps the lexer in a self-defined struct. Otherwise
// Rust complains about implementing Iterator trait for
// Lexer<Token> struct, neither of which I created.
pub struct LexerWrap<'source> { pub lexer: Lexer<'source, Token> }

impl<'source> Deref for LexerWrap<'source> {
    type Target = Lexer<'source, Token>;
    fn deref(&self) -> &Self::Target {
	&self.lexer
    }
}

impl<'source> DerefMut for LexerWrap<'source> {
    fn deref_mut(&mut self) -> &mut Self::Target {
	&mut self.lexer
    }
}

// Implements iterator for lalrpop Result type
impl<'source> Iterator for LexerWrap<'source> {
    type Item = Result<(usize, Token, usize), LexicalError>;
    fn next(&mut self) -> Option<Self::Item> {
	let token_opt = self.lexer.next();
	let span = self.span();
	token_opt.map(|token| Ok((span.start, token, span.end)))
    }
}
