/*! Lexer for System F. The actual lexer is generated by Logos. */

use super::error::LexError;
use logos::{Lexer, Logos};
use std::fmt;

/// Callback for bool literal tokens
fn token_bool_lit<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<bool, LexError> {
    match lex.slice().parse::<bool>() {
        Result::Ok(b) => Ok(b),
        Result::Err(_err) => Err(lex.span().start),
    }
}

/// Callback for int literal tokens
fn token_int_lit<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<i64, LexError> {
    match lex.slice().parse::<i64>() {
        // Arithmetic magic to encode the negative sign
        Result::Ok(i) => Ok(i),
        Result::Err(_err) => Err(lex.span().start),
    }
}

// Tokens
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip "([ \t\n\r]+)|(/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/)")]
#[logos(error = LexError)]
pub enum Token<'source> {
    // Punctuations
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("=")]
    Equal,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("_")]
    Underscore,
    #[token("->")]
    Arrow,

    /// Precedence 7 as multiplication
    #[token("*")]
    Mul,

    // Different precedences in the token enum level is
    // necessary for the parser to disambiguate
    /// Precedence 6
    #[regex(r"\-|\+", |lex| lex.slice())]
    Infix6(&'source str),

    /// Precedence 4
    #[regex(r"<|>|==|!=", |lex| lex.slice())]
    Infix4(&'source str),

    /// Precedence 3
    #[regex(r"[&|]", |lex| lex.slice())]
    Infix3(&'source str),

    /// Identifiers
    #[regex(r"[a-z][0-9a-zA-Z_]*", |lex| lex.slice())]
    ExpId(&'source str),

    /// Type Identifiers
    #[regex(r"[A-Z][0-9a-zA-Z_]*", |lex| lex.slice())]
    TypId(&'source str),

    /// Integer literals
    #[regex(r"-?[0-9]+", token_int_lit)]
    IntLit(i64),

    /// Bool literals
    #[regex(r"true|false", token_bool_lit)]
    BoolLit(bool),

    /// Unit literal aka null
    #[token("null")]
    UnitLit,

    /***** Keywords *****/
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("let")]
    Let,
    #[token("fix")]
    Fix,
    #[token("and")]
    And,
    #[token("in")]
    In,
    #[regex("Λ|any")]
    Any,
    #[regex("\\\\|λ|lambda")]
    Lambda,
    #[regex("∀|forall")]
    Forall,

    // Built-in types
    #[token("Int")]
    TInt,
    #[token("Bool")]
    TBool,
    #[token("Unit")]
    TUnit,
}

impl<'source> fmt::Display for Token<'source> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

pub struct LexerWrap<'source> {
    lexer: Lexer<'source, Token<'source>>,
}

impl<'source> LexerWrap<'source> {
    pub fn new(input: &'source str) -> Self {
        LexerWrap {
            lexer: Token::lexer(input),
        }
    }
}

// Implements iterator for lalrpop Result type
impl<'source> Iterator for LexerWrap<'source> {
    type Item = Result<(usize, Token<'source>, usize), LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        let token_opt = self.lexer.next();
        let span = self.lexer.span();
        match token_opt {
            Some(Ok(token)) => Some(Ok((span.start, token, span.end))),
            Some(Err(_)) => Some(Err(span.start)),
            None => None,
        }
    }
}
