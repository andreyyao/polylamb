use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    //Delimiters
    #[token(":")] Colon,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LBrack,
    #[token("]")] RBrack,
    
    //Arithmetic operators
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Times,

    //Literals
    #[regex(r"[1-9][0-9]*")] Number,
    
    //Normal Racket Keywords
    #[token("if")] If,
    #[token("define")] Define, //Top-level function defs
    #[token("lambda")] Lambda, //Anonymous functions

    //Type-related stuff
    #[token("->")] Arrow, //Function type constructor
    #[token("All")] Forall, //Parametric Polymorphism
    // #[token("define-type")] TypeDef,

    #[regex(r"[[a-zA-Z][\\!\\?_-]]+")] Identifier,
    
    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    // We can also use this variant to define whitespace,
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

fn main() {
    let mut lex = Token::lexer("Create ridiculously fast Lexers.");

    assert_eq!(lex.next(), Some(Token::Identifier));
    assert_eq!(lex.span(), 0..6);
    assert_eq!(lex.slice(), "Create");
}
