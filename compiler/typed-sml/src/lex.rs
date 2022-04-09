use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("->")]
    Arrow,
    
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,
    
    #[token("~")]
    Neg,

    #[regex(r"\*|div|-|\+|=|<>")]
    Infix, //Priority: 7, 7, 6, 6, 4, 4

    #[regex(r"[a-zA-Z][0-9a-zA-Z_']*")]
    Ident, //Identifiers

    #[regex(r"(0x[0-9A-F]+)|([0-9]+)")]
    IntLit, //Integer literals

    #[regex(r"true|false")]
    BoolLit, //Boolean literals

    #[regex(r"andalso|orelse|not|if|then|else|val|rec|fn")]
    Keyword,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
