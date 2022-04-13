use typed_sml::parse::parser;
use typed_sml::lex::Token;
use typed_sml::lex::LexerWrap;
use logos::Logos;

/// Just asserts that the input string parses as a well-formed
/// Expression. They might not be well-typed, but that's fine
fn expr_ok(input: &str){
    let lexer = LexerWrap{ lexer: Token::lexer(input) };
    assert!(parser::ExprParser::new().parse(lexer).is_ok());
}

const INPUT_ATOMS : [&str; 15] = [
    "haha", "decoy", "rubberband", "(2 + 4)",
    "1", "x", "x_x", "()", "true", "false",
    "((), (()), ((())))", "(2, 4, x)", "(\n\t\n)",
    "(fn (x: int) : int => x + 1 * 3)", "(twice f 1)",
];

const INPUT_IFS : [&str; 4] = [
    "if b then 1 else 3",
    "if 120937 then x + y + z else (fn (x : bool) : bool => not x)",
    "if b then b else if b then b else b",
    "if (x * y <= z = 1) then a else ((((), ())))"
];

#[test]
fn well_formed() {
    for input in INPUT_ATOMS {
	expr_ok(input);
    };
    for input in INPUT_IFS {
	expr_ok(input);
    }
}
