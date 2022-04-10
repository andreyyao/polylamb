extern crate typed_sml;

use logos::Logos;
use typed_sml::lex::Token;

fn check_one(input: &str, token: Token) {
    let mut lexer = Token::lexer(input);
    assert_eq!(lexer.next(), Some(token));
    assert_eq!(lexer.slice(), input);
}

#[test]
fn int_lits() {
    let inputs = [
	"12345", "02020112", "0xAF7926", "0x000ABCD",
	"12638124", "0", "127436", "91746", "0xFFFF",
    ];
    for input in inputs { check_one(input, Token::IntLit); }
}

#[test]
fn bool_lits() {
    check_one("true", Token::BoolLit);
    check_one("false", Token::BoolLit);
}

#[test]
fn idents() {
    let inputs = [
	"abfuwegvdw", "AYUSDFIS", "aGgiIGoVoD",
	"A12796132_'", "B'_'_'_'_", "C1s1j7n8O9",
	"llllllllll", "wtf", "decoy_rubberband",
	"derive_macro", "lololol", "mitochondria",
	"Ahhhhhhh", "folder''", "map''"
    ];
    for input in inputs { check_one(input, Token::Ident); }
}

#[test]
fn infixes() {
    let inputs = [
	"*", "div", "+", "-", "=", "<>"
    ];
    for input in inputs { check_one(input, Token::Infix); }
}
