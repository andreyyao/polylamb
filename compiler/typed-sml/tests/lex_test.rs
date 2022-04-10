extern crate typed_sml;
use logos::Logos;
use typed_sml::lex::Token;
use typed_sml::ast::Binary;

fn check_one(input: &str, token: Token) {
    let mut lexer = Token::lexer(input);
    assert_eq!(lexer.next(), Some(token));
    assert_eq!(lexer.slice(), input);
}

#[test]
fn int_lits() {
    let pairs = [
	("12345", 12345),
	("02020112", 2020112),
	("0xAF7926", 0xaf7926),
	("0x000ABCD", 0xabcd),
	("12638124", 12638124),
	("0", 0),
	("127436", 127436),
	("91746", 91746),
	("0xFFFF", 0xffff)
    ];
    for (input, expect) in pairs {
	check_one(input, Token::IntLit(expect));
    }
}

#[test]
fn bool_lits() {
    check_one("true", Token::BoolLit(true));
    check_one("false", Token::BoolLit(false));
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
    for input in inputs {
	check_one(input, Token::Ident(input.to_string()));
    }
}

#[test]
fn infixes() {
    use Binary::*;
    let pairs = [
	("*", Mul),
	("mod", Mod),
	("+", Add),
	("-", Sub),
	("=", Eq),
	("<", Lt),
	(">", Gt),
	("<=", Le),
	(">=", Ge),
	("<>", Ne),
	("andalso", Andalso),
	("orelse", Orelse)
    ];
    for (input, expect) in pairs {
	check_one(input, Token::Infix(expect));
    }
}

#[test]
fn keywords() {
    use Token::*;
    let pairs = [
	("if", If),
	("then", Then),
	("else", Else),
	("val", Val),
	("rec", Rec),
	("fn", Fn)
    ];
    for (input, expect) in pairs {
	check_one(input, expect);
    }
}
