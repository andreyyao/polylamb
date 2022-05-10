extern crate sml;
use sml::parsing::lex::Token;
use logos::Logos;

fn check_one(input: &str, token: Token) {
    let mut lexer = Token::lexer(input);
    assert_eq!(lexer.next(), Some(token));
    assert_eq!(lexer.slice(), input);
}

const INT_PAIRS : [(&str, i64); 12] = [
    ("02020112", 2020112),
    ("0xAF7926", 0xaf7926),
    ("0x000ABCD", 0xabcd),
    ("12638124", 12638124),
    ("0xFFFF", 0xffff),
    ("127436", 127436),
    ("91746", 91746),
    ("12345", 12345),
    ("0", 0),
    ("~0", 0),
    ("~1023", -1023),
    ("~0xFFF", -0xfff)
];

const IDENTS: [&str; 21]= [
    "abfuwegvdw", "AYUSDFIS", "aGgiIGoVoD",
    "A12796132_'", "B'_'_'_'_", "C1s1j7n8O9",
    "llllllllll", "wtf", "decoy_rubberband",
    "derive_macro", "lololol", "mitochondria",
    "Ahhhhhhh", "folder''", "map''",
    "not2", "anotb", "notnot",
    "ifthenelse", "hehe", "h1h3oi4bh54o"
];

#[test]
fn int_lits() {
    for (input, expect) in INT_PAIRS {
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
    for input in IDENTS {
	check_one(input, Token::Ident(input));
    }
}

#[test]
fn infixes() {
    let ops6 = [ "+", "-"];
    let ops4 = [ "<", ">", "<=", ">=", "<>" ];
    let ops3 = [ "andalso", "orelse" ];
    check_one("*", Token::Mul);
    check_one("=", Token::Equal);
    for input in ops6 { check_one(input, Token::Infix6(input)); }
    for input in ops4 { check_one(input, Token::Infix4(input)); }
    for input in ops3 { check_one(input, Token::Infix3(input)); }
}

#[test]
fn keywords() {
    use Token::*;
    let pairs = [
	("if", If), ("then", Then), ("else", Else),
	("val", Val), ("rec", Rec), ("fn", Fn)
    ];
    for (input, expect) in pairs {
	check_one(input, expect);
    }
}

#[test]
fn symbols() {
    use Token::*;
    let pairs = [
	("(", LParen), (")", RParen), ("=", Equal), (",", Comma),
	(":", Colon), ("->", Arrow), ("=>", TwoArrow)
    ];
    for (input, expect) in pairs {
	check_one(input, expect);
    }
}
