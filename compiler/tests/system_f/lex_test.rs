use compiler::system_f::lex::Token;
use logos::Logos;

/// Expects to see `token` when lexing `input`
fn check_one(input: &str, token: Token) {
    let mut lexer = Token::lexer(input);
    assert_eq!(lexer.next(), Some(token));
    assert_eq!(lexer.slice(), input);
}

const INT_PAIRS : [(&str, i64); 7] = [
    ("02020112", 2020112),
    ("12638124", 12638124),
    ("127436", 127436),
    ("~91746", -91746),
    ("12345", 12345),
    ("0", 0),
    ("~0", 0),
];

const EXP_IDENTS: [&str; 21] = [
    "abfuwegvdw", "aYUSDFIS", "aGgiIGoVoD",
    "xA12796132_", "b____", "c1s1j7n8O9",
    "llllllllll", "wtf", "decoy_rubberband",
    "derive_macro", "lololol", "mitochondria",
    "ahhhhhhh", "folder", "map",
    "not2", "anotb", "notnot",
    "ifthenelse", "hehe", "h1h3oi4bh54o"
];

const TYP_IDENTS: [&str; 12] = [
    "Float", "A__ababa", "Object",
    "Tensor", "CS6120", "HahaDecoyRubberband",
    "Reduce", "Reuse", "Recycle",
    "ASYFIA7EWGF", "A_jigfwe_vtw1fwf12132y____",
    "ProjectContractChargingPeriodProjectAccountReferenceVM"
];

const COMMENTS: [&str; 4] = [
    "/**/",
    "/* 2 + 3 = 5 */",
    "/* let x : Int = 1 in x */",
    "\n/*\t\t\n boom shakalaka. */\n\t"
];

#[test]
fn int_lits() {
    for (input, expect) in INT_PAIRS {
        check_one(input, Token::IntLit(expect));
    }
}

#[test]
fn unit_lit() {
    check_one("null", Token::UnitLit);
}

#[test]
fn bool_lits() {
    check_one("true", Token::BoolLit(true));
    check_one("false", Token::BoolLit(false));
}

#[test]
fn idents() {
    for input in EXP_IDENTS {
        check_one(input, Token::ExpId(input));
    }
    for input in TYP_IDENTS {
        check_one(input, Token::TypId(input));
    }
}

#[test]
fn comments() {
    for input in COMMENTS {
        let mut lexer = Token::lexer(input);
        assert_eq!(lexer.next(), None)
    }
}

#[test]
fn infixes() {
    let ops6 = [ "+", "-"];
    let ops4 = [ "<", ">", "==", "!=" ];
    let ops3 = [ "&&", "||" ];
    check_one("*", Token::Mul);
    check_one("=", Token::Equal);
    for input in ops6 { check_one(input, Token::Infix6(input)); }
    for input in ops4 { check_one(input, Token::Infix4(input)); }
    for input in ops3 { check_one(input, Token::Infix3(input)); }
}