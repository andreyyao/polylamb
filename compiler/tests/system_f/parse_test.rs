use compiler::system_f::ast::{RawExpr, RawType};
use compiler::system_f::parse::{parse_decl, parse_expr, parse_type};

const LITERALS: [&str; 8] = ["1", "~123", "true", "false", "null", "1048576", "0", "~0"];

// There are no types in the AST until type checking
const BINOPS: [&str; 10] = [
    "a + a",
    "a * true",
    "111 - ~100",
    "c1 && (c2 || c3)",
    "3 * (x - 1)",
    "a + b && c * d || e - f",
    "true + 1023",
    "null && (if true then false else null)",
    "1 + (lambda (x: Int) :Int. x) 2",
    "true * (lambda (x:A) (y:B) (z: C) :I . y)",
];

const IFS: [&str; 6] = [
    "if b then 1 else 3",
    "if 120937 then x + y + z else (lambda (x: Bool) :T. not x)",
    "if b then b else if b then b else b",
    "if (x * y < z == 1) then a else (((2, 4)))",
    "if teehee then 999 else 12479468",
    "if (0,0,9999 + 8,9) then hehehehe else (not 1 + 2)",
];

const DECLS: [&str; 3] = [
    "let x: Int = 1",
    "let xx: Hehe_2_w = (true && false + ~1048)",
    "let xx: Bool = y",
];

const TYPE_VARS: [&str; 10] = [
    "A",
    "B",
    "Z",
    "B1719364_76432",
    "YMCA",
    "CS6120",
    "CS6110",
    "WHAT_DOES_THE_FOX_SAY",
    "Hahahaha",
    "Never_gonna_give_you_up",
];

fn raw_expr_of(input: &str) -> RawExpr {
    let parse_result = parse_expr(input);
    parse_result.unwrap().expr
}

fn raw_type_of(input: &str) -> RawType {
    let parse_result = parse_type(input);
    parse_result.unwrap().typ
}

#[macro_export]
macro_rules! assert_matches {
    ( $x:expr, $y:pat ) => {
        assert!(matches!($x, $y))
    };
}

#[test]
fn check_exprs() {
    for s in LITERALS {
        let exp = raw_expr_of(s);
        println!("{}", exp);
        assert_matches!(exp, RawExpr::Con { .. })
    }
    for s in BINOPS {
        let exp = raw_expr_of(s);
        println!("{}", exp);
        assert_matches!(exp, RawExpr::Binop { .. })
    }
    for s in IFS {
        let exp = raw_expr_of(s);
        println!("{}", exp);
        assert_matches!(exp, RawExpr::If { .. })
    }
}

#[test]
fn check_types() {
    for t in TYPE_VARS {
        let typ = raw_type_of(t);
        println!("{}", typ);
        assert_matches!(typ, RawType::TVar(..))
    }
}

#[test]
fn check_decls() {
    for s in DECLS {
        assert_matches!(parse_decl(s), Result::Ok(..))
    }
}
