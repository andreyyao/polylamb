use compiler::system_f::ast::RawExpr;
use compiler::system_f::parse::{parse_decl, parse_val_expr};

const LITERALS: [&str; 8] = ["1", "~123", "true", "false", "null", "1048576", "0", "~0"];

// There are no types in the AST until type checking
const BINOPS: [&str; 8] = [
    "a + a",
    "a * true",
    "111 - ~100",
    "c1 && (c2 || c3)",
    "3 * (x - 1)",
    "a + b && c * d || e - f",
    "true + 1023",
    "null && (if true then false else null)",
];

const IFS: [&str; 6] = [
    "if b then 1 else 3",
    "if 120937 then x + y + z else (lambda x: Bool. not x)",
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

#[macro_export]
macro_rules! assert_matches {
    ( $x:expr, $y:pat ) => {
        assert!(matches!($x, $y))
    };
}

fn raw_expr_of(input: &str) -> RawExpr {
    let parse_result = parse_val_expr(input);
    parse_result.unwrap().expr
}

#[test]
fn check_exprs() {
    for s in LITERALS {
        assert_matches!(raw_expr_of(s), RawExpr::Con { .. })
    }
    for s in BINOPS {
        assert_matches!(raw_expr_of(s), RawExpr::Binop { .. })
    }
    for s in IFS {
        assert_matches!(raw_expr_of(s), RawExpr::If { .. })
    }
}

#[test]
fn check_decl() {
    for s in DECLS {
        assert_matches!(parse_decl(s), Result::Ok(..))
    }
}
