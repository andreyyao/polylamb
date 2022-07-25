use compiler::system_f::{ast, parse::{parse_val_expr, parse_prog}};
use compiler::system_f::ast::{Expr, RawExpr};

const LITERALS: [&str; 8] = [
    "1", "~123", "true", "false",
    "null", "1048576", "0", "~0"
];

const BINOPS: [&str; 5] = [
    "a + a", "a * true", "111 - ~100",
    "c1 && (c2 || c3)", "3 * (x - 1)"
];

#[test]
fn check_constants() {
    for input in LITERALS {
        let parse_result = parse_val_expr(input);
        assert!(matches!(
            parse_result.unwrap().expr,
            RawExpr::Con {..}
        ))
    }
}

#[test]
fn check_binops() {
    for input in BINOPS {
        let parse_result = parse_val_expr(input);
        assert!(matches!(
            parse_result.unwrap().expr,
            RawExpr::Binop {..}
        ))
    }
}