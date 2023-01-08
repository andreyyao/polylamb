use annotate_snippets::display_list::DisplayList;
use annotate_snippets::snippet::Snippet;
use compiler::system_f::ast::{Expr, Type};
use compiler::system_f::error::TypeError;
use compiler::system_f::parse::{parse_decl, parse_expr, parse_type};
use compiler::system_f::semant::{alpha_equiv, check, check_closed_expr};

const ALPHA_EQUIV_POSITIVE: [(&str, &str); 8] = [
    ("∀ A. A", "∀ B. B"),
    ("X", "X"),
    (
        "Int * (A -> B) * (∀ X. Y)",
        "Int * (A -> B) * (∀ Z. Y)",
    ),
    (
        "∀ X. X * (∀ Y. ∀ Z. X)",
        "∀ T. T * (∀ A. ∀ B. T)",
    ),
    ("∀ X. ∀ Y. Y", "∀ A. ∀ A. A"),
    (
        "Bool -> (∀ X. ∀ Y. X)",
        "Bool -> (∀ Z. ∀ X. Z)",
    ),
    (
        "∀ X. ∀ Y. ∀ Z. Y * Y -> Z",
        "∀ A. ∀ A. ∀ C. A * A -> C",
    ),
    (
        "∀ X. ∀ X. ∀ X. X -> X",
        "∀ A. ∀ B. ∀ C. C -> C",
    )
];

const ALPHA_EQUIV_NEGATIVE: [(&str, &str); 6] = [
    ("X", "Y"),
    ("∀ A. A", "∀ B. C"),
    ("Int -> Bool", "Unit -> Unit"),
    ("∀ X. ∀ Y. X", "∀ T1. ∀ T2. T2"),
    ("∀ X. X -> X", "∀ Y. Y -> Unit"),
    ("∀ Z. Z", "∀ TypVar. Typ"),
];

/// Pairs of (expr, type) strings
const BINOPS: [(&str, &str); 4] = [
    ("1 + 1", "Int"),
    ("2 < (3 * 4 + 5 * 7)", "Bool"),
    ("true && false", "Bool"),
    ("true && (if false then 1 < 2 else 10 == 10)", "Bool"),
];

/// Negative tests form binary expressions
const BINOPS_NEGATIVE: [&str; 4] = [
    "2 + (if 3 then 4 else 5)",
    "2 + false",
    "true && (if false then 1048576 else ~42069)",
    "true - 3 == 1",
];

#[test]
fn test_alpha_equiv() {
    for (s1, s2) in ALPHA_EQUIV_POSITIVE {
        let typ1 = parse_type(s1).unwrap().typ;
        let typ2 = parse_type(s2).unwrap().typ;
        println!("{}", typ1);
        println!("{}", typ2);
        assert!(alpha_equiv(&typ1, &typ2))
    }
}

#[test]
fn test_alpha_equiv_neg() {
    for (s1, s2) in ALPHA_EQUIV_NEGATIVE {
        let typ1 = parse_type(s1).unwrap().typ;
        let typ2 = parse_type(s2).unwrap().typ;
        println!("{}", typ1);
        println!("{}", typ2);
        assert!(!alpha_equiv(&typ1, &typ2))
    }
}

#[test]
fn test_type_checking() {
    for (s1, s2) in BINOPS {
        let exp = parse_expr(s1).unwrap().expr;
        let typ = parse_type(s2).unwrap().typ;
        let checked = check_closed_expr(&Expr::new(exp)).unwrap();
        println!("{}", typ);
        println!("{}", checked);
        assert!(alpha_equiv(&typ, &checked))
    }
}

#[test]
fn test_type_checking_negative() {
    for s in BINOPS_NEGATIVE {
        let exp = parse_expr(s).unwrap().expr;
        let error = check_closed_expr(&Expr::new(exp)).unwrap_err();
        let snippet: Snippet = error.into();
        let dlist = DisplayList::from(snippet);
        println!("{}\n", dlist)
    }
}
