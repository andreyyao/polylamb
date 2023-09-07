use annotate_snippets::display_list::DisplayList;
use annotate_snippets::snippet::Snippet;
use compiler::ast::ast::Expr;
use compiler::ast::parse::{parse_expr, parse_type};
use compiler::ast::semant::{alpha_equiv, check_closed_expr};

const ALPHA_EQUIV_POSITIVE: &[(&str, &str)] = &[
    ("∀ A. A", "∀ B. B"),
    ("X", "X"),
    ("Int * (A -> B) * (∀ X. Y)", "Int * (A -> B) * (∀ Z. Y)"),
    ("∀ X. X * (∀ Y. ∀ Z. X)", "∀ T. T * (∀ A. ∀ B. T)"),
    ("∀ X. ∀ Y. Y", "∀ A. ∀ A. A"),
    ("Bool -> (∀ X. ∀ Y. X)", "Bool -> (∀ Z. ∀ X. Z)"),
    ("∀ X. ∀ Y. ∀ Z. (Y * Y) -> Z", "∀ A. ∀ A. ∀ C. (A * A) -> C"),
    ("∀ X. ∀ X. ∀ X. X -> X", "∀ A. ∀ B. ∀ C. C -> C"),
];

const ALPHA_EQUIV_NEGATIVE: &[(&str, &str)] = &[
    ("X", "Y"),
    ("∀ A. A", "∀ B. C"),
    ("Int -> Bool", "Unit -> Unit"),
    ("∀ X. ∀ Y. X", "∀ T1. ∀ T2. T2"),
    ("∀ X. X -> X", "∀ Y. Y -> Unit"),
    ("∀ Z. Z", "∀ TypVar. Typ"),
];

/// Pairs of (binop, type) strings
const BINOPS: &[(&str, &str)] = &[
    ("1 + 1", "Int"),
    ("2 < (3 * 4 + 5 * 7)", "Bool"),
    ("true && false", "Bool"),
    ("true && (if false then 1 < 2 else 10 == 10)", "Bool"),
];

/// Pairs of (any, type) strings
const ANYS: &[(&str, &str)] = &[
    ("any A. λ (x: A) (y: A). x", "forall B. B -> B -> B"),
    (
        "any X. λ (x: X * X). (x, x)",
        "forall Y. Y * Y -> ((Y * Y) * (Y * Y))",
    ),
    (
        "any X. λ (x1: Int) (x2: Int). (x1, x2)",
        "forall B. Int -> Int -> (Int * Int)",
    ),
    ("(any X. Λ Y. λ (y: Y). y) [Int] [Bool]", "Bool -> Bool"),
];

/// Pairs of (λ, type) strings
const LAMBDAS: &[(&str, &str)] = &[
    ("λ (a: Int). 0", "Int -> Int"),
    (
        "λ (x: Int) (y: Int). (x - y) * (x + y)",
        "Int -> Int -> Int",
    ),
];

/// Pairs of (tuple, type) strings
const TUPLES: &[(&str, &str)] = &[
    (
        "(λ (x: Int). x + 1, λ (x: Bool). if x then 1 else 0)",
        "(Int -> Int) * (Bool -> Int)",
    ),
    ("(69, Λ A. λ (a: A). a)", "Int * (forall A. A -> A)"),
    (
	"(λ (x: Int). x + 1, λ (x: Bool). if x then 1 else 0)",
	"(Int -> Int) * (Bool -> Int)"
    )
];

/// Negative tests form binary expressions
const BINOP_NEG: &[&str] = &[
    "2 + (if 3 then 4 else 5)",
    "2 + false",
    "true & (if false then 1048576 else -42069)",
    "true - 3 == 1",
];

const LAMBDA_NEG: &[&str] = &["λ (x: Int) (x: Int). y", "λ (x: Int) (y: Bool) (y: Int). y"];

const LET_NEG: &[&str] = &[
    "let x: Int = 1 in x & true",
    "let (x: Int, y: Bool) = (1, 2) in x + y",
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
    let everything = [BINOPS, ANYS, LAMBDAS, TUPLES];
    for suite in everything {
        for (s1, s2) in suite {
            let exp = parse_expr(s1).unwrap().expr;
            println!("{}", exp);
            let typ = parse_type(s2).unwrap().typ;
            let checked = check_closed_expr(&Expr::new(exp)).unwrap();
            println!("{}", checked);
            assert!(alpha_equiv(&typ, &checked))
        }
    }
}

#[test]
fn test_type_checking_negative() {
    let everything = [BINOP_NEG, LAMBDA_NEG, LET_NEG];
    for suite in everything {
        for s in suite {
            let exp = parse_expr(s).unwrap().expr;
            let error = check_closed_expr(&Expr::new(exp)).unwrap_err();
            let snippet: Snippet = error.into();
            let dlist = DisplayList::from(snippet);
            println!("{}\n", dlist)
        }
    }
}
