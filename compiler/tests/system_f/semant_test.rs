use compiler::system_f::ast::{RawExpr, RawType};
use compiler::system_f::parse::{parse_decl, parse_typ_expr, parse_val_expr};

const ALPHA_EQUIV_POSITIVE: [(&str, &str); 6] = [
    ("forall A. A", "forall B. B"),
    ("X", "X"),
    (
        "Int * (A -> B) * (forall X. Y)",
        "Int * (A -> B) * (forall Z. Y)",
    ),
    (
        "forall X. X * (forall Y. forall Z. X)",
        "forall T. T * (forall A. forall B. T)",
    ),
    ("forall X. forall Y. Y", "forall A. forall A. A"),
    (
        "Bool -> (forall X. forall Y. X)",
        "Bool -> (forall Z. forall X. Z)",
    ),
];

const ALPHA_EQUIV_NEGATIVE: [(&str, &str); 6] = [
    ("X", "Y"),
    ("forall A. A", "forall B. C"),
    ("Int -> Bool", "Unit -> Unit"),
    ("forall X. forall Y. X", "forall T1. forall T2. T2"),
    ("forall X. X -> X", "forall Y. Y -> Unit"),
    ("forall Z. Z", "forall TypVar. Typ"),
];

#[test]
fn test_alpha_equiv() {
    for (s1, s2) in ALPHA_EQUIV_POSITIVE {
        let typ1 = parse_typ_expr(s1).unwrap().typ;
        let typ2 = parse_typ_expr(s2).unwrap().typ;
        println!("{}", typ1);
        println!("{}", typ2);
        assert!(RawType::alpha_equiv(&typ1, &typ2))
    }
}

#[test]
fn test_alpha_equiv_neg() {
    for (s1, s2) in ALPHA_EQUIV_NEGATIVE {
        let typ1 = parse_typ_expr(s1).unwrap().typ;
        let typ2 = parse_typ_expr(s2).unwrap().typ;
        println!("{}", typ1);
        println!("{}", typ2);
        assert!(!RawType::alpha_equiv(&typ1, &typ2))
    }
}
