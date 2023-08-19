use annotate_snippets::display_list::DisplayList;
use annotate_snippets::snippet::Snippet;
use compiler::system_f::ast::Expr;
use compiler::system_f::interp::eval_expr;
use compiler::system_f::parse::{parse_expr, parse_type};
use compiler::system_f::semant::{alpha_equiv, check_closed_expr};

const ARITHMETIC: &[&str] = &[
    "1 + 3",
    "2 - 7 * 3",
    "3 * 3 * -1",
    "(-1 * -3) * (2 + 5)",
    "-1048576 * -1048576",
    "0 * -0",
    "9223372036854775807 * -1",
    "9223372036854775807 + 1", // Min int
];

const BOOLEAN: &[&str] = &[
    "true",
    "false",
    "true && false",
    "true || false",
    "true && true",
    "true || true",
    "false && false",
    "false || false",
    "1 < 3",
    "-9223372036854775808 < 0",
    "0 < 0",
    "0 > 6",
    "100 > 99",
    "100 > 101"
];

const LAMBDAS: &[&str] = &[
    "(λ (x: Int). x + 1, λ (x: Bool). if x then 1 else 0)"
];

#[test]
fn test_all() {
    let everything = ARITHMETIC.iter().chain(BOOLEAN).chain(LAMBDAS);
    for expr_string in everything {
        println!("{}", expr_string);
	let exp = parse_expr(expr_string).unwrap().expr;
	println!("{}", eval_expr(&exp).unwrap());
	println!("---------------------------------------------")
    }
}