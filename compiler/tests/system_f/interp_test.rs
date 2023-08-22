use compiler::system_f::interp::eval_expr;
use compiler::system_f::parse::parse_expr;
use compiler::system_f::semant::check_closed_expr;

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
    "100 > 101",
];

const LAMBDAS: &[&str] = &[
    "(λ (x: Bool). if x then 1 else 0)",
    "(λ x: Int. x + 1) 2",
    "(λ x: Int. λ y: Int. x + y) 10 5",
    "(let x: Int = 5 in λ y: Int. y + x) 4",
    "(any T. λ x: T. x) [Bool] true",
    r"let (x: Int, y: Bool, z: Int -> Bool) =
          (69, true, λ x: Int. x > 0) in
          if y && (z x) then x else 0 - x",
];

#[test]
fn test_snippets() {
    let everything = ARITHMETIC.iter().chain(BOOLEAN).chain(LAMBDAS);
    for expr_string in everything {
        println!("{}", expr_string);
        let exp = parse_expr(expr_string).unwrap();
        check_closed_expr(&exp).unwrap();
        println!("{}", eval_expr(&exp.expr));
        println!("---------------------------------------------")
    }
}
