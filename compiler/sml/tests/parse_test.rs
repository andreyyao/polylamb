mod util;
use sml::sml::{ast, parse::{parse_valexpr, parse_prog}};


const INPUT_ATOMS : [&str; 15] = [
    "haha", "decoy", "rubberband", "(2 + 4)",
    "1", "x", "x_x", "2", "true", "false",
    "(1, 2)", "(2, 4, x)", "((0,0),(0,0))",
    "(fn (x: int) => x + 1 * 3)", "(twice (f 1))",
];

const INPUT_IFS : [&str; 6] = [
    "if b then 1 else 3",
    "if 120937 then x + y + z else (fn (x : bool) => not x)",
    "if b then b else if b then b else b",
    "if (x * y <= z = 1) then a else (((2, 4)))",
    "if teehee then 999 else 12479468",
    "if (0,0,9999 + 8,9) then hehehehe else (not 1 + 2)"
];

const INPUT_FNS : [&str; 6] = [
    "fn (x: int) => 1048576",
    "fn (x: int) => x + 1",
    /* "fn (x: 't1, y: 't2, z: 't3) => x * y + (if true then z else x)", */
    "fn (x: bool, y: bool * int) => (y, x)",
    "fn (x: bool, y: bool) => z + w",
    "fn (x: int) => fn (y: int -> int) => y (y x)",
    "fn (pair: int * int) => 0"
];

const INPUT_BINOPS : [&str; 8] = [
    "1 = 1", "2 + 2", "x + y - z * x", "(~1 + 1289768) * (100 * variable)",
    "1 + 1 + 1", "0 + aaa", "x <= y", "cond1 andalso cond2 orelse cond3"
];

#[test]
fn check_atoms() {
    for input in INPUT_ATOMS {
	let expr_result = parse_valexpr(input);
	assert!(expr_result.is_ok());
    }
}

#[test]
fn check_ifs() {
    for input in INPUT_IFS {
	assert!(matches!(parse_valexpr(input), Ok(ast::Expr::Branch{..})));
    }
}

#[test]
fn check_fns() {
    for input in INPUT_FNS {
	let parse_result = parse_valexpr(input);
	assert!(
	    matches!(parse_result, Ok(ast::Expr::Lambda{..})),
	    "\nInput: { }\nGot: {:?}\n", input, parse_result 
	);
    }
}

#[test]
fn check_binops() {
    for input in INPUT_BINOPS {
	let parse_result = parse_valexpr(input);
	assert!(
	    matches!(parse_result, Ok(ast::Expr::Binop{..})),
	    "\nInput: { }\nGot: {:?}\n", input, parse_result 
	);
    }
}

#[test]
fn check_benchmarks() {
    let map = util::read_benches();
    for (key, value) in map {
	let prog = parse_prog(&value).unwrap();
	println!("{} {}", key, prog.len());
    }
}
