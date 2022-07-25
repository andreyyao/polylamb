mod util;
use compiler::sml::{ast, parse::{parse_valexpr, parse_prog}};

// #[test]
// fn check_benchmarks() {
//     let map = util::read_benches("../benches/hehe.txt");
//     for (key, value) in map {
// 	let prog = parse_prog(&value).unwrap();
// 	println!("{} {}", key, prog.len());
//     }
// }
