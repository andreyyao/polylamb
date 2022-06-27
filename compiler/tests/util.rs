use std::fs;
use std::collections::HashMap;


pub fn read_benches(lang: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for file in fs::read_dir(format!("{}{}", "./benches/", lang)).unwrap() {
	let prog_name =
	    file.as_ref().unwrap().path().file_stem()
	    .unwrap().to_str().unwrap().to_string();
	let prog = fs::read_to_string(file.unwrap().path()).unwrap();
	map.insert(prog_name, prog);
    }
    map
}
