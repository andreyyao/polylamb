// use annotate_snippets::display_list::{DisplayList, FormatOptions};
// use annotate_snippets::snippet::{Annotation, Slice, Snippet};
// use polylamb::ast::parse::parse_prog;
// use polylamb::ast::semant::check_prog;

// const SOURCE: &str = "tests/progs/simple.polylamb";

fn main() {
    let _ = polylamb::ast::repl::repl();
    // let program = std::fs::read_to_string(SOURCE).expect("Hmm");
    // let prog = parse_prog(&program).unwrap();
    // let result = check_prog(&prog);
    // let err = result.unwrap_err();
    // let snippet = Snippet {
    //     title: Some(Annotation {
    //         id: None,
    //         label: Some(err.title),
    //         annotation_type: err.annot_type,
    //     }),
    //     footer: vec![],
    //     slices: vec![Slice {
    //         source: &program,
    //         line_start: 1, // TODO
    //         origin: None,
    //         annotations: err.annotations,
    //         fold: false,
    //     }],
    //     opt: FormatOptions {
    //         color: true,
    //         anonymized_line_numbers: false,
    //         margin: None,
    //     },
    // };
    // let dlist: DisplayList = snippet.into();
    // println!("{}", dlist)
}
