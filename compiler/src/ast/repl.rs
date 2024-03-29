use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

use super::error::TypeError;
use super::interp::{eval_decl, eval_expr, Environment};
use super::parse::{parse_decl, parse_expr};
use super::semant::Context;

pub fn repl() -> Result<()> {
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut env = Environment::default();
    let mut ctxt = Context::default();
    println!("Welcome to the polylamb interpreter!");
    println!("Type \"#help\" to display the help message\n");
    loop {
        let readline = rl.readline("λ2 >> ");
        match &readline {
            Ok(input) => {
                rl.add_history_entry(input.as_str())?;
                if input.starts_with('#') {
                    match input.as_str() {
                        "#help" => println!("{}", HELP_MESSAGE),
                        "#exit" => {
                            println!("See ya!");
                            break;
                        }
                        "#env" => print_env(&env, &ctxt),
                        _ => println!("Unknown command"),
                    }
                } else {
                    match parse_decl(input) {
                        Ok(decl) => match eval_decl(&decl, &mut ctxt, &mut env) {
                            Ok(_) => (),
                            Err(err) => display_type_error(input, err),
                        },
                        Err(_) => match parse_expr(input) {
                            Ok(expr) => match eval_expr(&expr, &mut ctxt, &mut env) {
                                Ok(closure) => println!("{}", closure),
                                Err(err) => display_type_error(input, err),
                            },
                            Err(parse_err) => println!("{}", parse_err),
                        },
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("End of file");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}

fn print_env(env: &Environment, ctxt: &Context) {
    for (k, v) in ctxt {
        println!("{} : {} := {}", k, v, env[k])
    }
}

fn display_type_error(source: &str, err: TypeError) {
    use annotate_snippets::display_list::DisplayList;
    use annotate_snippets::display_list::FormatOptions;
    use annotate_snippets::snippet::*;
    let snippet = Snippet {
        title: Some(Annotation {
            id: None,
            label: Some(err.title),
            annotation_type: err.annot_type,
        }),
        footer: vec![],
        slices: vec![Slice {
            source: &source,
            line_start: 1, // TODO
            origin: None,
            annotations: err.annotations,
            fold: false,
        }],
        opt: FormatOptions {
            color: true,
            anonymized_line_numbers: false,
            margin: None,
        },
    };
    let dlist: DisplayList = snippet.into();
    println!("{}", dlist)
}

const HELP_MESSAGE: &str = r#"
#help - Displays this help message
#exit - Terminates the repl
#remove [id] - Removes [id] and its associated value from the context
#env - Print the current context"#;
