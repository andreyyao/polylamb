use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

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
                        "#context" => print_context(&env, &ctxt),
                        _ => println!("Unknown command"),
                    }
                } else {
                    match parse_decl(input) {
                        Ok(decl) => match eval_decl(&decl, &mut ctxt, &mut env) {
                            Ok(_) => (),
                            Err(typ_err) => println!("{}", typ_err.title),
                        },
                        Err(_) => match parse_expr(input) {
                            Ok(expr) => match eval_expr(&expr, &mut ctxt, &mut env) {
                                Ok(closure) => println!("{}", closure),
                                Err(typ_err) => println!("{}", typ_err.title),
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

fn print_context(env: &Environment, ctxt: &Context) {
    for (k, v) in ctxt {
        println!("{} : {} := {}", k, v, *env[k].borrow())
    }
}

const HELP_MESSAGE: &str = r#"
List of commands in the repl:

#help - Displays this help message
#exit - Terminates the repl
#remove [id] - Removes [id] and its associated value from the context
#context - Print the current context"#;
