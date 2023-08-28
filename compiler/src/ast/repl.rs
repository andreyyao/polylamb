use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

use crate::util::persistent::Snapshot;

use super::interp::eval_decl;
use super::parse::parse_decl;

pub fn repl() -> Result<()> {
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut env = Snapshot::default();
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
			    break
			},
			"#context" => {
			    println!("\n{}", env.current())
			},
			_ => println!("Unknown command")
		    }
		} else {
		    let decl = parse_decl(input).unwrap();
		    // TODO do proper error conversion
		    eval_decl(&decl, &mut env);
		}
            },
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("End of file");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}

const HELP_MESSAGE: &str = r#"
List of commands in the repl:

#help - Displays this help message
#exit - Terminates the repl
#remove [id] - Removes [id] and its associated value from the context
#context - Print the current context"#;