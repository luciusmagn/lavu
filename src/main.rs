use color_eyre::eyre::Result;
use interpreter::GerbilInterpreter;
use reedline::Signal;

mod lexer;
use lexer::tokenize;

mod interpreter;

mod parser;
use parser::{parser1, parser2};

mod repl;
use repl::{line_editor, print_logo};

mod ast;
use ast::ast1::ariadne_yap;

mod chars;

fn main() -> Result<()> {
    color_eyre::install()?;

    let (mut line_editor, prompt) = line_editor()?;
    let mut interpreter = GerbilInterpreter::new()?;

    print_logo();

    loop {
        let sig = line_editor.read_line(&*prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                let lexed = tokenize(&buffer);

                // Check for unclosed expressions
                if let Some(error_idx) = parser1::find_unclosed_sexp(&lexed) {
                    if let Err(e) =
                        parser1::create_diagnostic(&buffer, &lexed, error_idx)
                    {
                        println!("Error creating diagnostic: {}", e);
                    }
                    continue;
                }

                // Try to parse the input
                match parser1::parse(&lexed) {
                    Ok(expressions) => {
                        println!("lexed tokens:");
                        println!("{:#?}", lexed);
                        println!("parsed ast:");
                        println!("{:#?}", expressions);
                        println!("ariadne diagnostics:");
                        ariadne_yap(&buffer, &lexed, &expressions)?;

                        let parsed2 =
                            parser2::parse(&buffer, &lexed, &expressions);

                        println!("{parsed2:#?}");

                        // Evaluate in Gerbil (for now)
                        if let Err(e) = interpreter.eval(&buffer) {
                            println!("Error communicating with Gerbil: {}", e);
                            continue;
                        }

                        // Print output
                        for line in interpreter.get_output() {
                            println!("{}", line);
                        }
                        interpreter.clear_output();
                    }
                    Err(errors) => {
                        println!("Parse errors: {:?}", errors);
                    }
                }
            }
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                println!("\nAborted!");
                interpreter.close()?;
                break;
            }
            x => {
                println!("Event: {:?}", x);
            }
        }
    }

    Ok(())
}
