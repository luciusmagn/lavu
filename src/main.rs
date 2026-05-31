use color_eyre::eyre::Result;
use lavu::datum_parser::parse;
use lavu::query::infer_query;
use lavu::repl::{line_editor, print_logo};
use lavu::runtime::{eval_program, Env, Value};
use lavu::surface::classify_program;
use reedline::Signal;

fn main() -> Result<()> {
    color_eyre::install()?;

    let (mut line_editor, prompt) = line_editor()?;
    let env = Env::new();

    print_logo();

    loop {
        let sig = line_editor.read_line(&*prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                if let Some(query) = buffer.trim_start().strip_prefix('?') {
                    match infer_query(query) {
                        Ok(types) => {
                            for ty in types {
                                println!("{}", ty);
                            }
                        }
                        Err(error) => println!("Type query error: {}", error),
                    }
                    continue;
                }

                match eval_input(&buffer, &env) {
                    Ok(values) => {
                        for value in values {
                            if value != Value::Unspecified {
                                println!("{}", value);
                            }
                        }
                    }
                    Err(error) => println!("Eval error: {}", error),
                }
            }
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                println!("\nAborted!");
                break;
            }
            x => {
                println!("Event: {:?}", x);
            }
        }
    }

    Ok(())
}

fn eval_input(input: &str, env: &Env) -> Result<Vec<Value>> {
    let datums = parse(input)?;
    let program = classify_program(&datums)?;
    Ok(eval_program(&program, env)?)
}
