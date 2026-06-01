use color_eyre::eyre::Result;
use lavu::datum_parser::DatumParseError;
use lavu::datum_parser::parse;
use lavu::diagnostics::{
    report_datum_error, report_eval_error, report_query_error, report_surface_error,
};
use lavu::query::infer_query;
use lavu::repl::{line_editor, print_logo};
use lavu::runtime::{Env, EvalError, Value, eval_program};
use lavu::surface::{SurfaceError, classify_program};
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
                        Err(error) => report_query_error(query, &error),
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
                    Err(ReplError::Datum(error)) => report_datum_error(&buffer, &error),
                    Err(ReplError::Surface(error)) => report_surface_error(&buffer, &error),
                    Err(ReplError::Eval(error)) => report_eval_error(&buffer, &error),
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

#[derive(Debug)]
enum ReplError {
    Datum(DatumParseError),
    Surface(SurfaceError),
    Eval(EvalError),
}

fn eval_input(input: &str, env: &Env) -> std::result::Result<Vec<Value>, ReplError> {
    let datums = parse(input)?;
    let program = classify_program(&datums)?;
    eval_program(&program, env).map_err(ReplError::Eval)
}

impl From<DatumParseError> for ReplError {
    fn from(error: DatumParseError) -> Self {
        Self::Datum(error)
    }
}

impl From<SurfaceError> for ReplError {
    fn from(error: SurfaceError) -> Self {
        Self::Surface(error)
    }
}
