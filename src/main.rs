use color_eyre::eyre::Result;
use lavu::datum_parser::DatumParseError;
use lavu::datum_parser::parse;
use lavu::diagnostics::{
    report_datum_error, report_eval_error, report_query_error, report_surface_error,
    report_type_error,
};
use lavu::infer::{Inferencer, TypeEnv, TypeError};
use lavu::query::infer_query_with_context;
use lavu::repl::{line_editor, print_logo};
use lavu::runtime::{Env, EvalError, Value, eval_program};
use lavu::surface::{Program, SurfaceContext, SurfaceError};
use reedline::Signal;

fn main() -> Result<()> {
    color_eyre::install()?;

    let (mut line_editor, prompt) = line_editor()?;
    let env = Env::new();
    let mut surface = SurfaceContext::new();
    let mut type_env = TypeEnv::new();

    print_logo();

    loop {
        let sig = line_editor.read_line(&*prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                if let Some(query) = buffer.trim_start().strip_prefix('?') {
                    match infer_query_with_context(query, &surface, &type_env) {
                        Ok(types) => {
                            for ty in types {
                                println!("{}", ty);
                            }
                        }
                        Err(error) => report_query_error(query, &error),
                    }
                    continue;
                }

                match eval_input(&buffer, &env, &mut surface, &mut type_env) {
                    Ok(output) => {
                        for value in output.values {
                            if value != Value::Unspecified {
                                println!("{}", value);
                            }
                        }
                        if let Some(error) = output.type_error {
                            report_type_error(&buffer, &error);
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

#[derive(Debug)]
struct EvalOutput {
    values: Vec<Value>,
    type_error: Option<TypeError>,
}

fn eval_input(
    input: &str,
    env: &Env,
    surface: &mut SurfaceContext,
    type_env: &mut TypeEnv,
) -> std::result::Result<EvalOutput, ReplError> {
    let datums = parse(input)?;
    let program = surface.classify_program(&datums)?;
    let values = eval_program(&program, env).map_err(ReplError::Eval)?;
    let type_error = update_type_env(&program, type_env).err();
    Ok(EvalOutput { values, type_error })
}

fn update_type_env(program: &Program, type_env: &mut TypeEnv) -> Result<(), TypeError> {
    let mut inferencer = Inferencer::new();
    for form in &program.forms {
        let mut next_env = type_env.clone();
        inferencer.infer_top_level(form, &mut next_env)?;
        *type_env = next_env;
    }
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::eval_input;
    use lavu::infer::TypeEnv;
    use lavu::query::{QueryError, infer_query_with_context};
    use lavu::runtime::{Env, Value};
    use lavu::surface::SurfaceContext;

    #[test]
    fn reports_failed_inference_without_updating_repl_type_env() {
        let env = Env::new();
        let mut surface = SurfaceContext::new();
        let mut type_env = TypeEnv::new();

        let output = eval_input(
            "(define (broken x) (+ x \"hello\"))",
            &env,
            &mut surface,
            &mut type_env,
        )
        .unwrap();

        assert_eq!(output.values, vec![Value::Unspecified]);
        assert_eq!(
            output.type_error.unwrap().to_string(),
            "type mismatch: expected number?, got string?"
        );

        let QueryError::Type(error) =
            infer_query_with_context("broken", &surface, &type_env).unwrap_err()
        else {
            panic!("expected broken to remain absent from the type environment");
        };
        assert_eq!(error.to_string(), "unbound variable: broken");
    }

    #[test]
    fn keeps_successful_forms_before_a_failed_type_update() {
        let env = Env::new();
        let mut surface = SurfaceContext::new();
        let mut type_env = TypeEnv::new();

        let output = eval_input(
            "(define x 1) (define (broken y) (+ y \"hello\"))",
            &env,
            &mut surface,
            &mut type_env,
        )
        .unwrap();

        assert!(output.type_error.is_some());
        let types = infer_query_with_context("x", &surface, &type_env).unwrap();
        assert_eq!(types[0].to_string(), "number?");
    }
}
