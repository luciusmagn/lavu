use color_eyre::eyre::Result;
use lavu::datum_parser::DatumParseError;
use lavu::datum_parser::parse;
use lavu::diagnostics::{
    report_datum_error, report_eval_error, report_query_error, report_surface_error,
    report_type_error,
};
use lavu::highlight::{paint_query, paint_type};
use lavu::infer::{Inferencer, TypeEnv, TypeError};
use lavu::query::infer_query_with_context;
use lavu::repl::{line_editor, print_logo};
use lavu::runtime::{Env, EvalError, Value, eval_top_level};
use lavu::surface::{SurfaceContext, SurfaceError};
use lavu::syntax::{Atom, Datum, Spanned};
use lavu::types::Type;
use reedline::Signal;

use std::io::IsTerminal;

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
                        Ok(types) => print_query_types(query, &types),
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

/// Print each inferred query type as `value : type-info`, echoing the source
/// of the form (syntax highlighted) beside its colored type when stdout is a
/// terminal. Procedure types expand into a multi-line parameter block, using
/// the queried lambda's formal names when available. Falls back to plain text
/// when piped, and to the bare type when the parse does not line up
/// one-to-one with the inferred forms.
fn print_query_types(query: &str, types: &[Type]) {
    let colored = std::io::stdout().is_terminal();
    let sources = parse(query)
        .ok()
        .filter(|datums| datums.len() == types.len());

    for (index, ty) in types.iter().enumerate() {
        match (sources.as_ref().map(|datums| &datums[index]), colored) {
            (Some(datum), true) => {
                let source = query[datum.span.clone()].trim();
                let names = lambda_param_names(&datum.node);
                println!("{}", paint_query(source, ty, &names));
            }
            (Some(datum), false) => println!("{} : {ty}", query[datum.span.clone()].trim()),
            (None, true) => println!("{}", paint_type(ty)),
            (None, false) => println!("{ty}"),
        }
    }

    if !types.is_empty() {
        println!();
    }
}

/// The formal parameter names of a queried `(lambda <formals> ...)`, in order
/// with any dotted rest name last, so the type display can label each
/// inferred parameter. Anything that is not a lambda yields no names.
fn lambda_param_names(datum: &Datum) -> Vec<String> {
    let Datum::List(items) = datum else {
        return Vec::new();
    };
    let (Some(head), Some(formals)) = (items.first(), items.get(1)) else {
        return Vec::new();
    };
    if !matches!(&head.node, Datum::Atom(Atom::Identifier(name)) if name == "lambda") {
        return Vec::new();
    }
    formal_names(&formals.node)
}

fn formal_names(formals: &Datum) -> Vec<String> {
    match formals {
        Datum::Atom(Atom::Identifier(name)) => vec![name.clone()],
        Datum::List(items) => items.iter().filter_map(identifier_name).collect(),
        Datum::DottedList(items, rest) => items
            .iter()
            .chain(std::iter::once(rest.as_ref()))
            .filter_map(identifier_name)
            .collect(),
        _ => Vec::new(),
    }
}

fn identifier_name(datum: &Spanned<Datum>) -> Option<String> {
    match &datum.node {
        Datum::Atom(Atom::Identifier(name)) => Some(name.clone()),
        _ => None,
    }
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
    let mut inferencer = Inferencer::new();
    let mut values = Vec::new();

    for datum in &datums {
        let mut next_surface = surface.clone();
        let program = next_surface.classify_program(std::slice::from_ref(datum))?;

        for form in &program.forms {
            let mut next_env = type_env.clone();
            if let Err(error) = inferencer.infer_top_level(form, &mut next_env) {
                return Ok(EvalOutput {
                    values,
                    type_error: Some(error),
                });
            }

            values.push(eval_top_level(form, env).map_err(ReplError::Eval)?);
            *type_env = next_env;
        }

        *surface = next_surface;
    }

    Ok(EvalOutput {
        values,
        type_error: None,
    })
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

        assert_eq!(output.values, Vec::<Value>::new());
        assert_eq!(
            output.type_error.unwrap().to_string(),
            "type constraint conflict: expected number?, got string?"
        );
        assert!(env.lookup("broken").is_none());

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
        assert_eq!(output.values, vec![Value::Unspecified]);
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("broken").is_none());
        let types = infer_query_with_context("x", &surface, &type_env).unwrap();
        assert_eq!(types[0].to_string(), "number?");
    }

    #[test]
    fn keeps_successful_macros_before_later_forms() {
        let env = Env::new();
        let mut surface = SurfaceContext::new();
        let mut type_env = TypeEnv::new();

        let output = eval_input(
            "(define-syntax id
               (syntax-rules ()
                 ((id x) x)))
             (id 1)",
            &env,
            &mut surface,
            &mut type_env,
        )
        .unwrap();

        assert_eq!(
            output
                .values
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            vec!["1".to_string()]
        );
        assert_eq!(
            infer_query_with_context("(id \"x\")", &surface, &type_env).unwrap()[0].to_string(),
            "string?"
        );
    }

    #[test]
    fn does_not_keep_macros_after_failed_forms() {
        let env = Env::new();
        let mut surface = SurfaceContext::new();
        let mut type_env = TypeEnv::new();

        let output = eval_input(
            "(define (broken x) (+ x \"hello\"))
             (define-syntax id
               (syntax-rules ()
                 ((id x) x)))",
            &env,
            &mut surface,
            &mut type_env,
        )
        .unwrap();

        assert!(output.type_error.is_some());
        let QueryError::Type(error) =
            infer_query_with_context("(id 1)", &surface, &type_env).unwrap_err()
        else {
            panic!("expected id macro not to be installed");
        };
        assert_eq!(error.to_string(), "unbound variable: id");
    }
}
