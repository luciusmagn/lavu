use thiserror::Error;

use crate::datum_parser::{DatumParseError, parse};
use crate::infer::{Inferencer, TraceStep, TypeEnv, TypeError};
use crate::surface::{SurfaceContext, SurfaceError, classify_program};
use crate::types::Type;

#[derive(Debug, Error)]
pub enum QueryError {
    #[error(transparent)]
    Datum(#[from] DatumParseError),

    #[error(transparent)]
    Surface(#[from] SurfaceError),

    #[error(transparent)]
    Type(#[from] TypeError),
}

pub fn infer_query(input: &str) -> Result<Vec<Type>, QueryError> {
    let datums = parse(input)?;
    let program = classify_program(&datums)?;
    let mut env = TypeEnv::new();
    let mut inferencer = Inferencer::new();

    Ok(inferencer.infer_program(&program, &mut env)?)
}

pub fn infer_query_with_surface(
    input: &str,
    surface: &SurfaceContext,
) -> Result<Vec<Type>, QueryError> {
    infer_query_with_context(input, surface, &TypeEnv::new())
}

pub fn infer_query_with_context(
    input: &str,
    surface: &SurfaceContext,
    env: &TypeEnv,
) -> Result<Vec<Type>, QueryError> {
    let datums = parse(input)?;
    let mut surface = surface.clone();
    let program = surface.classify_program(&datums)?;
    let mut env = env.clone();
    let mut inferencer = Inferencer::new();

    Ok(inferencer.infer_program(&program, &mut env)?)
}

/// Infer `input` while recording the engine's steps, for the `?? expr` REPL
/// command. Returns the inferred types alongside the ordered trace.
pub fn trace_query_with_context(
    input: &str,
    surface: &SurfaceContext,
    env: &TypeEnv,
) -> Result<(Vec<Type>, Vec<TraceStep>), QueryError> {
    let datums = parse(input)?;
    let mut surface = surface.clone();
    let program = surface.classify_program(&datums)?;
    let mut env = env.clone();
    let mut inferencer = Inferencer::new();
    inferencer.enable_trace();

    let types = inferencer.infer_program(&program, &mut env)?;
    Ok((types, inferencer.trace_steps()))
}

#[cfg(test)]
mod tests {
    use super::{
        infer_query, infer_query_with_context, infer_query_with_surface, trace_query_with_context,
    };
    use crate::datum_parser::parse;
    use crate::infer::{Inferencer, TypeEnv};
    use crate::surface::SurfaceContext;

    #[test]
    fn infers_query_expression_type() {
        let types = infer_query("(lambda (x) (+ x 1))").unwrap();

        assert_eq!(types[0].to_string(), "(-> number? number?)");
    }

    #[test]
    fn trace_records_steps_innermost_first() {
        let (types, steps) =
            trace_query_with_context("(+ 1 2)", &SurfaceContext::new(), &TypeEnv::new()).unwrap();

        assert_eq!(types[0].to_string(), "number?");

        // Operands are settled before the application that combines them.
        assert_eq!(steps.first().unwrap().detail, "literal");
        let last = steps.last().unwrap();
        assert_eq!(last.detail, "application of `+`");
        assert_eq!(last.ty.to_string(), "number?");
        assert_eq!(
            steps.iter().filter(|step| step.detail == "literal").count(),
            2
        );
    }

    #[test]
    fn infers_multiple_query_forms() {
        let types = infer_query("1 \"x\"").unwrap();

        assert_eq!(types[0].to_string(), "number?");
        assert_eq!(types[1].to_string(), "string?");
    }

    #[test]
    fn infers_with_repl_surface_macros() {
        let mut surface = SurfaceContext::new();
        let datums = parse(
            "(define-syntax id
               (syntax-rules ()
                 ((id x) x)))",
        )
        .unwrap();
        surface.classify_program(&datums).unwrap();

        let types = infer_query_with_surface("(id (+ 1 2))", &surface).unwrap();

        assert_eq!(types[0].to_string(), "number?");
    }

    #[test]
    fn infers_with_repl_type_bindings() {
        let mut surface = SurfaceContext::new();
        let datums = parse("(define x 1)").unwrap();
        let program = surface.classify_program(&datums).unwrap();
        let mut env = TypeEnv::new();
        let mut inferencer = Inferencer::new();
        inferencer.infer_program(&program, &mut env).unwrap();

        let types = infer_query_with_context("x", &surface, &env).unwrap();

        assert_eq!(types[0].to_string(), "number?");
    }
}
