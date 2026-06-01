use thiserror::Error;

use crate::datum_parser::{DatumParseError, parse};
use crate::infer::{Inferencer, TypeEnv, TypeError};
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
    let datums = parse(input)?;
    let mut surface = surface.clone();
    let program = surface.classify_program(&datums)?;
    let mut env = TypeEnv::new();
    let mut inferencer = Inferencer::new();

    Ok(inferencer.infer_program(&program, &mut env)?)
}

#[cfg(test)]
mod tests {
    use super::{infer_query, infer_query_with_surface};
    use crate::datum_parser::parse;
    use crate::surface::SurfaceContext;

    #[test]
    fn infers_query_expression_type() {
        let types = infer_query("(lambda (x) (+ x 1))").unwrap();

        assert_eq!(types[0].to_string(), "(-> number? number?)");
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
}
