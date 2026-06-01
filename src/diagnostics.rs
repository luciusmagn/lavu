use ariadne::{Color, Config, Label, Report, ReportKind, Source};

use crate::datum_parser::DatumParseError;
use crate::highlight::segments;
use crate::infer::TypeError;
use crate::query::QueryError;
use crate::runtime::EvalError;
use crate::surface::SurfaceError;
use crate::syntax::SourceSpan;

const REPL_SOURCE: &str = "repl";

pub fn report_query_error(input: &str, error: &QueryError) {
    match error {
        QueryError::Datum(error) => report_datum_error(input, error),
        QueryError::Surface(error) => report_surface_error(input, error),
        QueryError::Type(error) => report_type_error(input, error),
    }
}

pub fn report_datum_error(input: &str, error: &DatumParseError) {
    report(input, "parse error", &error.to_string(), datum_span(error));
}

pub fn report_surface_error(input: &str, error: &SurfaceError) {
    report(
        input,
        "syntax error",
        &error.to_string(),
        surface_span(error),
    );
}

pub fn report_type_error(input: &str, error: &TypeError) {
    report(input, "type error", &error.to_string(), type_span(error));
}

pub fn report_eval_error(input: &str, error: &EvalError) {
    report(
        input,
        "evaluation error",
        &error.to_string(),
        eval_span(error),
    );
}

fn report(input: &str, title: &'static str, message: &str, span: SourceSpan) {
    let span = normalize_span(input, span);

    // Disable underlines so the per-token highlight labels below tint the
    // source without each drawing its own underbar; the primary error label
    // still points at its span with a message arrow.
    let mut builder = Report::build(ReportKind::Error, (REPL_SOURCE, span.clone()))
        .with_config(Config::default().with_underlines(false))
        .with_message(title);

    // Syntax-highlight the displayed source by coloring each token's span.
    for segment in segments(input) {
        if let Some(color) = segment.category.ariadne_color() {
            builder.add_label(Label::new((REPL_SOURCE, segment.span.clone())).with_color(color));
        }
    }

    // The error label sits on top with a higher priority so its span keeps
    // the error color rather than the underlying syntax highlight.
    builder.add_label(
        Label::new((REPL_SOURCE, span))
            .with_message(message)
            .with_color(Color::Red)
            .with_priority(10),
    );

    let _ = builder.finish().eprint((REPL_SOURCE, Source::from(input)));
    eprintln!();
}

fn normalize_span(input: &str, span: SourceSpan) -> SourceSpan {
    let len = input.len();
    if len == 0 {
        return 0..1;
    }

    let start = span.start.min(len - 1);
    let end = span.end.min(len).max(start + 1);
    start..end
}

fn datum_span(error: &DatumParseError) -> SourceSpan {
    match error {
        DatumParseError::Lexer { span, .. }
        | DatumParseError::UnexpectedEnd { span, .. }
        | DatumParseError::UnexpectedToken { span, .. }
        | DatumParseError::UnclosedDelimiter { span }
        | DatumParseError::UnsupportedReaderSyntax { span, .. } => span.clone(),
    }
}

fn surface_span(error: &SurfaceError) -> SourceSpan {
    match error {
        SurfaceError::BadArity { span, .. }
        | SurfaceError::ExpectedIdentifier { span, .. }
        | SurfaceError::ExpectedList { span, .. }
        | SurfaceError::DuplicateIdentifier { span, .. }
        | SurfaceError::ReservedIdentifier { span, .. }
        | SurfaceError::EmptyApplication { span }
        | SurfaceError::UnsupportedDatum { span }
        | SurfaceError::DefinitionContext { span, .. }
        | SurfaceError::UnsupportedMacroPattern { span }
        | SurfaceError::InvalidMacroTemplate { span }
        | SurfaceError::NoMatchingMacroRule { span, .. }
        | SurfaceError::MacroExpansionLimit { span } => span.clone(),
    }
}

fn type_span(error: &TypeError) -> SourceSpan {
    match error {
        TypeError::UnboundVariable { span, .. }
        | TypeError::ExpectedProcedure { span, .. }
        | TypeError::ArityMismatch { span, .. }
        | TypeError::Mismatch { span, .. } => span.clone(),
    }
}

fn eval_span(error: &EvalError) -> SourceSpan {
    match error {
        EvalError::UnboundVariable { span, .. }
        | EvalError::UninitializedVariable { span, .. }
        | EvalError::NotProcedure { span }
        | EvalError::ArityMismatch { span, .. }
        | EvalError::TypeError { span, .. }
        | EvalError::IoError { span, .. }
        | EvalError::ReadError { span, .. }
        | EvalError::ContinuationJump { span, .. } => span.clone(),
    }
}
