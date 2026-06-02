use lavu::datum_parser::parse;
use lavu::runtime::{Env, Value, eval_program};
use lavu::surface::classify_program;
use lavu::syntax::{Atom, Datum, SourceSpan, Spanned};

const PITFALL_SOURCE: &str = include_str!("fixtures/r5rs/r5rs_pitfall.scm");
const RETURN_VALUE: &str = "lavu-r5rs-pitfall-return-value";

#[derive(Debug, Clone)]
struct PitfallCase {
    id: String,
    expected: Spanned<Datum>,
    actual: Spanned<Datum>,
}

#[test]
fn sisc_r5rs_pitfall_fixture_is_parseable() {
    let datums = parse(PITFALL_SOURCE).expect("fixture should parse as Scheme datums");
    let cases = should_be_cases(&datums);

    assert_eq!(cases.len(), 22);
    assert!(cases.iter().any(|case| case.id == "7.4"));
    assert!(cases.iter().any(|case| case.id == "'map"));
}

#[test]
#[ignore = "manual R5RS compliance corpus; current Lavu still has known gaps"]
fn sisc_r5rs_pitfall_suite() {
    let datums = parse(PITFALL_SOURCE).expect("fixture should parse as Scheme datums");
    let cases = should_be_cases(&datums);
    let failures = cases
        .iter()
        .filter_map(|case| match run_case(case) {
            Ok(true) => None,
            Ok(false) => Some(format!("{}: value did not match expected result", case.id)),
            Err(error) => Some(format!("{}: {error}", case.id)),
        })
        .collect::<Vec<_>>();

    assert!(
        failures.is_empty(),
        "{} R5RS pitfall case(s) failed:\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

fn should_be_cases(datums: &[Spanned<Datum>]) -> Vec<PitfallCase> {
    datums.iter().filter_map(should_be_case).collect()
}

fn should_be_case(datum: &Spanned<Datum>) -> Option<PitfallCase> {
    let Datum::List(items) = &datum.node else {
        return None;
    };
    let [head, id, expected, actual] = items.as_slice() else {
        return None;
    };
    if identifier(head).as_deref() != Some("should-be") {
        return None;
    }

    Some(PitfallCase {
        id: datum_label(id),
        expected: expected.clone(),
        actual: actual.clone(),
    })
}

fn run_case(case: &PitfallCase) -> Result<bool, String> {
    let env = Env::new();
    match eval_datum(&env, equality_check(case))? {
        Value::Boolean(passed) => Ok(passed),
        other => Err(format!("comparison returned non-boolean {other}")),
    }
}

fn eval_datum(env: &Env, datum: Spanned<Datum>) -> Result<Value, String> {
    let program = classify_program(&[datum]).map_err(|error| error.to_string())?;
    eval_program(&program, env)
        .map_err(|error| error.to_string())?
        .into_iter()
        .last()
        .ok_or_else(|| "program produced no values".to_string())
}

fn equality_check(case: &PitfallCase) -> Spanned<Datum> {
    let span = enclosing_span(&case.expected.span, &case.actual.span);
    let value_binding = list(
        vec![
            identifier_datum(RETURN_VALUE, case.actual.span.clone()),
            case.actual.clone(),
        ],
        case.actual.span.clone(),
    );
    let bindings = list(vec![value_binding], case.actual.span.clone());
    let comparison = list(
        vec![
            identifier_datum("equal?", case.expected.span.clone()),
            identifier_datum(RETURN_VALUE, case.actual.span.clone()),
            case.expected.clone(),
        ],
        span.clone(),
    );

    list(
        vec![identifier_datum("let", span.clone()), bindings, comparison],
        span,
    )
}

fn list(items: Vec<Spanned<Datum>>, span: SourceSpan) -> Spanned<Datum> {
    Spanned::new(Datum::List(items), span)
}

fn identifier_datum(name: impl Into<String>, span: SourceSpan) -> Spanned<Datum> {
    Spanned::new(Datum::identifier(name), span)
}

fn identifier(datum: &Spanned<Datum>) -> Option<String> {
    match &datum.node {
        Datum::Atom(Atom::Identifier(name)) => Some(name.clone()),
        _ => None,
    }
}

fn enclosing_span(left: &SourceSpan, right: &SourceSpan) -> SourceSpan {
    left.start.min(right.start)..left.end.max(right.end)
}

fn datum_label(datum: &Spanned<Datum>) -> String {
    match &datum.node {
        Datum::Atom(atom) => atom_label(atom),
        Datum::Quote(datum) => format!("'{}", datum_label(datum)),
        _ => format!("{:?}", datum.node),
    }
}

fn atom_label(atom: &Atom) -> String {
    match atom {
        Atom::Identifier(name) => name.clone(),
        Atom::Integer(value) => value.to_string(),
        Atom::Decimal(value) => value.to_string(),
        Atom::Real(numerator, denominator) => format!("{numerator}/{denominator}"),
        Atom::ExactComplex(value) => format!("{value}"),
        Atom::Complex(value) => format!("{value}"),
        Atom::String(value) => format!("{value:?}"),
        Atom::Boolean(true) => "#t".to_string(),
        Atom::Boolean(false) => "#f".to_string(),
        Atom::Character(value) => format!("#\\{value}"),
    }
}
