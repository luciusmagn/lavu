use crate::types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Primitive {
    pub name: &'static str,
    pub signature: Type,
    pub predicate: Option<PredicateRefinement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PredicateRefinement {
    pub argument: usize,
    pub positive: Type,
}

impl Primitive {
    fn new(name: &'static str, signature: Type) -> Self {
        Self {
            name,
            signature,
            predicate: None,
        }
    }

    fn predicate(name: &'static str, positive: Type) -> Self {
        Self {
            name,
            signature: Type::procedure(vec![Type::Any], Type::Boolean),
            predicate: Some(PredicateRefinement {
                argument: 0,
                positive,
            }),
        }
    }
}

pub fn r5rs_primitives() -> Vec<Primitive> {
    let a = Type::Var("a".to_string());
    let b = Type::Var("b".to_string());
    let thunk = Type::procedure(vec![], Type::Any);
    let result_thunk = Type::procedure(vec![], a.clone());
    let continuation = Type::procedure(vec![a.clone()], Type::Never);
    let continuation_receiver = Type::procedure(vec![continuation], b.clone());
    let continuation_result = Type::union(vec![a.clone(), b.clone()]);

    let mut primitives = vec![
        Primitive::predicate("boolean?", Type::Boolean),
        Primitive::predicate("number?", Type::Number),
        Primitive::predicate("complex?", Type::Number),
        Primitive::predicate("real?", Type::Number),
        Primitive::predicate("rational?", Type::Number),
        Primitive::predicate("integer?", Type::Number),
        Primitive::new("exact?", number_predicate()),
        Primitive::new("inexact?", number_predicate()),
        Primitive::new("zero?", number_predicate()),
        Primitive::new("positive?", number_predicate()),
        Primitive::new("negative?", number_predicate()),
        Primitive::new("odd?", number_predicate()),
        Primitive::new("even?", number_predicate()),
        Primitive::predicate("char?", Type::Char),
        Primitive::predicate("string?", Type::String),
        Primitive::predicate("symbol?", Type::Symbol),
        Primitive::predicate(
            "pair?",
            Type::Pair(Box::new(Type::Any), Box::new(Type::Any)),
        ),
        Primitive::predicate("null?", Type::Null),
        Primitive::predicate("list?", Type::List),
        Primitive::predicate("vector?", Type::Vector),
        Primitive::predicate(
            "procedure?",
            Type::Procedure(crate::types::ProcedureType::Rest {
                required: Vec::new(),
                rest: Box::new(Type::Any),
                result: Box::new(Type::Any),
            }),
        ),
        Primitive::predicate("port?", Type::Port),
        Primitive::predicate("input-port?", Type::InputPort),
        Primitive::predicate("output-port?", Type::OutputPort),
        Primitive::predicate("eof-object?", Type::EofObject),
        Primitive::new("not", Type::procedure(vec![Type::Any], Type::Boolean)),
        Primitive::new(
            "eqv?",
            Type::procedure(vec![Type::Any, Type::Any], Type::Boolean),
        ),
        Primitive::new(
            "eq?",
            Type::procedure(vec![Type::Any, Type::Any], Type::Boolean),
        ),
        Primitive::new(
            "equal?",
            Type::procedure(vec![Type::Any, Type::Any], Type::Boolean),
        ),
        Primitive::new(
            "force",
            Type::procedure(vec![Type::PromiseOf(Box::new(a.clone()))], a.clone()),
        ),
        Primitive::new("values", Type::uniform_variadic(Type::Any, Type::Unknown)),
        Primitive::new(
            "call-with-values",
            Type::procedure(vec![Type::Any, Type::Any], Type::Any),
        ),
        Primitive::new(
            "apply",
            Type::rest_procedure(vec![Type::Any, Type::Any], Type::Any, Type::Any),
        ),
        Primitive::new(
            "symbol->string",
            Type::procedure(vec![Type::Symbol], Type::String),
        ),
        Primitive::new(
            "string->symbol",
            Type::procedure(vec![Type::String], Type::Symbol),
        ),
        Primitive::new(
            "char->integer",
            Type::procedure(vec![Type::Char], Type::Number),
        ),
        Primitive::new(
            "integer->char",
            Type::procedure(vec![Type::Number], Type::Char),
        ),
        Primitive::new("char-alphabetic?", char_predicate()),
        Primitive::new("char-numeric?", char_predicate()),
        Primitive::new("char-whitespace?", char_predicate()),
        Primitive::new("char-upper-case?", char_predicate()),
        Primitive::new("char-lower-case?", char_predicate()),
        Primitive::new("char-upcase", Type::procedure(vec![Type::Char], Type::Char)),
        Primitive::new(
            "char-downcase",
            Type::procedure(vec![Type::Char], Type::Char),
        ),
        Primitive::new(
            "current-input-port",
            Type::procedure(vec![], Type::InputPort),
        ),
        Primitive::new(
            "current-output-port",
            Type::procedure(vec![], Type::OutputPort),
        ),
        Primitive::new(
            "open-input-file",
            Type::procedure(vec![Type::String], Type::InputPort),
        ),
        Primitive::new(
            "close-input-port",
            Type::procedure(vec![Type::InputPort], Type::Unknown),
        ),
        Primitive::new(
            "open-output-file",
            Type::procedure(vec![Type::String], Type::OutputPort),
        ),
        Primitive::new(
            "call-with-input-file",
            Type::procedure(
                vec![
                    Type::String,
                    Type::procedure(vec![Type::InputPort], a.clone()),
                ],
                a.clone(),
            ),
        ),
        Primitive::new(
            "call-with-output-file",
            Type::procedure(
                vec![
                    Type::String,
                    Type::procedure(vec![Type::OutputPort], a.clone()),
                ],
                a.clone(),
            ),
        ),
        Primitive::new(
            "with-input-from-file",
            Type::procedure(vec![Type::String, result_thunk.clone()], a.clone()),
        ),
        Primitive::new(
            "with-output-to-file",
            Type::procedure(vec![Type::String, result_thunk.clone()], a.clone()),
        ),
        Primitive::new("load", Type::procedure(vec![Type::String], Type::Unknown)),
        Primitive::new(
            "eval",
            Type::procedure(vec![Type::Any, Type::Any], Type::Any),
        ),
        Primitive::new(
            "scheme-report-environment",
            Type::procedure(vec![Type::Number], Type::Any),
        ),
        Primitive::new(
            "null-environment",
            Type::procedure(vec![Type::Number], Type::Any),
        ),
        Primitive::new(
            "interaction-environment",
            Type::procedure(vec![], Type::Any),
        ),
        Primitive::new(
            "dynamic-wind",
            Type::procedure(vec![thunk.clone(), result_thunk, thunk], a.clone()),
        ),
        Primitive::new(
            "call-with-current-continuation",
            Type::procedure(
                vec![continuation_receiver.clone()],
                continuation_result.clone(),
            ),
        ),
        Primitive::new(
            "call/cc",
            Type::procedure(vec![continuation_receiver], continuation_result),
        ),
        Primitive::new(
            "close-output-port",
            Type::procedure(vec![Type::OutputPort], Type::Unknown),
        ),
        Primitive::new(
            "read",
            Type::optional_procedure(vec![], vec![Type::InputPort], Type::Any),
        ),
        Primitive::new(
            "read-char",
            Type::optional_procedure(
                vec![],
                vec![Type::InputPort],
                Type::union(vec![Type::Char, Type::EofObject]),
            ),
        ),
        Primitive::new(
            "peek-char",
            Type::optional_procedure(
                vec![],
                vec![Type::InputPort],
                Type::union(vec![Type::Char, Type::EofObject]),
            ),
        ),
        Primitive::new(
            "char-ready?",
            Type::optional_procedure(vec![], vec![Type::InputPort], Type::Boolean),
        ),
        Primitive::new(
            "write",
            Type::optional_procedure(vec![Type::Any], vec![Type::OutputPort], Type::Unknown),
        ),
        Primitive::new(
            "display",
            Type::optional_procedure(vec![Type::Any], vec![Type::OutputPort], Type::Unknown),
        ),
        Primitive::new(
            "newline",
            Type::optional_procedure(vec![], vec![Type::OutputPort], Type::Unknown),
        ),
        Primitive::new(
            "write-char",
            Type::optional_procedure(vec![Type::Char], vec![Type::OutputPort], Type::Unknown),
        ),
        Primitive::new(
            "transcript-on",
            Type::procedure(vec![Type::String], Type::Unknown),
        ),
        Primitive::new("transcript-off", Type::procedure(vec![], Type::Unknown)),
        Primitive::new(
            "number->string",
            Type::optional_procedure(vec![Type::Number], vec![Type::Number], Type::String),
        ),
        Primitive::new(
            "string->number",
            Type::optional_procedure(
                vec![Type::String],
                vec![Type::Number],
                Type::union(vec![Type::Number, Type::Boolean]),
            ),
        ),
        Primitive::new("+", Type::uniform_variadic(Type::Number, Type::Number)),
        Primitive::new("*", Type::uniform_variadic(Type::Number, Type::Number)),
        Primitive::new(
            "-",
            Type::rest_procedure(vec![Type::Number], Type::Number, Type::Number),
        ),
        Primitive::new(
            "/",
            Type::rest_procedure(vec![Type::Number], Type::Number, Type::Number),
        ),
        Primitive::new("=", numeric_comparison()),
        Primitive::new("<", numeric_comparison()),
        Primitive::new(">", numeric_comparison()),
        Primitive::new("<=", numeric_comparison()),
        Primitive::new(">=", numeric_comparison()),
        Primitive::new(
            "max",
            Type::rest_procedure(vec![Type::Number], Type::Number, Type::Number),
        ),
        Primitive::new(
            "min",
            Type::rest_procedure(vec![Type::Number], Type::Number, Type::Number),
        ),
        Primitive::new("abs", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new(
            "quotient",
            Type::procedure(vec![Type::Number, Type::Number], Type::Number),
        ),
        Primitive::new(
            "remainder",
            Type::procedure(vec![Type::Number, Type::Number], Type::Number),
        ),
        Primitive::new(
            "modulo",
            Type::procedure(vec![Type::Number, Type::Number], Type::Number),
        ),
        Primitive::new("gcd", Type::uniform_variadic(Type::Number, Type::Number)),
        Primitive::new("lcm", Type::uniform_variadic(Type::Number, Type::Number)),
        Primitive::new(
            "numerator",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new(
            "denominator",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new("floor", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("ceiling", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new(
            "truncate",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new("round", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new(
            "exact->inexact",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new(
            "inexact->exact",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new(
            "make-rectangular",
            Type::procedure(vec![Type::Number, Type::Number], Type::Number),
        ),
        Primitive::new(
            "make-polar",
            Type::procedure(vec![Type::Number, Type::Number], Type::Number),
        ),
        Primitive::new(
            "real-part",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new(
            "imag-part",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new(
            "magnitude",
            Type::procedure(vec![Type::Number], Type::Number),
        ),
        Primitive::new("angle", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("exp", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("log", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("sin", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("cos", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("tan", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("asin", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new("acos", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new(
            "atan",
            Type::optional_procedure(vec![Type::Number], vec![Type::Number], Type::Number),
        ),
        Primitive::new(
            "rationalize",
            Type::procedure(vec![Type::Number, Type::Number], Type::Number),
        ),
        Primitive::new("sqrt", Type::procedure(vec![Type::Number], Type::Number)),
        Primitive::new(
            "expt",
            Type::procedure(vec![Type::Number, Type::Number], Type::Number),
        ),
        Primitive::new(
            "char=?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char<?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char>?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char<=?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char>=?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char-ci=?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char-ci<?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char-ci>?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char-ci<=?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "char-ci>=?",
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean),
        ),
        Primitive::new(
            "string=?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string<?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string>?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string<=?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string>=?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string-ci=?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string-ci<?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string-ci>?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string-ci<=?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string-ci>=?",
            Type::rest_procedure(
                vec![Type::String, Type::String],
                Type::String,
                Type::Boolean,
            ),
        ),
        Primitive::new(
            "string-length",
            Type::procedure(vec![Type::String], Type::Number),
        ),
        Primitive::new(
            "make-string",
            Type::optional_procedure(vec![Type::Number], vec![Type::Char], Type::String),
        ),
        Primitive::new("string", Type::uniform_variadic(Type::Char, Type::String)),
        Primitive::new(
            "string-ref",
            Type::procedure(vec![Type::String, Type::Number], Type::Char),
        ),
        Primitive::new(
            "string-set!",
            Type::procedure(vec![Type::String, Type::Number, Type::Char], Type::Unknown),
        ),
        Primitive::new(
            "substring",
            Type::procedure(vec![Type::String, Type::Number, Type::Number], Type::String),
        ),
        Primitive::new(
            "string-append",
            Type::uniform_variadic(Type::String, Type::String),
        ),
        Primitive::new(
            "string->list",
            Type::procedure(vec![Type::String], Type::ListOf(Box::new(Type::Char))),
        ),
        Primitive::new(
            "list->string",
            Type::procedure(vec![Type::ListOf(Box::new(Type::Char))], Type::String),
        ),
        Primitive::new(
            "string-copy",
            Type::procedure(vec![Type::String], Type::String),
        ),
        Primitive::new(
            "string-fill!",
            Type::procedure(vec![Type::String, Type::Char], Type::Unknown),
        ),
        Primitive::new(
            "make-vector",
            Type::optional_procedure(
                vec![Type::Number],
                vec![a.clone()],
                Type::VectorOf(Box::new(a.clone())),
            ),
        ),
        Primitive::new(
            "vector",
            Type::uniform_variadic(a.clone(), Type::VectorOf(Box::new(a.clone()))),
        ),
        Primitive::new(
            "vector-length",
            Type::procedure(vec![Type::Vector], Type::Number),
        ),
        Primitive::new(
            "vector-ref",
            Type::procedure(
                vec![Type::VectorOf(Box::new(a.clone())), Type::Number],
                a.clone(),
            ),
        ),
        Primitive::new(
            "vector-set!",
            Type::procedure(
                vec![Type::VectorOf(Box::new(a.clone())), Type::Number, a.clone()],
                Type::Unknown,
            ),
        ),
        Primitive::new(
            "vector->list",
            Type::procedure(
                vec![Type::VectorOf(Box::new(a.clone()))],
                Type::ListOf(Box::new(a.clone())),
            ),
        ),
        Primitive::new(
            "list->vector",
            Type::procedure(
                vec![Type::ListOf(Box::new(a.clone()))],
                Type::VectorOf(Box::new(a.clone())),
            ),
        ),
        Primitive::new(
            "vector-fill!",
            Type::procedure(
                vec![Type::VectorOf(Box::new(a.clone())), a.clone()],
                Type::Unknown,
            ),
        ),
        Primitive::new(
            "cons",
            Type::procedure(
                vec![a.clone(), b.clone()],
                Type::Pair(Box::new(a.clone()), Box::new(b.clone())),
            ),
        ),
        Primitive::new(
            "car",
            Type::procedure(
                vec![Type::Pair(Box::new(a.clone()), Box::new(b.clone()))],
                a.clone(),
            ),
        ),
        Primitive::new(
            "cdr",
            Type::procedure(
                vec![Type::Pair(Box::new(a.clone()), Box::new(b.clone()))],
                b.clone(),
            ),
        ),
        Primitive::new(
            "set-car!",
            Type::procedure(
                vec![
                    Type::Pair(Box::new(Type::Any), Box::new(Type::Any)),
                    Type::Any,
                ],
                Type::Unknown,
            ),
        ),
        Primitive::new(
            "set-cdr!",
            Type::procedure(
                vec![
                    Type::Pair(Box::new(Type::Any), Box::new(Type::Any)),
                    Type::Any,
                ],
                Type::Unknown,
            ),
        ),
        Primitive::new(
            "list",
            Type::uniform_variadic(a.clone(), Type::ListOf(Box::new(a.clone()))),
        ),
        Primitive::new("length", Type::procedure(vec![Type::List], Type::Number)),
        Primitive::new(
            "reverse",
            Type::procedure(
                vec![Type::ListOf(Box::new(a.clone()))],
                Type::ListOf(Box::new(a.clone())),
            ),
        ),
        Primitive::new("append", Type::uniform_variadic(Type::Any, Type::Any)),
        Primitive::new(
            "list-ref",
            Type::procedure(
                vec![Type::ListOf(Box::new(a.clone())), Type::Number],
                a.clone(),
            ),
        ),
        Primitive::new(
            "list-tail",
            Type::procedure(
                vec![Type::ListOf(Box::new(a.clone())), Type::Number],
                Type::ListOf(Box::new(a.clone())),
            ),
        ),
        Primitive::new(
            "memq",
            Type::procedure(
                vec![Type::Any, Type::ListOf(Box::new(a.clone()))],
                Type::union(vec![Type::Boolean, Type::ListOf(Box::new(a.clone()))]),
            ),
        ),
        Primitive::new(
            "memv",
            Type::procedure(
                vec![Type::Any, Type::ListOf(Box::new(a.clone()))],
                Type::union(vec![Type::Boolean, Type::ListOf(Box::new(a.clone()))]),
            ),
        ),
        Primitive::new(
            "member",
            Type::procedure(
                vec![Type::Any, Type::ListOf(Box::new(a.clone()))],
                Type::union(vec![Type::Boolean, Type::ListOf(Box::new(a.clone()))]),
            ),
        ),
        Primitive::new(
            "assq",
            Type::procedure(
                vec![Type::Any, Type::ListOf(Box::new(a.clone()))],
                Type::union(vec![Type::Boolean, a.clone()]),
            ),
        ),
        Primitive::new(
            "assv",
            Type::procedure(
                vec![Type::Any, Type::ListOf(Box::new(a.clone()))],
                Type::union(vec![Type::Boolean, a.clone()]),
            ),
        ),
        Primitive::new(
            "assoc",
            Type::procedure(
                vec![Type::Any, Type::ListOf(Box::new(a.clone()))],
                Type::union(vec![Type::Boolean, a.clone()]),
            ),
        ),
        Primitive::new("map", map_signature()),
        Primitive::new("for-each", for_each_signature()),
    ];

    primitives.extend(composed_accessor_primitives());
    primitives
}

pub fn primitive(name: &str) -> Option<Primitive> {
    r5rs_primitives()
        .into_iter()
        .find(|primitive| primitive.name == name)
}

fn numeric_comparison() -> Type {
    Type::rest_procedure(
        vec![Type::Number, Type::Number],
        Type::Number,
        Type::Boolean,
    )
}

fn number_predicate() -> Type {
    Type::procedure(vec![Type::Number], Type::Boolean)
}

fn char_predicate() -> Type {
    Type::procedure(vec![Type::Char], Type::Boolean)
}

fn map_signature() -> Type {
    let element = Type::Var("a".to_string());
    let result = Type::Var("b".to_string());
    Type::rest_procedure(
        vec![
            Type::rest_procedure(vec![element.clone()], element.clone(), result.clone()),
            Type::ListOf(Box::new(element.clone())),
        ],
        Type::ListOf(Box::new(element)),
        Type::ListOf(Box::new(result)),
    )
}

fn for_each_signature() -> Type {
    let element = Type::Var("a".to_string());
    Type::rest_procedure(
        vec![
            Type::rest_procedure(vec![element.clone()], element.clone(), Type::Any),
            Type::ListOf(Box::new(element.clone())),
        ],
        Type::ListOf(Box::new(element)),
        Type::Unknown,
    )
}

fn composed_accessor_primitives() -> Vec<Primitive> {
    [
        "caar", "cadr", "cdar", "cddr", "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr",
        "cddar", "cdddr", "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar",
        "cadddr", "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
    ]
    .into_iter()
    .map(|name| Primitive::new(name, composed_accessor_signature(name)))
    .collect()
}

fn composed_accessor_signature(name: &str) -> Type {
    let result = Type::Var("a".to_string());
    let middle = name
        .strip_prefix('c')
        .and_then(|name| name.strip_suffix('r'))
        .expect("composed accessor table only contains c...r names");
    let input = middle.chars().fold(result.clone(), |input, op| match op {
        'a' => Type::Pair(Box::new(input), Box::new(Type::Any)),
        'd' => Type::Pair(Box::new(Type::Any), Box::new(input)),
        _ => unreachable!("composed accessor table only contains a and d"),
    });

    Type::procedure(vec![input], result)
}

#[cfg(test)]
mod tests {
    use super::primitive;
    use crate::types::Type;

    #[test]
    fn exposes_predicate_refinements() {
        let string_predicate = primitive("string?").unwrap();

        assert_eq!(string_predicate.signature.to_string(), "(-> any? boolean?)");
        assert_eq!(string_predicate.predicate.unwrap().positive, Type::String);
    }

    #[test]
    fn exposes_polymorphic_list_primitives() {
        assert_eq!(
            primitive("reverse").unwrap().signature.to_string(),
            "(-> (listof a) (listof a))"
        );
        assert_eq!(
            primitive("cons").unwrap().signature.to_string(),
            "(-> a b (pair? a b))"
        );
        assert_eq!(
            primitive("set-car!").unwrap().signature.to_string(),
            "(-> (pair? any? any?) any? unknown?)"
        );
        assert_eq!(
            primitive("length").unwrap().signature.to_string(),
            "(-> list? number?)"
        );
        assert_eq!(
            primitive("append").unwrap().signature.to_string(),
            "(->* any? any?)"
        );
        assert_eq!(
            primitive("cadddr").unwrap().signature.to_string(),
            "(-> (pair? any? (pair? any? (pair? any? (pair? a any?)))) a)"
        );
    }

    #[test]
    fn exposes_variadic_numeric_and_character_primitives() {
        assert_eq!(
            primitive("+").unwrap().signature.to_string(),
            "(->* number? number?)"
        );
        assert_eq!(
            primitive("integer?").unwrap().signature.to_string(),
            "(-> any? boolean?)"
        );
        assert_eq!(
            primitive("zero?").unwrap().signature.to_string(),
            "(-> number? boolean?)"
        );
        assert_eq!(
            primitive("max").unwrap().signature.to_string(),
            "(-> number? number? * number?)"
        );
        assert_eq!(
            primitive("quotient").unwrap().signature.to_string(),
            "(-> number? number? number?)"
        );
        assert_eq!(
            primitive("gcd").unwrap().signature.to_string(),
            "(->* number? number?)"
        );
        assert_eq!(
            primitive("floor").unwrap().signature.to_string(),
            "(-> number? number?)"
        );
        assert_eq!(
            primitive("exact->inexact").unwrap().signature.to_string(),
            "(-> number? number?)"
        );
        assert_eq!(
            primitive("make-rectangular").unwrap().signature.to_string(),
            "(-> number? number? number?)"
        );
        assert_eq!(
            primitive("make-polar").unwrap().signature.to_string(),
            "(-> number? number? number?)"
        );
        assert_eq!(
            primitive("sqrt").unwrap().signature.to_string(),
            "(-> number? number?)"
        );
        assert_eq!(
            primitive("magnitude").unwrap().signature.to_string(),
            "(-> number? number?)"
        );
        assert_eq!(
            primitive("angle").unwrap().signature.to_string(),
            "(-> number? number?)"
        );
        assert_eq!(
            primitive("exp").unwrap().signature.to_string(),
            "(-> number? number?)"
        );
        assert_eq!(
            primitive("atan").unwrap().signature.to_string(),
            "(-> number? number? ? number?)"
        );
        assert_eq!(
            primitive("rationalize").unwrap().signature.to_string(),
            "(-> number? number? number?)"
        );
        assert_eq!(
            primitive("expt").unwrap().signature.to_string(),
            "(-> number? number? number?)"
        );
        assert_eq!(
            primitive("eqv?").unwrap().signature.to_string(),
            "(-> any? any? boolean?)"
        );
        assert_eq!(
            primitive("equal?").unwrap().signature.to_string(),
            "(-> any? any? boolean?)"
        );
        assert_eq!(
            primitive("force").unwrap().signature.to_string(),
            "(-> (promiseof a) a)"
        );
        assert_eq!(
            primitive("values").unwrap().signature.to_string(),
            "(->* any? unknown?)"
        );
        assert_eq!(
            primitive("call-with-values").unwrap().signature.to_string(),
            "(-> any? any? any?)"
        );
        assert_eq!(
            primitive("apply").unwrap().signature.to_string(),
            "(-> any? any? any? * any?)"
        );
        assert_eq!(
            primitive("char=?").unwrap().signature.to_string(),
            "(-> char? char? char? * boolean?)"
        );
        assert_eq!(
            primitive("char-alphabetic?").unwrap().signature.to_string(),
            "(-> char? boolean?)"
        );
        assert_eq!(
            primitive("char-upcase").unwrap().signature.to_string(),
            "(-> char? char?)"
        );
        assert_eq!(
            primitive("char-ci<=?").unwrap().signature.to_string(),
            "(-> char? char? char? * boolean?)"
        );
        assert_eq!(
            primitive("string-ci>=?").unwrap().signature.to_string(),
            "(-> string? string? string? * boolean?)"
        );
    }

    #[test]
    fn exposes_string_primitives() {
        assert_eq!(
            primitive("make-string").unwrap().signature.to_string(),
            "(-> number? char? ? string?)"
        );
        assert_eq!(
            primitive("string").unwrap().signature.to_string(),
            "(->* char? string?)"
        );
        assert_eq!(
            primitive("string-ref").unwrap().signature.to_string(),
            "(-> string? number? char?)"
        );
        assert_eq!(
            primitive("string-set!").unwrap().signature.to_string(),
            "(-> string? number? char? unknown?)"
        );
        assert_eq!(
            primitive("string->list").unwrap().signature.to_string(),
            "(-> string? (listof char?))"
        );
        assert_eq!(
            primitive("list->string").unwrap().signature.to_string(),
            "(-> (listof char?) string?)"
        );
    }

    #[test]
    fn exposes_vector_primitives() {
        assert_eq!(
            primitive("vector").unwrap().signature.to_string(),
            "(->* a (vectorof a))"
        );
        assert_eq!(
            primitive("vector-ref").unwrap().signature.to_string(),
            "(-> (vectorof a) number? a)"
        );
        assert_eq!(
            primitive("vector-set!").unwrap().signature.to_string(),
            "(-> (vectorof a) number? a unknown?)"
        );
        assert_eq!(
            primitive("vector->list").unwrap().signature.to_string(),
            "(-> (vectorof a) (listof a))"
        );
        assert_eq!(
            primitive("list->vector").unwrap().signature.to_string(),
            "(-> (listof a) (vectorof a))"
        );
    }

    #[test]
    fn exposes_indexed_list_primitives() {
        assert_eq!(
            primitive("list-ref").unwrap().signature.to_string(),
            "(-> (listof a) number? a)"
        );
        assert_eq!(
            primitive("list-tail").unwrap().signature.to_string(),
            "(-> (listof a) number? (listof a))"
        );
    }

    #[test]
    fn exposes_membership_primitives() {
        assert_eq!(
            primitive("member").unwrap().signature.to_string(),
            "(-> any? (listof a) (U boolean? (listof a)))"
        );
        assert_eq!(
            primitive("assoc").unwrap().signature.to_string(),
            "(-> any? (listof a) (U boolean? a))"
        );
    }

    #[test]
    fn exposes_higher_order_list_primitives() {
        assert_eq!(
            primitive("map").unwrap().signature.to_string(),
            "(-> (-> a a * b) (listof a) (listof a) * (listof b))"
        );
        assert_eq!(
            primitive("for-each").unwrap().signature.to_string(),
            "(-> (-> a a * any?) (listof a) (listof a) * unknown?)"
        );
    }

    #[test]
    fn exposes_conversion_primitives() {
        assert_eq!(
            primitive("symbol->string").unwrap().signature.to_string(),
            "(-> symbol? string?)"
        );
        assert_eq!(
            primitive("string->number").unwrap().signature.to_string(),
            "(-> string? number? ? (U boolean? number?))"
        );
    }

    #[test]
    fn exposes_output_primitives() {
        assert_eq!(
            primitive("current-input-port")
                .unwrap()
                .signature
                .to_string(),
            "(-> input-port?)"
        );
        assert_eq!(
            primitive("read-char").unwrap().signature.to_string(),
            "(-> input-port? ? (U char? eof-object?))"
        );
        assert_eq!(
            primitive("read").unwrap().signature.to_string(),
            "(-> input-port? ? any?)"
        );
        assert_eq!(
            primitive("current-output-port")
                .unwrap()
                .signature
                .to_string(),
            "(-> output-port?)"
        );
        assert_eq!(
            primitive("write").unwrap().signature.to_string(),
            "(-> any? output-port? ? unknown?)"
        );
        assert_eq!(
            primitive("open-output-file").unwrap().signature.to_string(),
            "(-> string? output-port?)"
        );
        assert_eq!(
            primitive("call-with-input-file")
                .unwrap()
                .signature
                .to_string(),
            "(-> string? (-> input-port? a) a)"
        );
        assert_eq!(
            primitive("call-with-output-file")
                .unwrap()
                .signature
                .to_string(),
            "(-> string? (-> output-port? a) a)"
        );
        assert_eq!(
            primitive("with-input-from-file")
                .unwrap()
                .signature
                .to_string(),
            "(-> string? (-> a) a)"
        );
        assert_eq!(
            primitive("with-output-to-file")
                .unwrap()
                .signature
                .to_string(),
            "(-> string? (-> a) a)"
        );
        assert_eq!(
            primitive("load").unwrap().signature.to_string(),
            "(-> string? unknown?)"
        );
        assert_eq!(
            primitive("eval").unwrap().signature.to_string(),
            "(-> any? any? any?)"
        );
        assert_eq!(
            primitive("interaction-environment")
                .unwrap()
                .signature
                .to_string(),
            "(-> any?)"
        );
        assert_eq!(
            primitive("dynamic-wind").unwrap().signature.to_string(),
            "(-> (-> any?) (-> a) (-> any?) a)"
        );
        assert_eq!(
            primitive("call/cc").unwrap().signature.to_string(),
            "(-> (-> (-> a never?) b) (U a b))"
        );
        assert_eq!(
            primitive("call-with-current-continuation")
                .unwrap()
                .signature
                .to_string(),
            "(-> (-> (-> a never?) b) (U a b))"
        );
        assert_eq!(
            primitive("write-char").unwrap().signature.to_string(),
            "(-> char? output-port? ? unknown?)"
        );
        assert_eq!(
            primitive("transcript-on").unwrap().signature.to_string(),
            "(-> string? unknown?)"
        );
        assert_eq!(
            primitive("transcript-off").unwrap().signature.to_string(),
            "(-> unknown?)"
        );
    }
}
