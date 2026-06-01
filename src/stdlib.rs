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

    vec![
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
        Primitive::new("force", Type::procedure(vec![Type::Any], Type::Any)),
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
            "number->string",
            Type::procedure(vec![Type::Number], Type::String),
        ),
        Primitive::new(
            "string->number",
            Type::procedure(
                vec![Type::String],
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
            "make-vector",
            Type::rest_procedure(vec![Type::Number], Type::Any, Type::Vector),
        ),
        Primitive::new("vector", Type::uniform_variadic(a.clone(), Type::Vector)),
        Primitive::new(
            "vector-length",
            Type::procedure(vec![Type::Vector], Type::Number),
        ),
        Primitive::new(
            "vector-ref",
            Type::procedure(vec![Type::Vector, Type::Number], Type::Any),
        ),
        Primitive::new(
            "vector-set!",
            Type::procedure(vec![Type::Vector, Type::Number, Type::Any], Type::Unknown),
        ),
        Primitive::new(
            "vector->list",
            Type::procedure(vec![Type::Vector], Type::List),
        ),
        Primitive::new(
            "list->vector",
            Type::procedure(vec![Type::List], Type::Vector),
        ),
        Primitive::new(
            "vector-fill!",
            Type::procedure(vec![Type::Vector, Type::Any], Type::Unknown),
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
            "list",
            Type::uniform_variadic(a.clone(), Type::ListOf(Box::new(a.clone()))),
        ),
        Primitive::new(
            "reverse",
            Type::procedure(
                vec![Type::ListOf(Box::new(a.clone()))],
                Type::ListOf(Box::new(a.clone())),
            ),
        ),
        Primitive::new(
            "append",
            Type::uniform_variadic(Type::ListOf(Box::new(a.clone())), Type::ListOf(Box::new(a))),
        ),
        Primitive::new(
            "list-ref",
            Type::procedure(vec![Type::List, Type::Number], Type::Any),
        ),
        Primitive::new(
            "list-tail",
            Type::procedure(vec![Type::List, Type::Number], Type::List),
        ),
        Primitive::new(
            "memq",
            Type::procedure(vec![Type::Any, Type::List], Type::Any),
        ),
        Primitive::new(
            "memv",
            Type::procedure(vec![Type::Any, Type::List], Type::Any),
        ),
        Primitive::new(
            "member",
            Type::procedure(vec![Type::Any, Type::List], Type::Any),
        ),
        Primitive::new(
            "assq",
            Type::procedure(vec![Type::Any, Type::List], Type::Any),
        ),
        Primitive::new(
            "assv",
            Type::procedure(vec![Type::Any, Type::List], Type::Any),
        ),
        Primitive::new(
            "assoc",
            Type::procedure(vec![Type::Any, Type::List], Type::Any),
        ),
        Primitive::new(
            "map",
            Type::rest_procedure(vec![Type::Any, Type::List], Type::List, Type::List),
        ),
        Primitive::new(
            "for-each",
            Type::rest_procedure(vec![Type::Any, Type::List], Type::List, Type::Unknown),
        ),
    ]
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
            primitive("eqv?").unwrap().signature.to_string(),
            "(-> any? any? boolean?)"
        );
        assert_eq!(
            primitive("equal?").unwrap().signature.to_string(),
            "(-> any? any? boolean?)"
        );
        assert_eq!(
            primitive("force").unwrap().signature.to_string(),
            "(-> any? any?)"
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
    fn exposes_vector_primitives() {
        assert_eq!(
            primitive("vector").unwrap().signature.to_string(),
            "(->* a vector?)"
        );
        assert_eq!(
            primitive("vector-ref").unwrap().signature.to_string(),
            "(-> vector? number? any?)"
        );
        assert_eq!(
            primitive("vector-set!").unwrap().signature.to_string(),
            "(-> vector? number? any? unknown?)"
        );
    }

    #[test]
    fn exposes_indexed_list_primitives() {
        assert_eq!(
            primitive("list-ref").unwrap().signature.to_string(),
            "(-> list? number? any?)"
        );
        assert_eq!(
            primitive("list-tail").unwrap().signature.to_string(),
            "(-> list? number? list?)"
        );
    }

    #[test]
    fn exposes_membership_primitives() {
        assert_eq!(
            primitive("member").unwrap().signature.to_string(),
            "(-> any? list? any?)"
        );
        assert_eq!(
            primitive("assoc").unwrap().signature.to_string(),
            "(-> any? list? any?)"
        );
    }

    #[test]
    fn exposes_higher_order_list_primitives() {
        assert_eq!(
            primitive("map").unwrap().signature.to_string(),
            "(-> any? list? list? * list?)"
        );
        assert_eq!(
            primitive("for-each").unwrap().signature.to_string(),
            "(-> any? list? list? * unknown?)"
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
            "(-> string? (U boolean? number?))"
        );
    }
}
