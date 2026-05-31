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
            "string-length",
            Type::procedure(vec![Type::String], Type::Number),
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
            primitive("char=?").unwrap().signature.to_string(),
            "(-> char? char? char? * boolean?)"
        );
    }
}
