use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Any,
    Unknown,
    Never,
    Boolean,
    Number,
    Char,
    String,
    Symbol,
    Pair(Box<Type>, Box<Type>),
    Null,
    List,
    ListOf(Box<Type>),
    Vector,
    Procedure(ProcedureType),
    Port,
    InputPort,
    OutputPort,
    EofObject,
    Values(Vec<Type>),
    Var(String),
    Union(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ProcedureType {
    Fixed {
        params: Vec<Type>,
        result: Box<Type>,
    },
    UniformVariadic {
        param: Box<Type>,
        result: Box<Type>,
    },
    Rest {
        required: Vec<Type>,
        rest: Box<Type>,
        result: Box<Type>,
    },
}

impl Type {
    pub fn procedure(params: impl Into<Vec<Type>>, result: Type) -> Self {
        Self::Procedure(ProcedureType::Fixed {
            params: params.into(),
            result: Box::new(result),
        })
    }

    pub fn uniform_variadic(param: Type, result: Type) -> Self {
        Self::Procedure(ProcedureType::UniformVariadic {
            param: Box::new(param),
            result: Box::new(result),
        })
    }

    pub fn rest_procedure(required: impl Into<Vec<Type>>, rest: Type, result: Type) -> Self {
        Self::Procedure(ProcedureType::Rest {
            required: required.into(),
            rest: Box::new(rest),
            result: Box::new(result),
        })
    }

    pub fn union(types: impl Into<Vec<Type>>) -> Self {
        let mut types = types.into();
        if types.iter().any(|ty| matches!(ty, Self::Any)) {
            return Self::Any;
        }
        types.retain(|ty| !matches!(ty, Self::Never));
        types.sort();
        types.dedup();

        match types.as_slice() {
            [] => Self::Never,
            [single] => single.clone(),
            _ => Self::Union(types),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Any => write!(f, "any?"),
            Type::Unknown => write!(f, "unknown?"),
            Type::Never => write!(f, "never?"),
            Type::Boolean => write!(f, "boolean?"),
            Type::Number => write!(f, "number?"),
            Type::Char => write!(f, "char?"),
            Type::String => write!(f, "string?"),
            Type::Symbol => write!(f, "symbol?"),
            Type::Pair(car, cdr) => write!(f, "(pair? {} {})", car, cdr),
            Type::Null => write!(f, "null?"),
            Type::List => write!(f, "list?"),
            Type::ListOf(element) => write!(f, "(listof {})", element),
            Type::Vector => write!(f, "vector?"),
            Type::Procedure(procedure) => write!(f, "{procedure}"),
            Type::Port => write!(f, "port?"),
            Type::InputPort => write!(f, "input-port?"),
            Type::OutputPort => write!(f, "output-port?"),
            Type::EofObject => write!(f, "eof-object?"),
            Type::Values(types) => {
                write_joined(f, "(values", types)?;
                write!(f, ")")
            }
            Type::Var(name) => write!(f, "{name}"),
            Type::Union(types) => {
                write_joined(f, "(U", types)?;
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for ProcedureType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProcedureType::Fixed { params, result } => {
                write_joined(f, "(->", params)?;
                write!(f, " {result})")
            }
            ProcedureType::UniformVariadic { param, result } => {
                write!(f, "(->* {param} {result})")
            }
            ProcedureType::Rest {
                required,
                rest,
                result,
            } => {
                write_joined(f, "(->", required)?;
                write!(f, " {rest} * {result})")
            }
        }
    }
}

fn write_joined(f: &mut fmt::Formatter<'_>, prefix: &str, types: &[Type]) -> fmt::Result {
    write!(f, "{prefix}")?;
    for ty in types {
        write!(f, " {ty}")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{ProcedureType, Type};

    #[test]
    fn displays_predicate_shaped_atoms() {
        assert_eq!(Type::Any.to_string(), "any?");
        assert_eq!(Type::Unknown.to_string(), "unknown?");
        assert_eq!(Type::Never.to_string(), "never?");
        assert_eq!(Type::Boolean.to_string(), "boolean?");
        assert_eq!(Type::Number.to_string(), "number?");
        assert_eq!(Type::Char.to_string(), "char?");
        assert_eq!(Type::String.to_string(), "string?");
        assert_eq!(Type::Symbol.to_string(), "symbol?");
        assert_eq!(Type::Null.to_string(), "null?");
        assert_eq!(Type::List.to_string(), "list?");
        assert_eq!(Type::Vector.to_string(), "vector?");
        assert_eq!(Type::Port.to_string(), "port?");
        assert_eq!(Type::InputPort.to_string(), "input-port?");
        assert_eq!(Type::OutputPort.to_string(), "output-port?");
        assert_eq!(Type::EofObject.to_string(), "eof-object?");
    }

    #[test]
    fn displays_values() {
        assert_eq!(
            Type::Values(vec![Type::Number, Type::String]).to_string(),
            "(values number? string?)"
        );
        assert_eq!(Type::Values(Vec::new()).to_string(), "(values)");
    }

    #[test]
    fn displays_structured_types() {
        assert_eq!(
            Type::Pair(Box::new(Type::Symbol), Box::new(Type::Number)).to_string(),
            "(pair? symbol? number?)"
        );
        assert_eq!(
            Type::ListOf(Box::new(Type::Number)).to_string(),
            "(listof number?)"
        );
    }

    #[test]
    fn displays_unions() {
        assert_eq!(
            Type::union(vec![Type::String, Type::Number]).to_string(),
            "(U number? string?)"
        );
        assert_eq!(
            Type::union(vec![Type::Never, Type::Char]).to_string(),
            "char?"
        );
        assert_eq!(Type::union(vec![Type::Any, Type::Char]).to_string(), "any?");
        assert_eq!(Type::union(Vec::new()).to_string(), "never?");
        assert_eq!(Type::union(vec![Type::Char]).to_string(), "char?");
    }

    #[test]
    fn displays_procedures() {
        assert_eq!(
            Type::procedure(vec![Type::Number], Type::Number).to_string(),
            "(-> number? number?)"
        );
        assert_eq!(
            Type::procedure(vec![Type::Char, Type::Char], Type::Boolean).to_string(),
            "(-> char? char? boolean?)"
        );
        assert_eq!(
            Type::uniform_variadic(Type::Number, Type::Number).to_string(),
            "(->* number? number?)"
        );
        assert_eq!(
            Type::rest_procedure(vec![Type::Char, Type::Char], Type::Char, Type::Boolean)
                .to_string(),
            "(-> char? char? char? * boolean?)"
        );
    }

    #[test]
    fn displays_polymorphic_procedures() {
        let a = Type::Var("a".to_string());
        let reverse = Type::Procedure(ProcedureType::Fixed {
            params: vec![Type::ListOf(Box::new(a.clone()))],
            result: Box::new(Type::ListOf(Box::new(a))),
        });

        assert_eq!(reverse.to_string(), "(-> (listof a) (listof a))");
    }
}
