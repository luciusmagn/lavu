use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use num::BigInt;
use thiserror::Error;

use crate::surface::{Expr, Program, TopLevel};
use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(BigInt),
    Boolean(bool),
    Character(char),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Vector(Vec<Value>),
    Procedure(Rc<Procedure>),
    Primitive(&'static str),
    Unspecified,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Procedure {
    params: Vec<String>,
    body: Vec<Spanned<Expr>>,
    env: Env,
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum EvalError {
    #[error("unbound variable: {name}")]
    UnboundVariable { name: String, span: SourceSpan },

    #[error("cannot apply non-procedure")]
    NotProcedure { span: SourceSpan },

    #[error("wrong number of arguments")]
    ArityMismatch {
        expected: usize,
        actual: usize,
        span: SourceSpan,
    },

    #[error("type error: expected {expected}")]
    TypeError {
        expected: &'static str,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Env(Rc<RefCell<Frame>>);

#[derive(Debug, Clone, PartialEq)]
struct Frame {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Env {
    pub fn new() -> Self {
        let env = Self(Rc::new(RefCell::new(Frame {
            bindings: HashMap::new(),
            parent: None,
        })));
        env.install_primitives();
        env
    }

    fn child(parent: Env) -> Self {
        Self(Rc::new(RefCell::new(Frame {
            bindings: HashMap::new(),
            parent: Some(parent),
        })))
    }

    pub fn define(&self, name: impl Into<String>, value: Value) {
        self.0.borrow_mut().bindings.insert(name.into(), value);
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
        let frame = self.0.borrow();
        frame
            .bindings
            .get(name)
            .cloned()
            .or_else(|| frame.parent.as_ref().and_then(|parent| parent.lookup(name)))
    }

    pub fn set(&self, name: &str, value: Value) -> bool {
        if self.0.borrow().bindings.contains_key(name) {
            self.0.borrow_mut().bindings.insert(name.to_string(), value);
            true
        } else {
            self.0
                .borrow()
                .parent
                .as_ref()
                .is_some_and(|parent| parent.set(name, value))
        }
    }

    fn install_primitives(&self) {
        for name in [
            "+",
            "-",
            "*",
            "=",
            "<",
            ">",
            "<=",
            ">=",
            "boolean?",
            "number?",
            "char?",
            "string?",
            "symbol?",
            "pair?",
            "null?",
            "list?",
            "vector?",
            "procedure?",
            "not",
            "list",
            "reverse",
            "string-length",
            "char=?",
        ] {
            self.define(name, Value::Primitive(name));
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

pub fn eval_program(program: &Program, env: &Env) -> Result<Vec<Value>, EvalError> {
    program
        .forms
        .iter()
        .map(|form| eval_top_level(form, env))
        .collect()
}

pub fn eval_top_level(form: &Spanned<TopLevel>, env: &Env) -> Result<Value, EvalError> {
    match &form.node {
        TopLevel::Define { name, value } => {
            let value = eval_expr(value, env)?;
            env.define(name.node.clone(), value);
            Ok(Value::Unspecified)
        }
        TopLevel::Expr(expr) => eval_expr(
            &Spanned {
                node: expr.clone(),
                span: form.span.clone(),
                origin: form.origin,
            },
            env,
        ),
    }
}

pub fn eval_expr(expr: &Spanned<Expr>, env: &Env) -> Result<Value, EvalError> {
    match &expr.node {
        Expr::Literal(atom) => Ok(atom_to_value(atom)),
        Expr::Variable(name) => env.lookup(name).ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
            span: expr.span.clone(),
        }),
        Expr::Quote(datum) => datum_to_value(datum),
        Expr::Lambda { params, body } => Ok(Value::Procedure(Rc::new(Procedure {
            params: params.iter().map(|param| param.node.clone()).collect(),
            body: body.to_vec(),
            env: env.clone(),
        }))),
        Expr::If {
            condition,
            consequent,
            alternate,
        } => {
            if truthy(&eval_expr(condition, env)?) {
                eval_expr(consequent, env)
            } else if let Some(alternate) = alternate {
                eval_expr(alternate, env)
            } else {
                Ok(Value::Unspecified)
            }
        }
        Expr::Begin(exprs) => eval_sequence(exprs, env),
        Expr::Set { name, value } => {
            let value = eval_expr(value, env)?;
            if env.set(&name.node, value) {
                Ok(Value::Unspecified)
            } else {
                Err(EvalError::UnboundVariable {
                    name: name.node.clone(),
                    span: name.span.clone(),
                })
            }
        }
        Expr::Apply { operator, operands } => {
            let procedure = eval_expr(operator, env)?;
            let args = operands
                .iter()
                .map(|operand| eval_expr(operand, env))
                .collect::<Result<Vec<_>, _>>()?;
            apply(procedure, args, expr.span.clone())
        }
    }
}

fn eval_sequence(exprs: &[Spanned<Expr>], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Unspecified;
    for expr in exprs {
        result = eval_expr(expr, env)?;
    }
    Ok(result)
}

fn apply(procedure: Value, args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    match procedure {
        Value::Primitive(name) => apply_primitive(name, args, span),
        Value::Procedure(procedure) => {
            if procedure.params.len() != args.len() {
                return Err(EvalError::ArityMismatch {
                    expected: procedure.params.len(),
                    actual: args.len(),
                    span,
                });
            }

            let env = Env::child(procedure.env.clone());
            for (name, value) in procedure.params.iter().zip(args) {
                env.define(name.clone(), value);
            }

            eval_sequence(&procedure.body, &env)
        }
        _ => Err(EvalError::NotProcedure { span }),
    }
}

fn apply_primitive(
    name: &'static str,
    args: Vec<Value>,
    span: SourceSpan,
) -> Result<Value, EvalError> {
    match name {
        "+" => add(args, span),
        "-" => subtract(args, span),
        "*" => multiply(args, span),
        "=" => numeric_compare(args, span, |a, b| a == b),
        "<" => numeric_compare(args, span, |a, b| a < b),
        ">" => numeric_compare(args, span, |a, b| a > b),
        "<=" => numeric_compare(args, span, |a, b| a <= b),
        ">=" => numeric_compare(args, span, |a, b| a >= b),
        "boolean?" => predicate(args, span, |value| matches!(value, Value::Boolean(_))),
        "number?" => predicate(args, span, |value| matches!(value, Value::Integer(_))),
        "char?" => predicate(args, span, |value| matches!(value, Value::Character(_))),
        "string?" => predicate(args, span, |value| matches!(value, Value::String(_))),
        "symbol?" => predicate(args, span, |value| matches!(value, Value::Symbol(_))),
        "pair?" => predicate(args, span, |value| match value {
            Value::List(items) => !items.is_empty(),
            _ => false,
        }),
        "null?" => predicate(
            args,
            span,
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        "list?" => predicate(args, span, |value| matches!(value, Value::List(_))),
        "vector?" => predicate(args, span, |value| matches!(value, Value::Vector(_))),
        "procedure?" => predicate(args, span, |value| {
            matches!(value, Value::Procedure(_) | Value::Primitive(_))
        }),
        "not" => unary(args, span, |value| Ok(Value::Boolean(!truthy(&value)))),
        "list" => Ok(Value::List(args)),
        "reverse" => unary(args, span.clone(), |value| match value {
            Value::List(mut items) => {
                items.reverse();
                Ok(Value::List(items))
            }
            _ => Err(EvalError::TypeError {
                expected: "list?",
                span,
            }),
        }),
        "string-length" => unary(args, span.clone(), |value| match value {
            Value::String(text) => Ok(Value::Integer(BigInt::from(text.chars().count()))),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span,
            }),
        }),
        "char=?" => char_eq(args, span),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
    }
}

fn add(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    Ok(Value::Integer(
        numeric_args(args, span)?.into_iter().sum::<BigInt>(),
    ))
}

fn subtract(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let mut numbers = numeric_args(args, span.clone())?.into_iter();
    let Some(first) = numbers.next() else {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual: 0,
            span,
        });
    };

    let result = if numbers.len() == 0 {
        -first
    } else {
        numbers.fold(first, |difference, n| difference - n)
    };

    Ok(Value::Integer(result))
}

fn multiply(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    Ok(Value::Integer(
        numeric_args(args, span)?
            .into_iter()
            .fold(BigInt::from(1), |product, n| product * n),
    ))
}

fn numeric_compare(
    args: Vec<Value>,
    span: SourceSpan,
    pred: impl Fn(&BigInt, &BigInt) -> bool,
) -> Result<Value, EvalError> {
    let numbers = numeric_args(args, span.clone())?;
    if numbers.len() < 2 {
        return Err(EvalError::ArityMismatch {
            expected: 2,
            actual: numbers.len(),
            span,
        });
    }

    Ok(Value::Boolean(
        numbers.windows(2).all(|pair| pred(&pair[0], &pair[1])),
    ))
}

fn numeric_args(args: Vec<Value>, span: SourceSpan) -> Result<Vec<BigInt>, EvalError> {
    args.into_iter()
        .map(|value| match value {
            Value::Integer(n) => Ok(n),
            _ => Err(EvalError::TypeError {
                expected: "number?",
                span: span.clone(),
            }),
        })
        .collect()
}

fn char_eq(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::ArityMismatch {
            expected: 2,
            actual: args.len(),
            span,
        });
    }

    let chars = args
        .into_iter()
        .map(|value| match value {
            Value::Character(c) => Ok(c),
            _ => Err(EvalError::TypeError {
                expected: "char?",
                span: span.clone(),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Boolean(
        chars.windows(2).all(|pair| pair[0] == pair[1]),
    ))
}

fn predicate(
    args: Vec<Value>,
    span: SourceSpan,
    pred: impl FnOnce(&Value) -> bool,
) -> Result<Value, EvalError> {
    unary(args, span, |value| Ok(Value::Boolean(pred(&value))))
}

fn unary(
    args: Vec<Value>,
    span: SourceSpan,
    f: impl FnOnce(Value) -> Result<Value, EvalError>,
) -> Result<Value, EvalError> {
    let actual = args.len();
    let [value]: [Value; 1] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 1,
        actual,
        span: span.clone(),
    })?;
    f(value)
}

fn datum_to_value(datum: &Spanned<Datum>) -> Result<Value, EvalError> {
    match &datum.node {
        Datum::Atom(atom) => Ok(match atom {
            Atom::Identifier(name) => Value::Symbol(name.clone()),
            _ => atom_to_value(atom),
        }),
        Datum::List(items) => items
            .iter()
            .map(datum_to_value)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::List),
        Datum::Vector(items) => items
            .iter()
            .map(datum_to_value)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Vector),
        Datum::Quote(inner) => datum_to_value(inner),
        Datum::Quasiquote(_) | Datum::Unquote(_) | Datum::UnquoteSplicing(_) => {
            Err(EvalError::TypeError {
                expected: "implemented quote datum",
                span: datum.span.clone(),
            })
        }
    }
}

fn atom_to_value(atom: &Atom) -> Value {
    match atom {
        Atom::Identifier(name) => Value::Symbol(name.clone()),
        Atom::Integer(n) => Value::Integer(n.clone()),
        Atom::Decimal(n) => Value::String(n.to_string()),
        Atom::Real(n, d) => Value::String(format!("{n}/{d}")),
        Atom::Complex(n) => Value::String(format!("{}+{}i", n.re, n.im)),
        Atom::String(text) => Value::String(text.clone()),
        Atom::Boolean(value) => Value::Boolean(*value),
        Atom::Character(value) => Value::Character(*value),
    }
}

fn truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(value) => write!(f, "{}", if *value { "#t" } else { "#f" }),
            Value::Character(' ') => write!(f, "#\\space"),
            Value::Character('\n') => write!(f, "#\\newline"),
            Value::Character(c) => write!(f, "#\\{c}"),
            Value::String(text) => write!(f, "\"{text}\""),
            Value::Symbol(name) => write!(f, "{name}"),
            Value::List(items) => {
                write!(f, "(")?;
                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Vector(items) => {
                write!(f, "#(")?;
                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Procedure(_) | Value::Primitive(_) => write!(f, "#<procedure>"),
            Value::Unspecified => write!(f, "#<unspecified>"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::datum_parser::parse;
    use crate::runtime::{eval_program, Env};
    use crate::surface::classify_program;

    fn eval_one(input: &str) -> String {
        let datums = parse(input).unwrap();
        let program = classify_program(&datums).unwrap();
        let env = Env::new();
        eval_program(&program, &env)
            .unwrap()
            .last()
            .unwrap()
            .to_string()
    }

    #[test]
    fn evaluates_primitive_arithmetic() {
        assert_eq!(eval_one("(+ 1 2 3)"), "6");
        assert_eq!(eval_one("(- 10 3 2)"), "5");
        assert_eq!(eval_one("(* 2 3 4)"), "24");
        assert_eq!(eval_one("(= 2 2 2)"), "#t");
        assert_eq!(eval_one("(< 1 2 3)"), "#t");
    }

    #[test]
    fn evaluates_lambda_application() {
        assert_eq!(eval_one("((lambda (x) (+ x 1)) 2)"), "3");
    }

    #[test]
    fn evaluates_define_procedure_shorthand() {
        assert_eq!(eval_one("(define (add1 x) (+ x 1)) (add1 4)"), "5");
    }

    #[test]
    fn evaluates_recursive_top_level_definitions() {
        assert_eq!(
            eval_one("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"),
            "120"
        );
    }

    #[test]
    fn evaluates_if_with_scheme_truthiness() {
        assert_eq!(eval_one("(if #f 1 2)"), "2");
        assert_eq!(eval_one("(if '() 1 2)"), "1");
    }

    #[test]
    fn evaluates_list_reverse_and_char_comparison() {
        assert_eq!(eval_one("(reverse (list 1 2 3))"), "(3 2 1)");
        assert_eq!(eval_one("(char=? #\\a #\\a)"), "#t");
    }

    #[test]
    fn evaluates_desugared_let_and_boolean_forms() {
        assert_eq!(eval_one("(let ((x 2)) (+ x 3))"), "5");
        assert_eq!(eval_one("(let* ((x 2) (y (+ x 3))) y)"), "5");
        assert_eq!(eval_one("(and #t 1)"), "1");
        assert_eq!(eval_one("(or #f 7)"), "7");
    }

    #[test]
    fn evaluates_desugared_cond() {
        assert_eq!(
            eval_one("(cond ((string? 1) 10) ((number? 1) 20) (else 30))"),
            "20"
        );
    }
}
