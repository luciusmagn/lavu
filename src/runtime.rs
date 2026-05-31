use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use bigdecimal::BigDecimal;
use num::{BigInt, BigRational, Complex};
use thiserror::Error;

use crate::surface::{Expr, Program, TopLevel};
use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(BigInt),
    Rational(BigRational),
    Decimal(BigDecimal),
    Complex(Complex<BigDecimal>),
    Boolean(bool),
    Character(char),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Vector(Vec<Value>),
    Procedure(Rc<Procedure>),
    Primitive(&'static str),
    Unspecified,
    Uninitialized,
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

    #[error("variable used before initialization: {name}")]
    UninitializedVariable { name: String, span: SourceSpan },

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
            "/",
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
            "eqv?",
            "cons",
            "car",
            "cdr",
            "list",
            "reverse",
            "append",
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
        Expr::Variable(name) => match env.lookup(name) {
            Some(Value::Uninitialized) => Err(EvalError::UninitializedVariable {
                name: name.clone(),
                span: expr.span.clone(),
            }),
            Some(value) => Ok(value),
            None => Err(EvalError::UnboundVariable {
                name: name.clone(),
                span: expr.span.clone(),
            }),
        },
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
        Expr::LetRec { bindings, body } => eval_letrec(bindings, body, env),
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

fn eval_letrec(
    bindings: &[(Spanned<String>, Spanned<Expr>)],
    body: &[Spanned<Expr>],
    env: &Env,
) -> Result<Value, EvalError> {
    let local = Env::child(env.clone());
    for (name, _) in bindings {
        local.define(name.node.clone(), Value::Uninitialized);
    }

    for (name, value_expr) in bindings {
        let value = eval_expr(value_expr, &local)?;
        local.set(&name.node, value);
    }

    eval_sequence(body, &local)
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
        "/" => divide(args, span),
        "=" => numeric_compare(args, span, |a, b| a == b, |a, b| a == b),
        "<" => numeric_compare(args, span, |a, b| a < b, |a, b| a < b),
        ">" => numeric_compare(args, span, |a, b| a > b, |a, b| a > b),
        "<=" => numeric_compare(args, span, |a, b| a <= b, |a, b| a <= b),
        ">=" => numeric_compare(args, span, |a, b| a >= b, |a, b| a >= b),
        "boolean?" => predicate(args, span, |value| matches!(value, Value::Boolean(_))),
        "number?" => predicate(args, span, |value| {
            matches!(
                value,
                Value::Integer(_) | Value::Rational(_) | Value::Decimal(_) | Value::Complex(_)
            )
        }),
        "char?" => predicate(args, span, |value| matches!(value, Value::Character(_))),
        "string?" => predicate(args, span, |value| matches!(value, Value::String(_))),
        "symbol?" => predicate(args, span, |value| matches!(value, Value::Symbol(_))),
        "pair?" => predicate(args, span, |value| match value {
            Value::List(items) => !items.is_empty(),
            Value::Pair(_, _) => true,
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
        "eqv?" => eqv(args, span),
        "cons" => cons(args, span),
        "car" => unary(args, span.clone(), |value| car(value, span)),
        "cdr" => unary(args, span.clone(), |value| cdr(value, span)),
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
        "append" => append(args, span),
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

#[derive(Debug, Clone)]
enum NumberValue {
    Exact(BigRational),
    Decimal(BigDecimal),
    Complex(Complex<BigDecimal>),
}

fn add(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let numbers = numeric_args(args, span)?;

    if numbers.iter().any(NumberValue::is_complex) {
        return Ok(Value::Complex(
            numbers
                .iter()
                .map(NumberValue::to_complex)
                .fold(complex_zero(), |sum, number| sum + number),
        ));
    }

    if numbers.iter().any(NumberValue::is_decimal) {
        return Ok(Value::Decimal(
            numbers
                .iter()
                .map(NumberValue::to_decimal)
                .fold(decimal_zero(), |sum, number| sum + number),
        ));
    }

    Ok(exact_number(
        numbers
            .into_iter()
            .map(NumberValue::into_exact)
            .sum::<BigRational>(),
    ))
}

fn subtract(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let numbers = numeric_args(args, span.clone())?;
    if numbers.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual: 0,
            span,
        });
    }

    if numbers.iter().any(NumberValue::is_complex) {
        return Ok(Value::Complex(fold_subtract(
            numbers.iter().map(NumberValue::to_complex).collect(),
        )));
    }

    if numbers.iter().any(NumberValue::is_decimal) {
        return Ok(Value::Decimal(fold_subtract(
            numbers.iter().map(NumberValue::to_decimal).collect(),
        )));
    }

    Ok(exact_number(fold_subtract(
        numbers.into_iter().map(NumberValue::into_exact).collect(),
    )))
}

fn multiply(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let numbers = numeric_args(args, span)?;

    if numbers.iter().any(NumberValue::is_complex) {
        return Ok(Value::Complex(
            numbers
                .iter()
                .map(NumberValue::to_complex)
                .fold(complex_one(), |product, number| product * number),
        ));
    }

    if numbers.iter().any(NumberValue::is_decimal) {
        return Ok(Value::Decimal(
            numbers
                .iter()
                .map(NumberValue::to_decimal)
                .fold(decimal_one(), |product, number| product * number),
        ));
    }

    Ok(exact_number(
        numbers
            .into_iter()
            .map(NumberValue::into_exact)
            .fold(BigRational::from_integer(BigInt::from(1)), |product, n| {
                product * n
            }),
    ))
}

fn divide(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let numbers = numeric_args(args, span.clone())?;
    if numbers.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual: 0,
            span,
        });
    }

    if numbers.iter().any(NumberValue::is_complex) {
        return Ok(Value::Complex(fold_divide(
            numbers.iter().map(NumberValue::to_complex).collect(),
            complex_one(),
        )));
    }

    if numbers.iter().any(NumberValue::is_decimal) {
        return Ok(Value::Decimal(fold_divide(
            numbers.iter().map(NumberValue::to_decimal).collect(),
            decimal_one(),
        )));
    }

    Ok(exact_number(fold_divide(
        numbers.into_iter().map(NumberValue::into_exact).collect(),
        BigRational::from_integer(BigInt::from(1)),
    )))
}

fn numeric_compare(
    args: Vec<Value>,
    span: SourceSpan,
    exact_pred: impl Fn(&BigRational, &BigRational) -> bool,
    decimal_pred: impl Fn(&BigDecimal, &BigDecimal) -> bool,
) -> Result<Value, EvalError> {
    let numbers = numeric_args(args, span.clone())?;
    if numbers.len() < 2 {
        return Err(EvalError::ArityMismatch {
            expected: 2,
            actual: numbers.len(),
            span,
        });
    }

    if numbers.iter().any(NumberValue::is_complex) {
        return Err(EvalError::TypeError {
            expected: "real number?",
            span,
        });
    }

    if numbers.iter().any(NumberValue::is_decimal) {
        let numbers = numbers
            .iter()
            .map(NumberValue::to_decimal)
            .collect::<Vec<_>>();
        return Ok(Value::Boolean(
            numbers
                .windows(2)
                .all(|pair| decimal_pred(&pair[0], &pair[1])),
        ));
    }

    let numbers = numbers
        .into_iter()
        .map(NumberValue::into_exact)
        .collect::<Vec<_>>();
    Ok(Value::Boolean(
        numbers
            .windows(2)
            .all(|pair| exact_pred(&pair[0], &pair[1])),
    ))
}

fn numeric_args(args: Vec<Value>, span: SourceSpan) -> Result<Vec<NumberValue>, EvalError> {
    args.into_iter()
        .map(|value| match value {
            Value::Integer(n) => Ok(NumberValue::Exact(BigRational::from_integer(n))),
            Value::Rational(n) => Ok(NumberValue::Exact(n)),
            Value::Decimal(n) => Ok(NumberValue::Decimal(n)),
            Value::Complex(n) => Ok(NumberValue::Complex(n)),
            _ => Err(EvalError::TypeError {
                expected: "number?",
                span: span.clone(),
            }),
        })
        .collect()
}

impl NumberValue {
    fn is_decimal(&self) -> bool {
        matches!(self, Self::Decimal(_))
    }

    fn is_complex(&self) -> bool {
        matches!(self, Self::Complex(_))
    }

    fn into_exact(self) -> BigRational {
        match self {
            Self::Exact(n) => n,
            Self::Decimal(_) | Self::Complex(_) => {
                unreachable!("numeric promotion should handle inexact numbers first")
            }
        }
    }

    fn to_decimal(&self) -> BigDecimal {
        match self {
            Self::Exact(n) => rational_to_decimal(n),
            Self::Decimal(n) => n.clone(),
            Self::Complex(_) => unreachable!("complex numbers should promote above decimals"),
        }
    }

    fn to_complex(&self) -> Complex<BigDecimal> {
        match self {
            Self::Exact(n) => Complex::new(rational_to_decimal(n), decimal_zero()),
            Self::Decimal(n) => Complex::new(n.clone(), decimal_zero()),
            Self::Complex(n) => n.clone(),
        }
    }
}

fn fold_subtract<T>(numbers: Vec<T>) -> T
where
    T: std::ops::Neg<Output = T> + std::ops::Sub<Output = T>,
{
    let mut numbers = numbers.into_iter();
    let first = numbers
        .next()
        .expect("fold_subtract requires at least one number");
    if numbers.len() == 0 {
        -first
    } else {
        numbers.fold(first, |difference, n| difference - n)
    }
}

fn fold_divide<T>(numbers: Vec<T>, one: T) -> T
where
    T: std::ops::Div<Output = T>,
{
    let mut numbers = numbers.into_iter();
    let first = numbers
        .next()
        .expect("fold_divide requires at least one number");
    if numbers.len() == 0 {
        one / first
    } else {
        numbers.fold(first, |quotient, n| quotient / n)
    }
}

fn rational_to_decimal(number: &BigRational) -> BigDecimal {
    BigDecimal::from(number.numer().clone()) / BigDecimal::from(number.denom().clone())
}

fn decimal_zero() -> BigDecimal {
    BigDecimal::from(0)
}

fn decimal_one() -> BigDecimal {
    BigDecimal::from(1)
}

fn complex_zero() -> Complex<BigDecimal> {
    Complex::new(decimal_zero(), decimal_zero())
}

fn complex_one() -> Complex<BigDecimal> {
    Complex::new(decimal_one(), decimal_zero())
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

fn eqv(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [left, right]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span,
    })?;

    Ok(Value::Boolean(eqv_value(&left, &right)))
}

fn eqv_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Rational(left), Value::Rational(right)) => left == right,
        (Value::Integer(left), Value::Rational(right))
        | (Value::Rational(right), Value::Integer(left)) => {
            &BigRational::from_integer(left.clone()) == right
        }
        (Value::Decimal(left), Value::Decimal(right)) => left == right,
        (Value::Complex(left), Value::Complex(right)) => left == right,
        (Value::Character(left), Value::Character(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) if left.is_empty() && right.is_empty() => true,
        _ => false,
    }
}

fn cons(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [head, tail]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span,
    })?;

    Ok(cons_value(head, tail))
}

fn car(value: Value, span: SourceSpan) -> Result<Value, EvalError> {
    match value {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(head, _) => Ok(*head),
        _ => Err(EvalError::TypeError {
            expected: "pair?",
            span,
        }),
    }
}

fn cdr(value: Value, span: SourceSpan) -> Result<Value, EvalError> {
    match value {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(_, tail) => Ok(*tail),
        _ => Err(EvalError::TypeError {
            expected: "pair?",
            span,
        }),
    }
}

fn append(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for arg in args {
        match arg {
            Value::List(items) => result.extend(items),
            _ => {
                return Err(EvalError::TypeError {
                    expected: "list?",
                    span,
                });
            }
        }
    }

    Ok(Value::List(result))
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
        Datum::DottedList(items, tail) => {
            let tail = datum_to_value(tail)?;
            items
                .iter()
                .rev()
                .map(datum_to_value)
                .try_fold(tail, |tail, head| head.map(|head| cons_value(head, tail)))
        }
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
        Atom::Decimal(n) => Value::Decimal(n.clone()),
        Atom::Real(n, d) => Value::Rational(BigRational::new(n.clone(), d.clone())),
        Atom::Complex(n) => Value::Complex(n.clone()),
        Atom::String(text) => Value::String(text.clone()),
        Atom::Boolean(value) => Value::Boolean(*value),
        Atom::Character(value) => Value::Character(*value),
    }
}

fn truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

fn exact_number(number: BigRational) -> Value {
    if number.denom() == &BigInt::from(1) {
        Value::Integer(number.numer().clone())
    } else {
        Value::Rational(number)
    }
}

fn cons_value(head: Value, tail: Value) -> Value {
    match tail {
        Value::List(mut items) => {
            items.insert(0, head);
            Value::List(items)
        }
        tail => Value::Pair(Box::new(head), Box::new(tail)),
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Rational(n) => write!(f, "{}/{}", n.numer(), n.denom()),
            Value::Decimal(n) => write!(f, "{n}"),
            Value::Complex(n) => {
                let imaginary = n.im.to_string();
                if imaginary.starts_with('-') {
                    write!(f, "{}{}i", n.re, imaginary)
                } else {
                    write!(f, "{}+{}i", n.re, imaginary)
                }
            }
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
            Value::Pair(head, tail) => write!(f, "({head} . {tail})"),
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
            Value::Uninitialized => write!(f, "#<uninitialized>"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::datum_parser::parse;
    use crate::runtime::{Env, eval_program};
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
        assert_eq!(eval_one("(/ 1 2)"), "1/2");
        assert_eq!(eval_one("(+ 1/2 1/2)"), "1");
        assert_eq!(eval_one("(+ 1.5 2.25)"), "3.75");
        assert_eq!(eval_one("(+ 1+2i 3+4i)"), "4+6i");
        assert_eq!(eval_one("(= 2 2 2)"), "#t");
        assert_eq!(eval_one("(= 1/2 (/ 1 2))"), "#t");
        assert_eq!(eval_one("(< 1.5 2.5)"), "#t");
        assert_eq!(eval_one("(< 1 2 3)"), "#t");
    }

    #[test]
    fn treats_numeric_tower_literals_as_numbers() {
        assert_eq!(eval_one("(number? 1/2)"), "#t");
        assert_eq!(eval_one("(number? 1.5)"), "#t");
        assert_eq!(eval_one("1/2"), "1/2");
        assert_eq!(eval_one("1.5"), "1.5");
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
    fn evaluates_local_recursive_bindings() {
        assert_eq!(
            eval_one("(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))"),
            "120"
        );
    }

    #[test]
    fn evaluates_named_let_loops() {
        assert_eq!(
            eval_one("(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))"),
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
        assert_eq!(eval_one("(eqv? 'a 'a)"), "#t");
    }

    #[test]
    fn evaluates_pair_and_list_primitives() {
        assert_eq!(eval_one("(cons 1 (list 2 3))"), "(1 2 3)");
        assert_eq!(eval_one("(cons 1 2)"), "(1 . 2)");
        assert_eq!(eval_one("(car (list 1 2 3))"), "1");
        assert_eq!(eval_one("(cdr (list 1 2 3))"), "(2 3)");
        assert_eq!(eval_one("(append (list 1) (list 2 3))"), "(1 2 3)");
    }

    #[test]
    fn evaluates_quoted_dotted_lists() {
        assert_eq!(eval_one("'(1 . 2)"), "(1 . 2)");
        assert_eq!(eval_one("'(1 2 . ())"), "(1 2)");
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

    #[test]
    fn evaluates_desugared_case() {
        assert_eq!(eval_one("(case 'b ((a c) 10) ((b d) 20) (else 30))"), "20");
    }
}
