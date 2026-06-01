use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use bigdecimal::BigDecimal;
use num::{BigInt, BigRational, Complex, ToPrimitive};
use thiserror::Error;

use crate::lexer::{Token, tokenize};
use crate::surface::{Expr, Program, TopLevel, classify_expr};
use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Clone)]
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
    Vector(Rc<RefCell<Vec<Value>>>),
    Procedure(Rc<Procedure>),
    Primitive(&'static str),
    Promise(Rc<Promise>),
    Unspecified,
    Uninitialized,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => left == right,
            (Value::Rational(left), Value::Rational(right)) => left == right,
            (Value::Decimal(left), Value::Decimal(right)) => left == right,
            (Value::Complex(left), Value::Complex(right)) => left == right,
            (Value::Boolean(left), Value::Boolean(right)) => left == right,
            (Value::Character(left), Value::Character(right)) => left == right,
            (Value::String(left), Value::String(right)) => left == right,
            (Value::Symbol(left), Value::Symbol(right)) => left == right,
            (Value::List(left), Value::List(right)) => left == right,
            (Value::Pair(left_car, left_cdr), Value::Pair(right_car, right_cdr)) => {
                left_car == right_car && left_cdr == right_cdr
            }
            (Value::Vector(left), Value::Vector(right)) => *left.borrow() == *right.borrow(),
            (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
            (Value::Primitive(left), Value::Primitive(right)) => left == right,
            (Value::Promise(left), Value::Promise(right)) => Rc::ptr_eq(left, right),
            (Value::Unspecified, Value::Unspecified)
            | (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Procedure {
    params: Vec<String>,
    body: Vec<Spanned<Expr>>,
    env: Env,
}

#[derive(Debug)]
pub struct Promise {
    expr: Spanned<Expr>,
    env: Env,
    value: RefCell<Option<Value>>,
}

impl Promise {
    fn new(expr: Spanned<Expr>, env: Env) -> Self {
        Self {
            expr,
            env,
            value: RefCell::new(None),
        }
    }

    fn force(&self) -> Result<Value, EvalError> {
        if let Some(value) = self.value.borrow().clone() {
            return Ok(value);
        }

        let value = eval_expr(&self.expr, &self.env)?;
        *self.value.borrow_mut() = Some(value.clone());
        Ok(value)
    }
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
            "make-vector",
            "vector",
            "vector-length",
            "vector-ref",
            "vector-set!",
            "vector->list",
            "list->vector",
            "vector-fill!",
            "procedure?",
            "port?",
            "input-port?",
            "output-port?",
            "eof-object?",
            "not",
            "eqv?",
            "eq?",
            "equal?",
            "force",
            "apply",
            "symbol->string",
            "string->symbol",
            "char->integer",
            "integer->char",
            "number->string",
            "string->number",
            "cons",
            "car",
            "cdr",
            "list",
            "reverse",
            "append",
            "list-ref",
            "list-tail",
            "memq",
            "memv",
            "member",
            "assq",
            "assv",
            "assoc",
            "map",
            "for-each",
            "string-length",
            "char=?",
            "char<?",
            "char>?",
            "string=?",
            "string<?",
            "string>?",
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
        Expr::Quasiquote(datum) => eval_quasiquote(datum, env, 0),
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
        Expr::Delay(expr) => Ok(Value::Promise(Rc::new(Promise::new(
            expr.as_ref().clone(),
            env.clone(),
        )))),
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
        "make-vector" => make_vector(args, span),
        "vector" => Ok(Value::Vector(Rc::new(RefCell::new(args)))),
        "vector-length" => unary(args, span.clone(), |value| match value {
            Value::Vector(items) => Ok(Value::Integer(BigInt::from(items.borrow().len()))),
            _ => Err(EvalError::TypeError {
                expected: "vector?",
                span,
            }),
        }),
        "vector-ref" => vector_ref(args, span),
        "vector-set!" => vector_set(args, span),
        "vector->list" => unary(args, span.clone(), |value| match value {
            Value::Vector(items) => Ok(Value::List(items.borrow().clone())),
            _ => Err(EvalError::TypeError {
                expected: "vector?",
                span,
            }),
        }),
        "list->vector" => unary(args, span.clone(), |value| match value {
            Value::List(items) => Ok(Value::Vector(Rc::new(RefCell::new(items)))),
            _ => Err(EvalError::TypeError {
                expected: "list?",
                span,
            }),
        }),
        "vector-fill!" => vector_fill(args, span),
        "procedure?" => predicate(args, span, |value| {
            matches!(value, Value::Procedure(_) | Value::Primitive(_))
        }),
        "port?" | "input-port?" | "output-port?" | "eof-object?" => {
            predicate(args, span, |_| false)
        }
        "not" => unary(args, span, |value| Ok(Value::Boolean(!truthy(&value)))),
        "eqv?" => eqv(args, span),
        "eq?" => eq(args, span),
        "equal?" => equal(args, span),
        "force" => force(args, span),
        "apply" => apply_procedure_argument(args, span),
        "symbol->string" => unary(args, span.clone(), |value| match value {
            Value::Symbol(name) => Ok(Value::String(name)),
            _ => Err(EvalError::TypeError {
                expected: "symbol?",
                span,
            }),
        }),
        "string->symbol" => unary(args, span.clone(), |value| match value {
            Value::String(text) => Ok(Value::Symbol(text)),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span,
            }),
        }),
        "char->integer" => unary(args, span.clone(), |value| match value {
            Value::Character(ch) => Ok(Value::Integer(BigInt::from(ch as u32))),
            _ => Err(EvalError::TypeError {
                expected: "char?",
                span,
            }),
        }),
        "integer->char" => unary(args, span.clone(), |value| match value {
            Value::Integer(n) => integer_to_char(n, span),
            _ => Err(EvalError::TypeError {
                expected: "integer?",
                span,
            }),
        }),
        "number->string" => unary(args, span.clone(), |value| match value {
            Value::Integer(_) | Value::Rational(_) | Value::Decimal(_) | Value::Complex(_) => {
                Ok(Value::String(value.to_string()))
            }
            _ => Err(EvalError::TypeError {
                expected: "number?",
                span,
            }),
        }),
        "string->number" => unary(args, span.clone(), |value| match value {
            Value::String(text) => Ok(string_to_number(&text)),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span,
            }),
        }),
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
        "list-ref" => list_ref(args, span),
        "list-tail" => list_tail(args, span),
        "memq" => member(args, span, eq_value),
        "memv" => member(args, span, eqv_value),
        "member" => member(args, span, equal_value),
        "assq" => assoc(args, span, eq_value),
        "assv" => assoc(args, span, eqv_value),
        "assoc" => assoc(args, span, equal_value),
        "map" => map_list(args, span),
        "for-each" => for_each(args, span),
        "string-length" => unary(args, span.clone(), |value| match value {
            Value::String(text) => Ok(Value::Integer(BigInt::from(text.chars().count()))),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span,
            }),
        }),
        "char=?" => char_eq(args, span),
        "char<?" => char_compare(args, span, |left, right| left < right),
        "char>?" => char_compare(args, span, |left, right| left > right),
        "string=?" => string_compare(args, span, |left, right| left == right),
        "string<?" => string_compare(args, span, |left, right| left < right),
        "string>?" => string_compare(args, span, |left, right| left > right),
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
    char_compare(args, span, |left, right| left == right)
}

fn char_compare(
    args: Vec<Value>,
    span: SourceSpan,
    pred: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
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
        chars.windows(2).all(|pair| pred(pair[0], pair[1])),
    ))
}

fn string_compare(
    args: Vec<Value>,
    span: SourceSpan,
    pred: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::ArityMismatch {
            expected: 2,
            actual: args.len(),
            span,
        });
    }

    let strings = args
        .into_iter()
        .map(|value| match value {
            Value::String(text) => Ok(text),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span: span.clone(),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Boolean(
        strings
            .windows(2)
            .all(|pair| pred(pair[0].as_str(), pair[1].as_str())),
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

fn eq(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [left, right]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span,
    })?;

    Ok(Value::Boolean(eq_value(&left, &right)))
}

fn equal(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [left, right]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span,
    })?;

    Ok(Value::Boolean(equal_value(&left, &right)))
}

fn force(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Promise(promise) => promise.force(),
        _ => Err(EvalError::TypeError {
            expected: "promise?",
            span,
        }),
    })
}

fn apply_procedure_argument(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::ArityMismatch {
            expected: 2,
            actual: args.len(),
            span,
        });
    }

    let mut args = args.into_iter();
    let procedure = args
        .next()
        .expect("arity check ensures a procedure argument");
    let mut operands = args.collect::<Vec<_>>();
    let final_operand = operands
        .pop()
        .expect("arity check ensures a final list argument");
    let Value::List(final_operands) = final_operand else {
        return Err(EvalError::TypeError {
            expected: "list?",
            span,
        });
    };

    operands.extend(final_operands);
    apply(procedure, operands, span)
}

fn integer_to_char(n: BigInt, span: SourceSpan) -> Result<Value, EvalError> {
    let Some(codepoint) = n.to_u32() else {
        return Err(EvalError::TypeError {
            expected: "Unicode scalar value?",
            span,
        });
    };
    let Some(ch) = char::from_u32(codepoint) else {
        return Err(EvalError::TypeError {
            expected: "Unicode scalar value?",
            span,
        });
    };

    Ok(Value::Character(ch))
}

fn string_to_number(text: &str) -> Value {
    let tokens = tokenize(text)
        .into_iter()
        .filter(|(token, _, _)| !matches!(token, Token::Whitespace(_) | Token::LineComment))
        .map(|(token, _, _)| token)
        .collect::<Vec<_>>();

    let [token]: [Token; 1] = match tokens.try_into() {
        Ok(tokens) => tokens,
        Err(_) => return Value::Boolean(false),
    };

    match token {
        Token::Integer(n)
        | Token::Binary(n)
        | Token::Octal(n)
        | Token::Hex(n)
        | Token::DecInteger(n) => Value::Integer(n),
        Token::Real((numerator, denominator)) => {
            Value::Rational(BigRational::new(numerator, denominator))
        }
        Token::Decimal(n) => Value::Decimal(n),
        Token::Complex(n) => Value::Complex(n),
        _ => Value::Boolean(false),
    }
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

fn eq_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Promise(left), Value::Promise(right)) => Rc::ptr_eq(left, right),
        _ => eqv_value(left, right),
    }
}

fn equal_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::List(left), Value::List(right)) => {
            left.iter()
                .zip(right)
                .all(|(left, right)| equal_value(left, right))
                && left.len() == right.len()
        }
        (Value::Pair(left_car, left_cdr), Value::Pair(right_car, right_cdr)) => {
            equal_value(left_car, right_car) && equal_value(left_cdr, right_cdr)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let left = left.borrow();
            let right = right.borrow();
            left.iter()
                .zip(right.iter())
                .all(|(left, right)| equal_value(left, right))
                && left.len() == right.len()
        }
        _ => eqv_value(left, right),
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

fn list_ref(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [list, index]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let index = exact_nonnegative_integer(&index, span.clone())?;

    match list {
        Value::List(items) => items.get(index).cloned().ok_or(EvalError::TypeError {
            expected: "valid list index",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "list?",
            span,
        }),
    }
}

fn list_tail(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [list, index]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let index = exact_nonnegative_integer(&index, span.clone())?;

    match list {
        Value::List(items) if index <= items.len() => Ok(Value::List(items[index..].to_vec())),
        Value::List(_) => Err(EvalError::TypeError {
            expected: "valid list index",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "list?",
            span,
        }),
    }
}

fn member(
    args: Vec<Value>,
    span: SourceSpan,
    compare: impl Fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    let actual = args.len();
    let [target, list]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;

    match list {
        Value::List(items) => Ok(items
            .iter()
            .position(|item| compare(&target, item))
            .map(|index| Value::List(items[index..].to_vec()))
            .unwrap_or(Value::Boolean(false))),
        _ => Err(EvalError::TypeError {
            expected: "list?",
            span,
        }),
    }
}

fn assoc(
    args: Vec<Value>,
    span: SourceSpan,
    compare: impl Fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    let actual = args.len();
    let [target, alist]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;

    let Value::List(entries) = alist else {
        return Err(EvalError::TypeError {
            expected: "list?",
            span,
        });
    };

    for entry in entries {
        match &entry {
            Value::List(items) => {
                let Some(key) = items.first() else {
                    return Err(EvalError::TypeError {
                        expected: "pair?",
                        span,
                    });
                };
                if compare(&target, key) {
                    return Ok(entry);
                }
            }
            Value::Pair(key, _) if compare(&target, key) => return Ok(entry),
            Value::Pair(_, _) => {}
            _ => {
                return Err(EvalError::TypeError {
                    expected: "pair?",
                    span,
                });
            }
        }
    }

    Ok(Value::Boolean(false))
}

fn map_list(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let (procedure, lists) = procedure_and_lists(args, span.clone())?;
    let len = common_list_len(&lists, span.clone())?;
    let mut results = Vec::with_capacity(len);

    for index in 0..len {
        let operands = lists
            .iter()
            .map(|items| items[index].clone())
            .collect::<Vec<_>>();
        results.push(apply(procedure.clone(), operands, span.clone())?);
    }

    Ok(Value::List(results))
}

fn for_each(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let (procedure, lists) = procedure_and_lists(args, span.clone())?;
    let len = common_list_len(&lists, span.clone())?;

    for index in 0..len {
        let operands = lists
            .iter()
            .map(|items| items[index].clone())
            .collect::<Vec<_>>();
        apply(procedure.clone(), operands, span.clone())?;
    }

    Ok(Value::Unspecified)
}

fn procedure_and_lists(
    args: Vec<Value>,
    span: SourceSpan,
) -> Result<(Value, Vec<Vec<Value>>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::ArityMismatch {
            expected: 2,
            actual: args.len(),
            span,
        });
    }

    let mut args = args.into_iter();
    let procedure = args.next().expect("arity check ensures procedure argument");
    let lists = args
        .map(|value| match value {
            Value::List(items) => Ok(items),
            _ => Err(EvalError::TypeError {
                expected: "list?",
                span: span.clone(),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok((procedure, lists))
}

fn common_list_len(lists: &[Vec<Value>], span: SourceSpan) -> Result<usize, EvalError> {
    let len = lists.first().map_or(0, Vec::len);
    if lists.iter().any(|items| items.len() != len) {
        return Err(EvalError::TypeError {
            expected: "lists of equal length",
            span,
        });
    }

    Ok(len)
}

fn make_vector(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual: args.len(),
            span,
        });
    }

    let len = exact_nonnegative_integer(&args[0], span.clone())?;
    let fill = args.get(1).cloned().unwrap_or(Value::Unspecified);
    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
}

fn vector_ref(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [vector, index]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let index = exact_nonnegative_integer(&index, span.clone())?;

    match vector {
        Value::Vector(items) => items
            .borrow()
            .get(index)
            .cloned()
            .ok_or(EvalError::TypeError {
                expected: "valid vector index",
                span,
            }),
        _ => Err(EvalError::TypeError {
            expected: "vector?",
            span,
        }),
    }
}

fn vector_set(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [vector, index, value]: [Value; 3] =
        args.try_into().map_err(|_| EvalError::ArityMismatch {
            expected: 3,
            actual,
            span: span.clone(),
        })?;
    let index = exact_nonnegative_integer(&index, span.clone())?;

    match vector {
        Value::Vector(items) => {
            let mut items = items.borrow_mut();
            let Some(slot) = items.get_mut(index) else {
                return Err(EvalError::TypeError {
                    expected: "valid vector index",
                    span,
                });
            };
            *slot = value;
            Ok(Value::Unspecified)
        }
        _ => Err(EvalError::TypeError {
            expected: "vector?",
            span,
        }),
    }
}

fn vector_fill(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [vector, value]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;

    match vector {
        Value::Vector(items) => {
            items.borrow_mut().fill(value);
            Ok(Value::Unspecified)
        }
        _ => Err(EvalError::TypeError {
            expected: "vector?",
            span,
        }),
    }
}

fn exact_nonnegative_integer(value: &Value, span: SourceSpan) -> Result<usize, EvalError> {
    let Value::Integer(index) = value else {
        return Err(EvalError::TypeError {
            expected: "non-negative integer?",
            span,
        });
    };
    if index < &BigInt::from(0) {
        return Err(EvalError::TypeError {
            expected: "non-negative integer?",
            span,
        });
    }

    index.to_usize().ok_or(EvalError::TypeError {
        expected: "fixnum index?",
        span,
    })
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
            .map(|items| Value::Vector(Rc::new(RefCell::new(items)))),
        Datum::Quote(inner) => abbreviation_to_value("quote", inner),
        Datum::Quasiquote(inner) => abbreviation_to_value("quasiquote", inner),
        Datum::Unquote(inner) => abbreviation_to_value("unquote", inner),
        Datum::UnquoteSplicing(inner) => abbreviation_to_value("unquote-splicing", inner),
    }
}

fn abbreviation_to_value(name: &'static str, datum: &Spanned<Datum>) -> Result<Value, EvalError> {
    Ok(Value::List(vec![
        Value::Symbol(name.to_string()),
        datum_to_value(datum)?,
    ]))
}

fn eval_quasiquote(datum: &Spanned<Datum>, env: &Env, level: usize) -> Result<Value, EvalError> {
    match &datum.node {
        Datum::Unquote(inner) if level == 0 => eval_unquoted(inner, env),
        Datum::Unquote(inner) => Ok(Value::List(vec![
            Value::Symbol("unquote".to_string()),
            eval_quasiquote(inner, env, level - 1)?,
        ])),
        Datum::UnquoteSplicing(_) if level == 0 => Err(EvalError::TypeError {
            expected: "unquote-splicing inside quasiquote list",
            span: datum.span.clone(),
        }),
        Datum::UnquoteSplicing(inner) => Ok(Value::List(vec![
            Value::Symbol("unquote-splicing".to_string()),
            eval_quasiquote(inner, env, level - 1)?,
        ])),
        Datum::Quasiquote(inner) => Ok(Value::List(vec![
            Value::Symbol("quasiquote".to_string()),
            eval_quasiquote(inner, env, level + 1)?,
        ])),
        Datum::List(items) => eval_quasiquote_list(items, None, env, level),
        Datum::DottedList(items, tail) => {
            eval_quasiquote_list(items, Some(tail.as_ref()), env, level)
        }
        Datum::Vector(items) => items
            .iter()
            .map(|item| eval_quasiquote(item, env, level))
            .collect::<Result<Vec<_>, _>>()
            .map(|items| Value::Vector(Rc::new(RefCell::new(items)))),
        Datum::Atom(_) | Datum::Quote(_) => datum_to_value(datum),
    }
}

fn eval_quasiquote_list(
    items: &[Spanned<Datum>],
    tail: Option<&Spanned<Datum>>,
    env: &Env,
    level: usize,
) -> Result<Value, EvalError> {
    let mut values = Vec::new();

    for item in items {
        match &item.node {
            Datum::UnquoteSplicing(inner) if level == 0 => match eval_unquoted(inner, env)? {
                Value::List(spliced) => values.extend(spliced),
                _ => {
                    return Err(EvalError::TypeError {
                        expected: "list?",
                        span: inner.span.clone(),
                    });
                }
            },
            _ => values.push(eval_quasiquote(item, env, level)?),
        }
    }

    match tail {
        Some(tail) => {
            let tail = eval_quasiquote(tail, env, level)?;
            Ok(values
                .into_iter()
                .rev()
                .fold(tail, |tail, head| cons_value(head, tail)))
        }
        None => Ok(Value::List(values)),
    }
}

fn eval_unquoted(datum: &Spanned<Datum>, env: &Env) -> Result<Value, EvalError> {
    let expr = classify_expr(datum).map_err(|_| EvalError::TypeError {
        expected: "valid unquote expression",
        span: datum.span.clone(),
    })?;

    eval_expr(&expr, env)
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
                for (index, item) in items.borrow().iter().enumerate() {
                    if index > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Promise(_) => write!(f, "#<promise>"),
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
    fn evaluates_do_loops() {
        assert_eq!(
            eval_one("(do ((i 0 (+ i 1)) (acc 0 (+ acc i))) ((= i 5) acc))"),
            "10"
        );
    }

    #[test]
    fn evaluates_delay_and_force() {
        assert_eq!(eval_one("(force (delay (+ 1 2)))"), "3");
        assert_eq!(
            eval_one(
                "(define x 0) (define p (delay (begin (set! x (+ x 1)) x))) (force p) (force p)"
            ),
            "1"
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
        assert_eq!(eval_one("(char<? #\\a #\\b)"), "#t");
        assert_eq!(eval_one("(eqv? 'a 'a)"), "#t");
    }

    #[test]
    fn evaluates_equality_predicates() {
        assert_eq!(eval_one("(eq? 'a 'a)"), "#t");
        assert_eq!(eval_one("(equal? '(1 (2)) '(1 (2)))"), "#t");
        assert_eq!(eval_one("(equal? (vector 1 2) (vector 1 2))"), "#t");
        assert_eq!(eval_one("(eq? (vector 1 2) (vector 1 2))"), "#f");
        assert_eq!(eval_one("(define v (vector 1 2)) (eq? v v)"), "#t");
    }

    #[test]
    fn evaluates_string_comparisons() {
        assert_eq!(eval_one("(string=? \"a\" \"a\")"), "#t");
        assert_eq!(eval_one("(string<? \"a\" \"b\")"), "#t");
        assert_eq!(eval_one("(string>? \"b\" \"a\")"), "#t");
    }

    #[test]
    fn evaluates_unimplemented_resource_predicates_conservatively() {
        assert_eq!(eval_one("(port? 1)"), "#f");
        assert_eq!(eval_one("(input-port? 1)"), "#f");
        assert_eq!(eval_one("(output-port? 1)"), "#f");
        assert_eq!(eval_one("(eof-object? 1)"), "#f");
    }

    #[test]
    fn evaluates_conversion_primitives() {
        assert_eq!(eval_one("(symbol->string 'hello)"), "\"hello\"");
        assert_eq!(eval_one("(string->symbol \"hello\")"), "hello");
        assert_eq!(eval_one("(char->integer #\\A)"), "65");
        assert_eq!(eval_one("(integer->char 65)"), "#\\A");
        assert_eq!(eval_one("(number->string 1/2)"), "\"1/2\"");
        assert_eq!(eval_one("(string->number \"#x10\")"), "16");
        assert_eq!(eval_one("(string->number \"wat\")"), "#f");
    }

    #[test]
    fn evaluates_vector_primitives() {
        assert_eq!(eval_one("(vector 1 2 3)"), "#(1 2 3)");
        assert_eq!(eval_one("(vector-length (vector 1 2 3))"), "3");
        assert_eq!(eval_one("(vector-ref (vector 1 2 3) 1)"), "2");
        assert_eq!(eval_one("(make-vector 3 'x)"), "#(x x x)");
        assert_eq!(eval_one("(vector->list (vector 1 2))"), "(1 2)");
        assert_eq!(eval_one("(list->vector (list 1 2))"), "#(1 2)");
    }

    #[test]
    fn evaluates_vector_mutation_by_reference() {
        assert_eq!(
            eval_one("(define v (vector 1 2)) (vector-set! v 0 9) (vector->list v)"),
            "(9 2)"
        );
        assert_eq!(
            eval_one("(define v (vector 1 2)) (vector-fill! v 'x) (vector->list v)"),
            "(x x)"
        );
    }

    #[test]
    fn evaluates_pair_and_list_primitives() {
        assert_eq!(eval_one("(cons 1 (list 2 3))"), "(1 2 3)");
        assert_eq!(eval_one("(cons 1 2)"), "(1 . 2)");
        assert_eq!(eval_one("(car (list 1 2 3))"), "1");
        assert_eq!(eval_one("(cdr (list 1 2 3))"), "(2 3)");
        assert_eq!(eval_one("(append (list 1) (list 2 3))"), "(1 2 3)");
        assert_eq!(eval_one("(list-ref (list 'a 'b 'c) 1)"), "b");
        assert_eq!(eval_one("(list-tail (list 'a 'b 'c) 1)"), "(b c)");
    }

    #[test]
    fn evaluates_membership_primitives() {
        assert_eq!(eval_one("(memq 'b '(a b c))"), "(b c)");
        assert_eq!(eval_one("(memq 'x '(a b c))"), "#f");
        assert_eq!(eval_one("(member '(1) '((0) (1) (2)))"), "((1) (2))");
    }

    #[test]
    fn evaluates_association_primitives() {
        assert_eq!(eval_one("(assq 'b '((a 1) (b 2)))"), "(b 2)");
        assert_eq!(eval_one("(assq 'x '((a 1) (b 2)))"), "#f");
        assert_eq!(eval_one("(assoc '(b) '(((a) 1) ((b) 2)))"), "((b) 2)");
    }

    #[test]
    fn evaluates_higher_order_list_iteration() {
        assert_eq!(eval_one("(map (lambda (x) (+ x 1)) '(1 2 3))"), "(2 3 4)");
        assert_eq!(eval_one("(map + '(1 2) '(10 20))"), "(11 22)");
        assert_eq!(
            eval_one("(define x 0) (for-each (lambda (n) (set! x (+ x n))) '(1 2 3)) x"),
            "6"
        );
    }

    #[test]
    fn evaluates_apply() {
        assert_eq!(eval_one("(apply + (list 1 2 3))"), "6");
        assert_eq!(eval_one("(apply + 1 2 (list 3 4))"), "10");
        assert_eq!(eval_one("(apply (lambda (x y) (+ x y)) (list 4 5))"), "9");
    }

    #[test]
    fn evaluates_quoted_dotted_lists() {
        assert_eq!(eval_one("'(1 . 2)"), "(1 . 2)");
        assert_eq!(eval_one("'(1 2 . ())"), "(1 2)");
        assert_eq!(eval_one("''a"), "(quote a)");
    }

    #[test]
    fn evaluates_quasiquote() {
        assert_eq!(eval_one("`(1 ,(+ 1 2) 4)"), "(1 3 4)");
        assert_eq!(eval_one("`(a ,@(list 1 2) b)"), "(a 1 2 b)");
        assert_eq!(eval_one("`(1 . ,(+ 1 1))"), "(1 . 2)");
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
