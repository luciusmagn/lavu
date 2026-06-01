use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::rc::Rc;

use bigdecimal::{BigDecimal, RoundingMode};
use num::{
    BigInt, BigRational, Complex, FromPrimitive, Integer, Num, One, Signed, ToPrimitive, Zero,
};
use thiserror::Error;

use crate::datum_parser::parse as parse_datums;
use crate::lexer::{Token, tokenize};
use crate::surface::{Expr, Program, TopLevel, classify_expr, classify_program};
use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Clone)]
pub enum Value {
    Integer(BigInt),
    Rational(BigRational),
    Decimal(BigDecimal),
    Complex(Complex<BigDecimal>),
    Boolean(bool),
    Character(char),
    String(Rc<RefCell<String>>),
    Symbol(String),
    List(Vec<Value>),
    Pair(Rc<RefCell<PairValue>>),
    Vector(Rc<RefCell<Vec<Value>>>),
    InputPort(InputPort),
    OutputPort(OutputPort),
    Procedure(Rc<Procedure>),
    Primitive(&'static str),
    Promise(Rc<Promise>),
    Continuation(Continuation),
    Environment(Env),
    Values(Vec<Value>),
    EofObject,
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
            (Value::String(left), Value::String(right)) => *left.borrow() == *right.borrow(),
            (Value::Symbol(left), Value::Symbol(right)) => left == right,
            (Value::List(left), Value::List(right)) => left == right,
            (Value::Pair(left), Value::Pair(right)) => {
                let left = left.borrow();
                let right = right.borrow();
                left.car == right.car && left.cdr == right.cdr
            }
            (Value::Vector(left), Value::Vector(right)) => *left.borrow() == *right.borrow(),
            (Value::InputPort(left), Value::InputPort(right)) => left == right,
            (Value::OutputPort(left), Value::OutputPort(right)) => left == right,
            (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
            (Value::Primitive(left), Value::Primitive(right)) => left == right,
            (Value::Promise(left), Value::Promise(right)) => Rc::ptr_eq(left, right),
            (Value::Continuation(left), Value::Continuation(right)) => left == right,
            (Value::Environment(left), Value::Environment(right)) => Rc::ptr_eq(&left.0, &right.0),
            (Value::Values(left), Value::Values(right)) => left == right,
            (Value::EofObject, Value::EofObject)
            | (Value::Unspecified, Value::Unspecified)
            | (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct InputPort(Rc<RefCell<InputPortState>>);

impl InputPort {
    fn stdin() -> Self {
        Self(Rc::new(RefCell::new(InputPortState {
            kind: InputPortKind::Stdin,
            chars: Vec::new(),
            index: 0,
            closed: false,
        })))
    }

    fn from_string(text: String) -> Self {
        Self(Rc::new(RefCell::new(InputPortState {
            kind: InputPortKind::Buffer,
            chars: text.chars().collect(),
            index: 0,
            closed: false,
        })))
    }
}

impl PartialEq for InputPort {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug)]
struct InputPortState {
    kind: InputPortKind,
    chars: Vec<char>,
    index: usize,
    closed: bool,
}

#[derive(Debug)]
enum InputPortKind {
    Stdin,
    Buffer,
}

#[derive(Debug, Clone)]
pub enum OutputPort {
    Stdout,
    File(Rc<RefCell<OutputFilePort>>),
    #[cfg(test)]
    Buffer(Rc<RefCell<String>>),
}

impl PartialEq for OutputPort {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Stdout, Self::Stdout) => true,
            (Self::File(left), Self::File(right)) => Rc::ptr_eq(left, right),
            #[cfg(test)]
            (Self::Buffer(left), Self::Buffer(right)) => Rc::ptr_eq(left, right),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct OutputFilePort {
    file: fs::File,
    closed: bool,
}

thread_local! {
    static CURRENT_INPUT_PORT: RefCell<InputPort> = RefCell::new(InputPort::stdin());
    static CURRENT_OUTPUT_PORT: RefCell<OutputPort> = const { RefCell::new(OutputPort::Stdout) };
    static TRANSCRIPT_PORT: RefCell<Option<OutputPort>> = const { RefCell::new(None) };
    static NEXT_CONTINUATION_ID: Cell<usize> = const { Cell::new(0) };
}

#[derive(Debug, Clone, PartialEq)]
pub struct PairValue {
    car: Value,
    cdr: Value,
}

type PairPointer = *const RefCell<PairValue>;
type VectorPointer = *const RefCell<Vec<Value>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Procedure {
    params: Vec<String>,
    rest: Option<String>,
    body: Vec<Spanned<Expr>>,
    env: Env,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Continuation {
    id: usize,
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

    #[error("wrong number of arguments: expected {expected}, got {actual}")]
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

    #[error("I/O error: {message}")]
    IoError { message: String, span: SourceSpan },

    #[error("read error: {message}")]
    ReadError { message: String, span: SourceSpan },

    #[error("continuation used outside its dynamic extent")]
    ContinuationJump {
        id: usize,
        value: Box<Value>,
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
    pub fn empty() -> Self {
        Self(Rc::new(RefCell::new(Frame {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    pub fn new() -> Self {
        let env = Self::empty();
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
            "complex?",
            "real?",
            "rational?",
            "integer?",
            "exact?",
            "inexact?",
            "zero?",
            "positive?",
            "negative?",
            "odd?",
            "even?",
            "max",
            "min",
            "abs",
            "quotient",
            "remainder",
            "modulo",
            "gcd",
            "lcm",
            "numerator",
            "denominator",
            "floor",
            "ceiling",
            "truncate",
            "round",
            "exact->inexact",
            "inexact->exact",
            "make-rectangular",
            "make-polar",
            "real-part",
            "imag-part",
            "magnitude",
            "angle",
            "exp",
            "log",
            "sin",
            "cos",
            "tan",
            "asin",
            "acos",
            "atan",
            "rationalize",
            "sqrt",
            "expt",
            "char?",
            "char-alphabetic?",
            "char-numeric?",
            "char-whitespace?",
            "char-upper-case?",
            "char-lower-case?",
            "char-upcase",
            "char-downcase",
            "string?",
            "make-string",
            "string",
            "string-ref",
            "string-set!",
            "substring",
            "string-append",
            "string->list",
            "list->string",
            "string-copy",
            "string-fill!",
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
            "current-input-port",
            "current-output-port",
            "open-input-file",
            "open-output-file",
            "call-with-input-file",
            "call-with-output-file",
            "with-input-from-file",
            "with-output-to-file",
            "load",
            "eval",
            "scheme-report-environment",
            "null-environment",
            "interaction-environment",
            "dynamic-wind",
            "call-with-current-continuation",
            "call/cc",
            "close-input-port",
            "close-output-port",
            "read",
            "read-char",
            "peek-char",
            "char-ready?",
            "write",
            "display",
            "newline",
            "write-char",
            "transcript-on",
            "transcript-off",
            "eof-object?",
            "not",
            "eqv?",
            "eq?",
            "equal?",
            "force",
            "values",
            "call-with-values",
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
            "set-car!",
            "set-cdr!",
            "caar",
            "cadr",
            "cdar",
            "cddr",
            "caaar",
            "caadr",
            "cadar",
            "caddr",
            "cdaar",
            "cdadr",
            "cddar",
            "cdddr",
            "caaaar",
            "caaadr",
            "caadar",
            "caaddr",
            "cadaar",
            "cadadr",
            "caddar",
            "cadddr",
            "cdaaar",
            "cdaadr",
            "cdadar",
            "cdaddr",
            "cddaar",
            "cddadr",
            "cdddar",
            "cddddr",
            "list",
            "length",
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
            "char<=?",
            "char>=?",
            "char-ci=?",
            "char-ci<?",
            "char-ci>?",
            "char-ci<=?",
            "char-ci>=?",
            "string=?",
            "string<?",
            "string>?",
            "string<=?",
            "string>=?",
            "string-ci=?",
            "string-ci<?",
            "string-ci>?",
            "string-ci<=?",
            "string-ci>=?",
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
        Expr::Lambda { params, rest, body } => Ok(Value::Procedure(Rc::new(Procedure {
            params: params.iter().map(|param| param.node.clone()).collect(),
            rest: rest.as_ref().map(|param| param.node.clone()),
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
        Expr::LetRec { bindings, body } => {
            let local = eval_letrec_bindings(bindings, env)?;
            eval_sequence(body, &local)
        }
        Expr::Apply { operator, operands } => {
            let procedure = eval_expr(operator, env)?;
            let args = operands
                .iter()
                .map(|operand| eval_expr(operand, env))
                .collect::<Result<Vec<_>, _>>()?;
            match procedure {
                Value::Primitive("load") => load(args, expr.span.clone(), env),
                Value::Primitive("interaction-environment") => {
                    interaction_environment(args, expr.span.clone(), env)
                }
                procedure => apply(procedure, args, expr.span.clone()),
            }
        }
    }
}

fn eval_tail_expr(mut expr: Spanned<Expr>, mut env: Env) -> Result<Value, EvalError> {
    loop {
        match expr.node {
            Expr::Literal(atom) => return Ok(atom_to_value(&atom)),
            Expr::Variable(name) => {
                return match env.lookup(&name) {
                    Some(Value::Uninitialized) => Err(EvalError::UninitializedVariable {
                        name,
                        span: expr.span,
                    }),
                    Some(value) => Ok(value),
                    None => Err(EvalError::UnboundVariable {
                        name,
                        span: expr.span,
                    }),
                };
            }
            Expr::Quote(datum) => return datum_to_value(&datum),
            Expr::Quasiquote(datum) => return eval_quasiquote(&datum, &env, 0),
            Expr::Lambda { params, rest, body } => {
                return Ok(Value::Procedure(Rc::new(Procedure {
                    params: params.into_iter().map(|param| param.node).collect(),
                    rest: rest.map(|param| param.node),
                    body,
                    env,
                })));
            }
            Expr::If {
                condition,
                consequent,
                alternate,
            } => {
                if truthy(&eval_expr(&condition, &env)?) {
                    expr = *consequent;
                } else if let Some(alternate) = alternate {
                    expr = *alternate;
                } else {
                    return Ok(Value::Unspecified);
                }
            }
            Expr::Begin(exprs) => {
                if let Some(next) = eval_sequence_prefix(exprs, &env)? {
                    expr = next;
                } else {
                    return Ok(Value::Unspecified);
                }
            }
            Expr::Set { name, value } => {
                let value = eval_expr(&value, &env)?;
                if env.set(&name.node, value) {
                    return Ok(Value::Unspecified);
                }
                return Err(EvalError::UnboundVariable {
                    name: name.node,
                    span: name.span,
                });
            }
            Expr::Delay(delayed) => {
                return Ok(Value::Promise(Rc::new(Promise::new(*delayed, env))));
            }
            Expr::LetRec { bindings, body } => {
                env = eval_letrec_bindings(&bindings, &env)?;
                if let Some(next) = eval_sequence_prefix(body, &env)? {
                    expr = next;
                } else {
                    return Ok(Value::Unspecified);
                }
            }
            Expr::Apply { operator, operands } => {
                let span = expr.span;
                let procedure = eval_expr(&operator, &env)?;
                let args = operands
                    .iter()
                    .map(|operand| eval_expr(operand, &env))
                    .collect::<Result<Vec<_>, _>>()?;
                match procedure {
                    Value::Primitive("load") => return load(args, span, &env),
                    Value::Primitive("interaction-environment") => {
                        return interaction_environment(args, span, &env);
                    }
                    Value::Procedure(procedure) => {
                        env = procedure_application_env(&procedure, args, span)?;
                        if let Some(next) = eval_sequence_prefix(procedure.body.clone(), &env)? {
                            expr = next;
                        } else {
                            return Ok(Value::Unspecified);
                        }
                    }
                    procedure => return apply(procedure, args, span),
                }
            }
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

fn eval_tail_sequence(exprs: &[Spanned<Expr>], env: &Env) -> Result<Value, EvalError> {
    match exprs.split_last() {
        Some((last, prefix)) => {
            for expr in prefix {
                eval_expr(expr, env)?;
            }
            eval_tail_expr(last.clone(), env.clone())
        }
        None => Ok(Value::Unspecified),
    }
}

fn eval_sequence_prefix(
    mut exprs: Vec<Spanned<Expr>>,
    env: &Env,
) -> Result<Option<Spanned<Expr>>, EvalError> {
    let Some(last) = exprs.pop() else {
        return Ok(None);
    };
    for expr in &exprs {
        eval_expr(expr, env)?;
    }
    Ok(Some(last))
}

fn eval_letrec_bindings(
    bindings: &[(Spanned<String>, Spanned<Expr>)],
    env: &Env,
) -> Result<Env, EvalError> {
    let local = Env::child(env.clone());
    for (name, _) in bindings {
        local.define(name.node.clone(), Value::Uninitialized);
    }

    for (name, value_expr) in bindings {
        let value = eval_expr(value_expr, &local)?;
        local.set(&name.node, value);
    }

    Ok(local)
}

fn apply(procedure: Value, args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    match procedure {
        Value::Primitive(name) => apply_primitive(name, args, span),
        Value::Continuation(continuation) => apply_continuation(continuation, args, span),
        Value::Procedure(procedure) => {
            let env = procedure_application_env(&procedure, args, span)?;
            eval_tail_sequence(&procedure.body, &env)
        }
        _ => Err(EvalError::NotProcedure { span }),
    }
}

fn procedure_application_env(
    procedure: &Procedure,
    args: Vec<Value>,
    span: SourceSpan,
) -> Result<Env, EvalError> {
    if procedure.rest.is_none() && procedure.params.len() != args.len() {
        return Err(EvalError::ArityMismatch {
            expected: procedure.params.len(),
            actual: args.len(),
            span,
        });
    }
    if procedure.rest.is_some() && args.len() < procedure.params.len() {
        return Err(EvalError::ArityMismatch {
            expected: procedure.params.len(),
            actual: args.len(),
            span,
        });
    }

    let env = Env::child(procedure.env.clone());
    for (name, value) in procedure.params.iter().zip(args.iter()) {
        env.define(name.clone(), value.clone());
    }
    if let Some(rest) = &procedure.rest {
        env.define(
            rest.clone(),
            list_value(args[procedure.params.len()..].to_vec()),
        );
    }

    Ok(env)
}

fn is_procedure(value: &Value) -> bool {
    matches!(
        value,
        Value::Primitive(_) | Value::Continuation(_) | Value::Procedure(_)
    )
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
        "complex?" => predicate(args, span, number_value),
        "real?" => predicate(args, span, real_number_value),
        "rational?" => predicate(args, span, rational_number_value),
        "integer?" => predicate(args, span, integer_number_value),
        "exact?" => numeric_predicate(args, span, exact_number_value),
        "inexact?" => numeric_predicate(args, span, inexact_number_value),
        "zero?" => numeric_predicate(args, span, zero_number_value),
        "positive?" => numeric_predicate(args, span, positive_number_value),
        "negative?" => numeric_predicate(args, span, negative_number_value),
        "odd?" => numeric_predicate(args, span, odd_number_value),
        "even?" => numeric_predicate(args, span, even_number_value),
        "max" => numeric_extreme(
            args,
            span,
            |left, right| left > right,
            |left, right| left > right,
        ),
        "min" => numeric_extreme(
            args,
            span,
            |left, right| left < right,
            |left, right| left < right,
        ),
        "abs" => numeric_abs(args, span),
        "quotient" => {
            exact_integer_binary(args, span, "non-zero integer?", |left, right| left / right)
        }
        "remainder" => {
            exact_integer_binary(args, span, "non-zero integer?", |left, right| left % right)
        }
        "modulo" => exact_integer_binary(args, span, "non-zero integer?", modulo_value),
        "gcd" => exact_integer_fold(args, span, BigInt::zero(), |left, right| left.gcd(&right)),
        "lcm" => exact_integer_fold(args, span, BigInt::one(), |left, right| left.lcm(&right)),
        "numerator" => numerator(args, span),
        "denominator" => denominator(args, span),
        "floor" => numeric_round(args, span, BigRational::floor, RoundingMode::Floor),
        "ceiling" => numeric_round(args, span, BigRational::ceil, RoundingMode::Ceiling),
        "truncate" => numeric_round(args, span, BigRational::trunc, RoundingMode::Down),
        "round" => numeric_round(args, span, round_rational_half_even, RoundingMode::HalfEven),
        "exact->inexact" => exact_to_inexact(args, span),
        "inexact->exact" => inexact_to_exact(args, span),
        "make-rectangular" => make_rectangular(args, span),
        "make-polar" => make_polar(args, span),
        "real-part" => real_part(args, span),
        "imag-part" => imag_part(args, span),
        "magnitude" => magnitude(args, span),
        "angle" => angle(args, span),
        "exp" => complex_unary(args, span, |number| number.exp()),
        "log" => complex_unary(args, span, |number| number.ln()),
        "sin" => complex_unary(args, span, |number| number.sin()),
        "cos" => complex_unary(args, span, |number| number.cos()),
        "tan" => complex_unary(args, span, |number| number.tan()),
        "asin" => complex_unary(args, span, |number| number.asin()),
        "acos" => complex_unary(args, span, |number| number.acos()),
        "atan" => numeric_atan(args, span),
        "rationalize" => rationalize(args, span),
        "sqrt" => numeric_sqrt(args, span),
        "expt" => numeric_expt(args, span),
        "char?" => predicate(args, span, |value| matches!(value, Value::Character(_))),
        "char-alphabetic?" => char_predicate(args, span, char::is_alphabetic),
        "char-numeric?" => char_predicate(args, span, char::is_numeric),
        "char-whitespace?" => char_predicate(args, span, char::is_whitespace),
        "char-upper-case?" => char_predicate(args, span, char::is_uppercase),
        "char-lower-case?" => char_predicate(args, span, char::is_lowercase),
        "char-upcase" => char_map(args, span, |c| c.to_uppercase().next().unwrap_or(c)),
        "char-downcase" => char_map(args, span, |c| c.to_lowercase().next().unwrap_or(c)),
        "string?" => predicate(args, span, |value| matches!(value, Value::String(_))),
        "make-string" => make_string(args, span),
        "string" => string(args, span),
        "string-ref" => string_ref(args, span),
        "string-set!" => string_set(args, span),
        "substring" => substring(args, span),
        "string-append" => string_append(args, span),
        "string->list" => string_to_list(args, span),
        "list->string" => list_to_string(args, span),
        "string-copy" => unary(args, span.clone(), |value| match value {
            Value::String(text) => Ok(string_value(text.borrow().clone())),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span,
            }),
        }),
        "string-fill!" => string_fill(args, span),
        "symbol?" => predicate(args, span, |value| matches!(value, Value::Symbol(_))),
        "pair?" => predicate(args, span, |value| match value {
            Value::List(items) => !items.is_empty(),
            Value::Pair(_) => true,
            _ => false,
        }),
        "null?" => predicate(args, span, is_empty_list),
        "list?" => predicate(args, span, is_proper_list),
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
            Value::Vector(items) => Ok(list_value(items.borrow().clone())),
            _ => Err(EvalError::TypeError {
                expected: "vector?",
                span,
            }),
        }),
        "list->vector" => unary(args, span.clone(), |value| {
            expect_list_items(&value, span.clone())
                .map(|items| Value::Vector(Rc::new(RefCell::new(items))))
        }),
        "vector-fill!" => vector_fill(args, span),
        "procedure?" => predicate(args, span, |value| {
            matches!(
                value,
                Value::Procedure(_) | Value::Primitive(_) | Value::Continuation(_)
            )
        }),
        "port?" => predicate(args, span, |value| {
            matches!(value, Value::InputPort(_) | Value::OutputPort(_))
        }),
        "input-port?" => predicate(args, span, |value| matches!(value, Value::InputPort(_))),
        "output-port?" => predicate(args, span, |value| matches!(value, Value::OutputPort(_))),
        "current-input-port" => current_input_port(args, span),
        "current-output-port" => current_output_port(args, span),
        "open-input-file" => open_input_file(args, span),
        "open-output-file" => open_output_file(args, span),
        "call-with-input-file" => call_with_input_file(args, span),
        "call-with-output-file" => call_with_output_file(args, span),
        "with-input-from-file" => with_input_from_file(args, span),
        "with-output-to-file" => with_output_to_file(args, span),
        "load" => Err(EvalError::TypeError {
            expected: "direct load call?",
            span,
        }),
        "eval" => eval_value(args, span),
        "scheme-report-environment" => scheme_report_environment(args, span),
        "null-environment" => null_environment(args, span),
        "interaction-environment" => Err(EvalError::TypeError {
            expected: "direct interaction-environment call?",
            span,
        }),
        "dynamic-wind" => dynamic_wind(args, span),
        "call-with-current-continuation" | "call/cc" => call_cc(args, span),
        "close-input-port" => close_input_port(args, span),
        "close-output-port" => close_output_port(args, span),
        "read" => read_datum(args, span),
        "read-char" => read_char(args, span),
        "peek-char" => peek_char(args, span),
        "char-ready?" => char_ready(args, span),
        "write" => output_value(args, span, OutputMode::Write),
        "display" => output_value(args, span, OutputMode::Display),
        "newline" => newline(args, span),
        "write-char" => write_char(args, span),
        "transcript-on" => transcript_on(args, span),
        "transcript-off" => transcript_off(args, span),
        "eof-object?" => predicate(args, span, |value| matches!(value, Value::EofObject)),
        "not" => unary(args, span, |value| Ok(Value::Boolean(!truthy(&value)))),
        "eqv?" => eqv(args, span),
        "eq?" => eq(args, span),
        "equal?" => equal(args, span),
        "force" => force(args, span),
        "values" => Ok(Value::Values(args)),
        "call-with-values" => call_with_values(args, span),
        "apply" => apply_procedure_argument(args, span),
        "symbol->string" => unary(args, span.clone(), |value| match value {
            Value::Symbol(name) => Ok(string_value(name)),
            _ => Err(EvalError::TypeError {
                expected: "symbol?",
                span,
            }),
        }),
        "string->symbol" => unary(args, span.clone(), |value| match value {
            Value::String(text) => Ok(Value::Symbol(text.borrow().clone())),
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
        "number->string" => number_to_string(args, span),
        "string->number" => string_to_number_primitive(args, span),
        "cons" => cons(args, span),
        "car" => unary(args, span.clone(), |value| car(value, span)),
        "cdr" => unary(args, span.clone(), |value| cdr(value, span)),
        "set-car!" => set_car(args, span),
        "set-cdr!" => set_cdr(args, span),
        name if composed_accessor_ops(name).is_some() => composed_accessor(name, args, span),
        "list" => Ok(list_value(args)),
        "length" => unary(args, span.clone(), |value| {
            expect_list_items(&value, span.clone())
                .map(|items| Value::Integer(BigInt::from(items.len())))
        }),
        "reverse" => unary(args, span.clone(), |value| {
            let mut items = expect_list_items(&value, span.clone())?;
            items.reverse();
            Ok(list_value(items))
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
            Value::String(text) => Ok(Value::Integer(BigInt::from(text.borrow().chars().count()))),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span,
            }),
        }),
        "char=?" => char_eq(args, span),
        "char<?" => char_compare(args, span, |left, right| left < right),
        "char>?" => char_compare(args, span, |left, right| left > right),
        "char<=?" => char_compare(args, span, |left, right| left <= right),
        "char>=?" => char_compare(args, span, |left, right| left >= right),
        "char-ci=?" => char_ci_compare(args, span, |left, right| left == right),
        "char-ci<?" => char_ci_compare(args, span, |left, right| left < right),
        "char-ci>?" => char_ci_compare(args, span, |left, right| left > right),
        "char-ci<=?" => char_ci_compare(args, span, |left, right| left <= right),
        "char-ci>=?" => char_ci_compare(args, span, |left, right| left >= right),
        "string=?" => string_compare(args, span, |left, right| left == right),
        "string<?" => string_compare(args, span, |left, right| left < right),
        "string>?" => string_compare(args, span, |left, right| left > right),
        "string<=?" => string_compare(args, span, |left, right| left <= right),
        "string>=?" => string_compare(args, span, |left, right| left >= right),
        "string-ci=?" => string_ci_compare(args, span, |left, right| left == right),
        "string-ci<?" => string_ci_compare(args, span, |left, right| left < right),
        "string-ci>?" => string_ci_compare(args, span, |left, right| left > right),
        "string-ci<=?" => string_ci_compare(args, span, |left, right| left <= right),
        "string-ci>=?" => string_ci_compare(args, span, |left, right| left >= right),
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

fn numeric_predicate(
    args: Vec<Value>,
    span: SourceSpan,
    pred: impl FnOnce(&Value, SourceSpan) -> Result<bool, EvalError>,
) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        Ok(Value::Boolean(pred(&value, span)?))
    })
}

fn number_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Integer(_) | Value::Rational(_) | Value::Decimal(_) | Value::Complex(_)
    )
}

fn real_number_value(value: &Value) -> bool {
    match value {
        Value::Integer(_) | Value::Rational(_) | Value::Decimal(_) => true,
        Value::Complex(n) => n.im.is_zero(),
        _ => false,
    }
}

fn rational_number_value(value: &Value) -> bool {
    match value {
        Value::Integer(_) | Value::Rational(_) | Value::Decimal(_) => true,
        Value::Complex(n) => n.im.is_zero(),
        _ => false,
    }
}

fn integer_number_value(value: &Value) -> bool {
    match value {
        Value::Integer(_) => true,
        Value::Rational(n) => n.is_integer(),
        Value::Decimal(n) => n.is_integer(),
        Value::Complex(n) => n.im.is_zero() && n.re.is_integer(),
        _ => false,
    }
}

fn exact_number_value(value: &Value, span: SourceSpan) -> Result<bool, EvalError> {
    match value {
        Value::Integer(_) | Value::Rational(_) => Ok(true),
        Value::Decimal(_) | Value::Complex(_) => Ok(false),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    }
}

fn inexact_number_value(value: &Value, span: SourceSpan) -> Result<bool, EvalError> {
    exact_number_value(value, span).map(|exact| !exact)
}

fn zero_number_value(value: &Value, span: SourceSpan) -> Result<bool, EvalError> {
    match value {
        Value::Integer(n) => Ok(n.is_zero()),
        Value::Rational(n) => Ok(n.is_zero()),
        Value::Decimal(n) => Ok(n.is_zero()),
        Value::Complex(n) => Ok(n.re.is_zero() && n.im.is_zero()),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    }
}

fn positive_number_value(value: &Value, span: SourceSpan) -> Result<bool, EvalError> {
    Ok(real_ordering(value, span)? == Ordering::Greater)
}

fn negative_number_value(value: &Value, span: SourceSpan) -> Result<bool, EvalError> {
    Ok(real_ordering(value, span)? == Ordering::Less)
}

fn odd_number_value(value: &Value, span: SourceSpan) -> Result<bool, EvalError> {
    exact_integer(value, span).map(|n| n.is_odd())
}

fn even_number_value(value: &Value, span: SourceSpan) -> Result<bool, EvalError> {
    exact_integer(value, span).map(|n| n.is_even())
}

fn real_ordering(value: &Value, span: SourceSpan) -> Result<Ordering, EvalError> {
    match value {
        Value::Integer(n) => Ok(n.cmp(&BigInt::zero())),
        Value::Rational(n) => Ok(n.cmp(&BigRational::zero())),
        Value::Decimal(n) => Ok(decimal_ordering(n)),
        Value::Complex(_) => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
    }
}

fn decimal_ordering(number: &BigDecimal) -> Ordering {
    if number < &decimal_zero() {
        Ordering::Less
    } else if number > &decimal_zero() {
        Ordering::Greater
    } else {
        Ordering::Equal
    }
}

fn numeric_abs(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        Value::Rational(n) => Ok(exact_number(n.abs())),
        Value::Decimal(n) => Ok(Value::Decimal(n.abs())),
        Value::Complex(_) => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
    })
}

fn numeric_extreme(
    args: Vec<Value>,
    span: SourceSpan,
    exact_better: impl Fn(&BigRational, &BigRational) -> bool,
    decimal_better: impl Fn(&BigDecimal, &BigDecimal) -> bool,
) -> Result<Value, EvalError> {
    let numbers = numeric_args(args, span.clone())?;
    if numbers.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual: 0,
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
        let mut numbers = numbers.iter().map(NumberValue::to_decimal);
        let first = numbers
            .next()
            .expect("arity check ensures at least one number");
        return Ok(Value::Decimal(numbers.fold(first, |best, n| {
            if decimal_better(&n, &best) { n } else { best }
        })));
    }

    let mut numbers = numbers.into_iter().map(NumberValue::into_exact);
    let first = numbers
        .next()
        .expect("arity check ensures at least one number");
    Ok(exact_number(numbers.fold(first, |best, n| {
        if exact_better(&n, &best) { n } else { best }
    })))
}

fn exact_integer_binary(
    args: Vec<Value>,
    span: SourceSpan,
    zero_expected: &'static str,
    f: impl FnOnce(BigInt, BigInt) -> BigInt,
) -> Result<Value, EvalError> {
    let actual = args.len();
    let [left, right]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let left = exact_integer(&left, span.clone())?;
    let right = exact_integer(&right, span.clone())?;
    if right.is_zero() {
        return Err(EvalError::TypeError {
            expected: zero_expected,
            span,
        });
    }

    Ok(Value::Integer(f(left, right)))
}

fn exact_integer_fold(
    args: Vec<Value>,
    span: SourceSpan,
    identity: BigInt,
    f: impl Fn(BigInt, BigInt) -> BigInt,
) -> Result<Value, EvalError> {
    args.iter()
        .map(|value| exact_integer(value, span.clone()))
        .try_fold(identity, |acc, n| n.map(|n| f(acc, n).abs()))
        .map(Value::Integer)
}

fn modulo_value(left: BigInt, right: BigInt) -> BigInt {
    let remainder = left % &right;
    if remainder.is_zero()
        || remainder.sign() == right.sign()
        || right.sign() == num::bigint::Sign::NoSign
    {
        remainder
    } else {
        remainder + right
    }
}

fn numerator(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        exact_rational(&value, span).map(|n| Value::Integer(n.numer().clone()))
    })
}

fn denominator(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        exact_rational(&value, span).map(|n| Value::Integer(n.denom().clone()))
    })
}

fn numeric_round(
    args: Vec<Value>,
    span: SourceSpan,
    exact: impl FnOnce(&BigRational) -> BigRational,
    decimal: RoundingMode,
) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Integer(n) => Ok(Value::Integer(n)),
        Value::Rational(n) => Ok(exact_number(exact(&n))),
        Value::Decimal(n) => Ok(Value::Decimal(n.with_scale_round(0, decimal))),
        Value::Complex(_) => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
    })
}

fn round_rational_half_even(number: &BigRational) -> BigRational {
    let floor = number.floor();
    let ceiling = number.ceil();
    let distance_to_floor = number - floor.clone();
    let distance_to_ceiling = ceiling.clone() - number;

    match distance_to_floor.cmp(&distance_to_ceiling) {
        Ordering::Less => floor,
        Ordering::Greater => ceiling,
        Ordering::Equal if floor.to_integer().is_even() => floor,
        Ordering::Equal => ceiling,
    }
}

fn exact_to_inexact(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Integer(n) => Ok(Value::Decimal(BigDecimal::from(n))),
        Value::Rational(n) => Ok(Value::Decimal(rational_to_decimal(&n))),
        Value::Decimal(_) | Value::Complex(_) => Ok(value),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    })
}

fn inexact_to_exact(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Integer(_) | Value::Rational(_) => Ok(value),
        Value::Decimal(n) => decimal_to_rational(&n, span).map(exact_number),
        Value::Complex(_) => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    })
}

fn make_rectangular(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [real, imaginary]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;

    Ok(Value::Complex(Complex::new(
        real_to_decimal(real, span.clone())?,
        real_to_decimal(imaginary, span)?,
    )))
}

fn make_polar(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [magnitude, angle]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;

    let magnitude = decimal_to_f64(&real_to_decimal(magnitude, span.clone())?, span.clone())?;
    let angle = decimal_to_f64(&real_to_decimal(angle, span.clone())?, span.clone())?;
    Ok(Value::Complex(Complex::new(
        f64_to_decimal(magnitude * angle.cos(), span.clone())?,
        f64_to_decimal(magnitude * angle.sin(), span)?,
    )))
}

fn real_part(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Integer(_) | Value::Rational(_) | Value::Decimal(_) => Ok(value),
        Value::Complex(n) => Ok(Value::Decimal(n.re)),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    })
}

fn imag_part(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Integer(_) | Value::Rational(_) => Ok(Value::Integer(BigInt::zero())),
        Value::Decimal(_) => Ok(Value::Decimal(decimal_zero())),
        Value::Complex(n) => Ok(Value::Decimal(n.im)),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    })
}

fn magnitude(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let number = number_to_complex_decimal(value, span.clone())?;
        let squared = number.re.clone() * number.re + number.im.clone() * number.im;
        decimal_sqrt(squared, span).map(Value::Decimal)
    })
}

fn angle(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let number = number_to_complex_decimal(value, span.clone())?;
        let real = decimal_to_f64(&number.re, span.clone())?;
        let imaginary = decimal_to_f64(&number.im, span.clone())?;
        f64_to_decimal(imaginary.atan2(real), span).map(Value::Decimal)
    })
}

fn complex_unary(
    args: Vec<Value>,
    span: SourceSpan,
    f: impl FnOnce(Complex<f64>) -> Complex<f64>,
) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let number = number_to_complex_f64(value, span.clone())?;
        complex_f64_to_value(f(number), span)
    })
}

fn numeric_atan(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    match actual {
        1 => complex_unary(args, span, |number| number.atan()),
        2 => {
            let [y, x]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
                expected: 2,
                actual,
                span: span.clone(),
            })?;
            let y = decimal_to_f64(&real_to_decimal(y, span.clone())?, span.clone())?;
            let x = decimal_to_f64(&real_to_decimal(x, span.clone())?, span.clone())?;
            f64_to_decimal(y.atan2(x), span).map(Value::Decimal)
        }
        _ => Err(EvalError::ArityMismatch {
            expected: 1,
            actual,
            span,
        }),
    }
}

fn rationalize(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [number, tolerance]: [Value; 2] =
        args.try_into().map_err(|_| EvalError::ArityMismatch {
            expected: 2,
            actual,
            span: span.clone(),
        })?;

    let number = real_to_rational(number, span.clone())?;
    let tolerance = real_to_rational(tolerance, span.clone())?;
    if tolerance < BigRational::zero() {
        return Err(EvalError::TypeError {
            expected: "non-negative real number?",
            span,
        });
    }

    Ok(exact_number(simplest_rational(
        number.clone() - tolerance.clone(),
        number + tolerance,
    )))
}

fn simplest_rational(low: BigRational, high: BigRational) -> BigRational {
    if high < low {
        return simplest_rational(high, low);
    }
    if low == high {
        return low;
    }
    if low > BigRational::zero() {
        return simplest_positive_rational(low, high);
    }
    if high < BigRational::zero() {
        return -simplest_positive_rational(-high, -low);
    }
    BigRational::zero()
}

fn simplest_positive_rational(low: BigRational, high: BigRational) -> BigRational {
    let low_floor = low.floor().to_integer();
    let high_floor = high.floor().to_integer();
    let low_floor_rational = BigRational::from_integer(low_floor.clone());

    if low == low_floor_rational {
        return low_floor_rational;
    }
    if low_floor == high_floor {
        let one = BigRational::one();
        let low_fraction = low - low_floor_rational.clone();
        let high_fraction = high - BigRational::from_integer(high_floor);
        return low_floor_rational
            + one.clone()
                / simplest_positive_rational(one.clone() / high_fraction, one / low_fraction);
    }

    BigRational::from_integer(low_floor + BigInt::one())
}

fn numeric_sqrt(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let decimal = real_to_decimal(value, span.clone())?;
        if decimal < decimal_zero() {
            return decimal_sqrt(-decimal, span)
                .map(|imaginary| Value::Complex(Complex::new(decimal_zero(), imaginary)));
        }

        decimal_sqrt(decimal, span).map(Value::Decimal)
    })
}

fn numeric_expt(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [base, exponent]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let exponent = exact_integer(&exponent, span.clone())?;
    let exponent = exponent.to_i32().ok_or(EvalError::TypeError {
        expected: "small integer exponent?",
        span: span.clone(),
    })?;

    match base {
        Value::Integer(n) => Ok(exact_number(BigRational::from_integer(n).pow(exponent))),
        Value::Rational(n) => Ok(exact_number(n.pow(exponent))),
        Value::Decimal(n) => Ok(Value::Decimal(decimal_pow(n, exponent))),
        Value::Complex(n) => Ok(Value::Complex(complex_pow(n, exponent))),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    }
}

fn decimal_pow(base: BigDecimal, exponent: i32) -> BigDecimal {
    let power = pow_nonnegative(base, exponent.unsigned_abs(), decimal_one());
    if exponent.is_negative() {
        decimal_one() / power
    } else {
        power
    }
}

fn complex_pow(base: Complex<BigDecimal>, exponent: i32) -> Complex<BigDecimal> {
    let power = pow_nonnegative(base, exponent.unsigned_abs(), complex_one());
    if exponent.is_negative() {
        complex_one() / power
    } else {
        power
    }
}

fn pow_nonnegative<T>(mut base: T, mut exponent: u32, one: T) -> T
where
    T: Clone + std::ops::Mul<Output = T>,
{
    let mut result = one;
    while exponent > 0 {
        if exponent % 2 == 1 {
            result = result * base.clone();
        }
        base = base.clone() * base;
        exponent /= 2;
    }
    result
}

fn decimal_sqrt(number: BigDecimal, span: SourceSpan) -> Result<BigDecimal, EvalError> {
    number
        .sqrt()
        .map(|number| number.normalized())
        .ok_or(EvalError::TypeError {
            expected: "number with square root?",
            span,
        })
}

fn real_to_decimal(value: Value, span: SourceSpan) -> Result<BigDecimal, EvalError> {
    match value {
        Value::Integer(n) => Ok(BigDecimal::from(n)),
        Value::Rational(n) => Ok(rational_to_decimal(&n)),
        Value::Decimal(n) => Ok(n),
        Value::Complex(_) => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
    }
}

fn real_to_rational(value: Value, span: SourceSpan) -> Result<BigRational, EvalError> {
    match value {
        Value::Integer(n) => Ok(BigRational::from_integer(n)),
        Value::Rational(n) => Ok(n),
        Value::Decimal(n) => decimal_to_rational(&n, span),
        Value::Complex(_) => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
        _ => Err(EvalError::TypeError {
            expected: "real number?",
            span,
        }),
    }
}

fn number_to_complex_decimal(
    value: Value,
    span: SourceSpan,
) -> Result<Complex<BigDecimal>, EvalError> {
    match value {
        Value::Integer(n) => Ok(Complex::new(BigDecimal::from(n), decimal_zero())),
        Value::Rational(n) => Ok(Complex::new(rational_to_decimal(&n), decimal_zero())),
        Value::Decimal(n) => Ok(Complex::new(n, decimal_zero())),
        Value::Complex(n) => Ok(n),
        _ => Err(EvalError::TypeError {
            expected: "number?",
            span,
        }),
    }
}

fn number_to_complex_f64(value: Value, span: SourceSpan) -> Result<Complex<f64>, EvalError> {
    let number = number_to_complex_decimal(value, span.clone())?;
    Ok(Complex::new(
        decimal_to_f64(&number.re, span.clone())?,
        decimal_to_f64(&number.im, span)?,
    ))
}

fn decimal_to_rational(number: &BigDecimal, span: SourceSpan) -> Result<BigRational, EvalError> {
    let (digits, scale) = number.as_bigint_and_exponent();
    if scale >= 0 {
        Ok(BigRational::new(digits, power_of_ten(scale, span)?))
    } else {
        let scale = scale.checked_neg().ok_or(EvalError::TypeError {
            expected: "representable decimal scale?",
            span: span.clone(),
        })?;
        Ok(BigRational::from_integer(
            digits * power_of_ten(scale, span)?,
        ))
    }
}

fn decimal_to_f64(number: &BigDecimal, span: SourceSpan) -> Result<f64, EvalError> {
    number.to_f64().ok_or(EvalError::TypeError {
        expected: "finite decimal?",
        span,
    })
}

fn f64_to_decimal(number: f64, span: SourceSpan) -> Result<BigDecimal, EvalError> {
    BigDecimal::from_f64(number)
        .map(|number| number.normalized())
        .ok_or(EvalError::TypeError {
            expected: "finite decimal?",
            span,
        })
}

fn complex_f64_to_value(number: Complex<f64>, span: SourceSpan) -> Result<Value, EvalError> {
    let real = f64_to_decimal(zero_tiny_f64(number.re), span.clone())?;
    let imaginary = f64_to_decimal(zero_tiny_f64(number.im), span)?;
    if imaginary == decimal_zero() {
        Ok(Value::Decimal(real))
    } else {
        Ok(Value::Complex(Complex::new(real, imaginary)))
    }
}

fn zero_tiny_f64(number: f64) -> f64 {
    if number.abs() < 1e-12 { 0.0 } else { number }
}

fn power_of_ten(exponent: i64, span: SourceSpan) -> Result<BigInt, EvalError> {
    let exponent = u32::try_from(exponent).map_err(|_| EvalError::TypeError {
        expected: "representable decimal scale?",
        span,
    })?;
    Ok(BigInt::from(10).pow(exponent))
}

fn exact_integer(value: &Value, span: SourceSpan) -> Result<BigInt, EvalError> {
    match value {
        Value::Integer(n) => Ok(n.clone()),
        Value::Rational(n) if n.is_integer() => Ok(n.to_integer()),
        _ => Err(EvalError::TypeError {
            expected: "exact integer?",
            span,
        }),
    }
}

fn exact_rational(value: &Value, span: SourceSpan) -> Result<BigRational, EvalError> {
    match value {
        Value::Integer(n) => Ok(BigRational::from_integer(n.clone())),
        Value::Rational(n) => Ok(n.clone()),
        _ => Err(EvalError::TypeError {
            expected: "exact rational?",
            span,
        }),
    }
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

fn char_predicate(
    args: Vec<Value>,
    span: SourceSpan,
    pred: impl FnOnce(char) -> bool,
) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Character(c) => Ok(Value::Boolean(pred(c))),
        _ => Err(EvalError::TypeError {
            expected: "char?",
            span,
        }),
    })
}

fn char_map(
    args: Vec<Value>,
    span: SourceSpan,
    f: impl FnOnce(char) -> char,
) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::Character(c) => Ok(Value::Character(f(c))),
        _ => Err(EvalError::TypeError {
            expected: "char?",
            span,
        }),
    })
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

fn char_ci_compare(
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

    let chars = args
        .into_iter()
        .map(|value| match value {
            Value::Character(c) => Ok(c.to_lowercase().to_string()),
            _ => Err(EvalError::TypeError {
                expected: "char?",
                span: span.clone(),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Boolean(
        chars
            .windows(2)
            .all(|pair| pred(pair[0].as_str(), pair[1].as_str())),
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
            Value::String(text) => Ok(text.borrow().clone()),
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

fn make_string(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual: args.len(),
            span,
        });
    }

    let len = exact_nonnegative_integer(&args[0], span.clone())?;
    let fill = match args.get(1) {
        Some(Value::Character(c)) => *c,
        Some(_) => {
            return Err(EvalError::TypeError {
                expected: "char?",
                span,
            });
        }
        None => ' ',
    };

    Ok(string_value(fill.to_string().repeat(len)))
}

fn string(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    args.into_iter()
        .map(|value| match value {
            Value::Character(c) => Ok(c),
            _ => Err(EvalError::TypeError {
                expected: "char?",
                span: span.clone(),
            }),
        })
        .collect::<Result<String, _>>()
        .map(string_value)
}

fn string_ref(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [string, index]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let Value::String(text) = string else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };
    let index = exact_nonnegative_integer(&index, span.clone())?;
    let Some(ch) = text.borrow().chars().nth(index) else {
        return Err(EvalError::TypeError {
            expected: "valid string index",
            span,
        });
    };

    Ok(Value::Character(ch))
}

fn string_set(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [string, index, value]: [Value; 3] =
        args.try_into().map_err(|_| EvalError::ArityMismatch {
            expected: 3,
            actual,
            span: span.clone(),
        })?;
    let Value::String(text) = string else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };
    let Value::Character(ch) = value else {
        return Err(EvalError::TypeError {
            expected: "char?",
            span,
        });
    };
    let index = exact_nonnegative_integer(&index, span.clone())?;
    let mut text = text.borrow_mut();
    let start = string_byte_index(&text, index, span.clone())?;
    if start == text.len() {
        return Err(EvalError::TypeError {
            expected: "valid string index",
            span,
        });
    }
    let end = next_char_byte_index(&text, start);
    text.replace_range(start..end, ch.to_string().as_str());
    Ok(Value::Unspecified)
}

fn substring(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [string, start, end]: [Value; 3] =
        args.try_into().map_err(|_| EvalError::ArityMismatch {
            expected: 3,
            actual,
            span: span.clone(),
        })?;
    let Value::String(text) = string else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };
    let start = exact_nonnegative_integer(&start, span.clone())?;
    let end = exact_nonnegative_integer(&end, span.clone())?;
    if start > end {
        return Err(EvalError::TypeError {
            expected: "ordered substring indexes",
            span,
        });
    }

    let text = text.borrow();
    let start_byte = string_byte_index(&text, start, span.clone())?;
    let end_byte = string_byte_index(&text, end, span)?;
    Ok(string_value(text[start_byte..end_byte].to_string()))
}

fn string_append(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    args.into_iter()
        .map(|value| match value {
            Value::String(text) => Ok(text.borrow().clone()),
            _ => Err(EvalError::TypeError {
                expected: "string?",
                span: span.clone(),
            }),
        })
        .collect::<Result<String, _>>()
        .map(string_value)
}

fn string_to_list(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| match value {
        Value::String(text) => Ok(list_value(
            text.borrow().chars().map(Value::Character).collect(),
        )),
        _ => Err(EvalError::TypeError {
            expected: "string?",
            span,
        }),
    })
}

fn list_to_string(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        expect_list_items(&value, span.clone())?
            .into_iter()
            .map(|value| match value {
                Value::Character(c) => Ok(c),
                _ => Err(EvalError::TypeError {
                    expected: "char?",
                    span: span.clone(),
                }),
            })
            .collect::<Result<String, _>>()
            .map(string_value)
    })
}

fn string_fill(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [string, value]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let Value::String(text) = string else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };
    let Value::Character(ch) = value else {
        return Err(EvalError::TypeError {
            expected: "char?",
            span,
        });
    };
    let len = text.borrow().chars().count();
    *text.borrow_mut() = ch.to_string().repeat(len);
    Ok(Value::Unspecified)
}

fn string_byte_index(text: &str, index: usize, span: SourceSpan) -> Result<usize, EvalError> {
    if index == text.chars().count() {
        return Ok(text.len());
    }

    text.char_indices()
        .nth(index)
        .map(|(byte_index, _)| byte_index)
        .ok_or(EvalError::TypeError {
            expected: "valid string index",
            span,
        })
}

fn next_char_byte_index(text: &str, start: usize) -> usize {
    text[start..]
        .char_indices()
        .nth(1)
        .map(|(offset, _)| start + offset)
        .unwrap_or(text.len())
}

fn string_ci_compare(
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
            Value::String(text) => Ok(text.borrow().to_lowercase()),
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

fn call_with_values(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [producer, consumer]: [Value; 2] =
        args.try_into().map_err(|_| EvalError::ArityMismatch {
            expected: 2,
            actual,
            span: span.clone(),
        })?;

    let produced = apply(producer, Vec::new(), span.clone())?;
    let consumer_args = match produced {
        Value::Values(values) => values,
        value => vec![value],
    };
    apply(consumer, consumer_args, span)
}

fn current_input_port(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: 0,
            actual: args.len(),
            span,
        });
    }

    CURRENT_INPUT_PORT.with(|port| Ok(Value::InputPort(port.borrow().clone())))
}

fn open_input_file(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let Value::String(path) = value else {
            return Err(EvalError::TypeError {
                expected: "string?",
                span,
            });
        };
        input_port_from_path(path.borrow().as_str(), span).map(Value::InputPort)
    })
}

fn input_port_from_path(path: &str, span: SourceSpan) -> Result<InputPort, EvalError> {
    fs::read_to_string(path)
        .map(InputPort::from_string)
        .map_err(|error| EvalError::IoError {
            message: error.to_string(),
            span,
        })
}

fn close_input_port(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let port = input_port(value, span)?;
        port.0.borrow_mut().closed = true;
        Ok(Value::Unspecified)
    })
}

fn read_datum(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let port = optional_input_port(args, span.clone())?;
    let mut state = port.0.borrow_mut();
    if state.closed {
        return Err(EvalError::TypeError {
            expected: "open input-port?",
            span,
        });
    }
    if state.index >= state.chars.len() && matches!(state.kind, InputPortKind::Stdin) {
        refill_stdin(&mut state, span.clone())?;
    }
    if state.index >= state.chars.len() {
        return Ok(Value::EofObject);
    }

    let remaining = state.chars[state.index..].iter().collect::<String>();
    let datums = parse_datums(&remaining).map_err(|error| EvalError::ReadError {
        message: error.to_string(),
        span: span.clone(),
    })?;
    let Some(datum) = datums.into_iter().next() else {
        state.index = state.chars.len();
        return Ok(Value::EofObject);
    };
    state.index += remaining[..datum.span.end].chars().count();
    drop(state);

    datum_to_value(&datum)
}

fn read_char(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let port = optional_input_port(args, span.clone())?;
    input_char(&port, span, true)
}

fn peek_char(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let port = optional_input_port(args, span.clone())?;
    input_char(&port, span, false)
}

fn char_ready(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let port = optional_input_port(args, span.clone())?;
    let state = port.0.borrow();
    if state.closed {
        return Err(EvalError::TypeError {
            expected: "open input-port?",
            span,
        });
    }

    Ok(Value::Boolean(state.index < state.chars.len()))
}

fn optional_input_port(args: Vec<Value>, span: SourceSpan) -> Result<InputPort, EvalError> {
    let actual = args.len();
    let mut args = args.into_iter();
    let port = match args.next() {
        Some(port) => input_port(port, span.clone())?,
        None => CURRENT_INPUT_PORT.with(|port| port.borrow().clone()),
    };
    if args.next().is_some() {
        return Err(EvalError::ArityMismatch {
            expected: 0,
            actual,
            span,
        });
    }

    Ok(port)
}

fn input_port(value: Value, span: SourceSpan) -> Result<InputPort, EvalError> {
    match value {
        Value::InputPort(port) => Ok(port),
        _ => Err(EvalError::TypeError {
            expected: "input-port?",
            span,
        }),
    }
}

fn input_char(port: &InputPort, span: SourceSpan, advance: bool) -> Result<Value, EvalError> {
    let mut state = port.0.borrow_mut();
    if state.closed {
        return Err(EvalError::TypeError {
            expected: "open input-port?",
            span,
        });
    }
    if state.index >= state.chars.len() && matches!(state.kind, InputPortKind::Stdin) {
        refill_stdin(&mut state, span.clone())?;
    }
    if state.index >= state.chars.len() {
        return Ok(Value::EofObject);
    }

    let ch = state.chars[state.index];
    if advance {
        state.index += 1;
    }
    Ok(Value::Character(ch))
}

fn refill_stdin(state: &mut InputPortState, span: SourceSpan) -> Result<(), EvalError> {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|error| EvalError::IoError {
            message: error.to_string(),
            span,
        })?;
    state.chars = line.chars().collect();
    state.index = 0;
    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum OutputMode {
    Write,
    Display,
}

fn current_output_port(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: 0,
            actual: args.len(),
            span,
        });
    }

    CURRENT_OUTPUT_PORT.with(|port| Ok(Value::OutputPort(port.borrow().clone())))
}

fn open_output_file(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let Value::String(path) = value else {
            return Err(EvalError::TypeError {
                expected: "string?",
                span,
            });
        };
        output_port_from_path(path.borrow().as_str(), span).map(Value::OutputPort)
    })
}

fn output_port_from_path(path: &str, span: SourceSpan) -> Result<OutputPort, EvalError> {
    fs::File::create(path)
        .map(|file| {
            OutputPort::File(Rc::new(RefCell::new(OutputFilePort {
                file,
                closed: false,
            })))
        })
        .map_err(|error| EvalError::IoError {
            message: error.to_string(),
            span,
        })
}

fn call_with_input_file(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [path, procedure]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let Value::String(path) = path else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };

    let port = input_port_from_path(path.borrow().as_str(), span.clone())?;
    let result = apply(
        procedure,
        vec![Value::InputPort(port.clone())],
        span.clone(),
    );
    port.0.borrow_mut().closed = true;
    result
}

fn call_with_output_file(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [path, procedure]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let Value::String(path) = path else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };

    let port = output_port_from_path(path.borrow().as_str(), span.clone())?;
    let result = apply(
        procedure,
        vec![Value::OutputPort(port.clone())],
        span.clone(),
    );
    close_output_port_value(port);
    result
}

fn with_input_from_file(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [path, thunk]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let Value::String(path) = path else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };

    let port = input_port_from_path(path.borrow().as_str(), span.clone())?;
    let old = CURRENT_INPUT_PORT.with(|current| current.replace(port.clone()));
    let result = apply(thunk, Vec::new(), span);
    CURRENT_INPUT_PORT.with(|current| {
        current.replace(old);
    });
    port.0.borrow_mut().closed = true;
    result
}

fn with_output_to_file(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [path, thunk]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let Value::String(path) = path else {
        return Err(EvalError::TypeError {
            expected: "string?",
            span,
        });
    };

    let port = output_port_from_path(path.borrow().as_str(), span.clone())?;
    let old = CURRENT_OUTPUT_PORT.with(|current| current.replace(port.clone()));
    let result = apply(thunk, Vec::new(), span);
    CURRENT_OUTPUT_PORT.with(|current| {
        current.replace(old);
    });
    close_output_port_value(port);
    result
}

fn load(args: Vec<Value>, span: SourceSpan, env: &Env) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let Value::String(path) = value else {
            return Err(EvalError::TypeError {
                expected: "string?",
                span,
            });
        };
        let source =
            fs::read_to_string(path.borrow().as_str()).map_err(|error| EvalError::IoError {
                message: error.to_string(),
                span: span.clone(),
            })?;
        let datums = parse_datums(&source).map_err(|error| EvalError::ReadError {
            message: error.to_string(),
            span: span.clone(),
        })?;
        let program = classify_program(&datums).map_err(|error| EvalError::ReadError {
            message: error.to_string(),
            span: span.clone(),
        })?;
        eval_program(&program, env)
            .map(|values| values.last().cloned().unwrap_or(Value::Unspecified))
    })
}

fn eval_value(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [expr, environment]: [Value; 2] =
        args.try_into().map_err(|_| EvalError::ArityMismatch {
            expected: 2,
            actual,
            span: span.clone(),
        })?;
    let Value::Environment(env) = environment else {
        return Err(EvalError::TypeError {
            expected: "environment?",
            span,
        });
    };

    let datum = value_to_datum(expr, span.clone())?;
    let expr = classify_expr(&datum).map_err(|error| EvalError::ReadError {
        message: error.to_string(),
        span: span.clone(),
    })?;
    eval_expr(&expr, &env)
}

fn dynamic_wind(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [before, thunk, after]: [Value; 3] =
        args.try_into().map_err(|_| EvalError::ArityMismatch {
            expected: 3,
            actual,
            span: span.clone(),
        })?;

    apply(before, Vec::new(), span.clone())?;
    let result = apply(thunk, Vec::new(), span.clone());
    let after_result = apply(after, Vec::new(), span);
    match (result, after_result) {
        (Ok(value), Ok(_)) => Ok(value),
        (Err(error), Ok(_)) | (Ok(_), Err(error)) | (Err(error), Err(_)) => Err(error),
    }
}

fn call_cc(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |procedure| {
        let id = next_continuation_id();
        let continuation = Value::Continuation(Continuation { id });
        match apply(procedure, vec![continuation], span.clone()) {
            Err(EvalError::ContinuationJump {
                id: jump_id, value, ..
            }) if jump_id == id => Ok(*value),
            result => result,
        }
    })
}

fn next_continuation_id() -> usize {
    NEXT_CONTINUATION_ID.with(|cell| {
        let id = cell.get();
        cell.set(id.wrapping_add(1));
        id
    })
}

fn apply_continuation(
    continuation: Continuation,
    args: Vec<Value>,
    span: SourceSpan,
) -> Result<Value, EvalError> {
    let actual = args.len();
    let [value]: [Value; 1] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 1,
        actual,
        span: span.clone(),
    })?;
    Err(EvalError::ContinuationJump {
        id: continuation.id,
        value: Box::new(value),
        span,
    })
}

fn scheme_report_environment(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    require_environment_version(args, span)?;
    Ok(Value::Environment(Env::new()))
}

fn null_environment(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    require_environment_version(args, span)?;
    Ok(Value::Environment(Env::empty()))
}

fn interaction_environment(
    args: Vec<Value>,
    span: SourceSpan,
    env: &Env,
) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: 0,
            actual: args.len(),
            span,
        });
    }

    Ok(Value::Environment(env.clone()))
}

fn require_environment_version(args: Vec<Value>, span: SourceSpan) -> Result<(), EvalError> {
    let version = unary(args, span.clone(), |value| {
        exact_integer(&value, span.clone()).map(Value::Integer)
    })?;
    match version {
        Value::Integer(version) if version == BigInt::from(5) => Ok(()),
        _ => Err(EvalError::TypeError {
            expected: "R5RS version 5?",
            span,
        }),
    }
}

fn close_output_port(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let port = output_port(value, span)?;
        close_output_port_value(port);
        Ok(Value::Unspecified)
    })
}

fn close_output_port_value(port: OutputPort) {
    match port {
        OutputPort::Stdout => {}
        OutputPort::File(file) => file.borrow_mut().closed = true,
        #[cfg(test)]
        OutputPort::Buffer(_) => {}
    }
}

fn output_value(args: Vec<Value>, span: SourceSpan, mode: OutputMode) -> Result<Value, EvalError> {
    let (value, port) = value_and_optional_output_port(args, span.clone())?;
    let text = match mode {
        OutputMode::Write => value.to_string(),
        OutputMode::Display => display_text(&value),
    };
    write_output(&text, &port, span)
}

fn newline(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let port = optional_output_port(args, span.clone())?;
    write_output("\n", &port, span)
}

fn write_char(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let (value, port) = value_and_optional_output_port(args, span.clone())?;
    match value {
        Value::Character(ch) => write_output(&ch.to_string(), &port, span),
        _ => Err(EvalError::TypeError {
            expected: "char?",
            span,
        }),
    }
}

fn transcript_on(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    unary(args, span.clone(), |value| {
        let Value::String(path) = value else {
            return Err(EvalError::TypeError {
                expected: "string?",
                span,
            });
        };
        let port = output_port_from_path(path.borrow().as_str(), span)?;
        TRANSCRIPT_PORT.with(|current| {
            if let Some(old) = current.replace(Some(port)) {
                close_output_port_value(old);
            }
        });
        Ok(Value::Unspecified)
    })
}

fn transcript_off(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::ArityMismatch {
            expected: 0,
            actual: args.len(),
            span,
        });
    }

    TRANSCRIPT_PORT.with(|current| {
        if let Some(port) = current.replace(None) {
            close_output_port_value(port);
        }
    });
    Ok(Value::Unspecified)
}

fn value_and_optional_output_port(
    args: Vec<Value>,
    span: SourceSpan,
) -> Result<(Value, OutputPort), EvalError> {
    let actual = args.len();
    let mut args = args.into_iter();
    let value = args.next().ok_or(EvalError::ArityMismatch {
        expected: 1,
        actual,
        span: span.clone(),
    })?;
    let port = match args.next() {
        Some(port) => output_port(port, span.clone())?,
        None => CURRENT_OUTPUT_PORT.with(|port| port.borrow().clone()),
    };
    if args.next().is_some() {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual,
            span,
        });
    }

    Ok((value, port))
}

fn optional_output_port(args: Vec<Value>, span: SourceSpan) -> Result<OutputPort, EvalError> {
    let actual = args.len();
    let mut args = args.into_iter();
    let port = match args.next() {
        Some(port) => output_port(port, span.clone())?,
        None => CURRENT_OUTPUT_PORT.with(|port| port.borrow().clone()),
    };
    if args.next().is_some() {
        return Err(EvalError::ArityMismatch {
            expected: 0,
            actual,
            span,
        });
    }

    Ok(port)
}

fn output_port(value: Value, span: SourceSpan) -> Result<OutputPort, EvalError> {
    match value {
        Value::OutputPort(port) => Ok(port),
        _ => Err(EvalError::TypeError {
            expected: "output-port?",
            span,
        }),
    }
}

fn display_text(value: &Value) -> String {
    match value {
        Value::String(text) => text.borrow().clone(),
        Value::Character(ch) => ch.to_string(),
        _ => value.to_string(),
    }
}

fn write_output(text: &str, port: &OutputPort, span: SourceSpan) -> Result<Value, EvalError> {
    write_output_raw(text, port, span.clone())?;
    mirror_transcript(text, port, span)?;
    Ok(Value::Unspecified)
}

fn mirror_transcript(text: &str, port: &OutputPort, span: SourceSpan) -> Result<(), EvalError> {
    if matches!(port, OutputPort::Stdout) {
        let transcript = TRANSCRIPT_PORT.with(|current| current.borrow().clone());
        if let Some(transcript) = transcript {
            write_output_raw(text, &transcript, span)?;
        }
    }
    Ok(())
}

fn write_output_raw(text: &str, port: &OutputPort, span: SourceSpan) -> Result<(), EvalError> {
    match port {
        OutputPort::Stdout => {
            let mut stdout = io::stdout().lock();
            stdout
                .write_all(text.as_bytes())
                .and_then(|_| stdout.flush())
                .map_err(|error| EvalError::IoError {
                    message: error.to_string(),
                    span,
                })?;
            Ok(())
        }
        OutputPort::File(file) => {
            let mut port = file.borrow_mut();
            if port.closed {
                return Err(EvalError::TypeError {
                    expected: "open output-port?",
                    span,
                });
            }
            port.file
                .write_all(text.as_bytes())
                .and_then(|_| port.file.flush())
                .map_err(|error| EvalError::IoError {
                    message: error.to_string(),
                    span,
                })?;
            Ok(())
        }
        #[cfg(test)]
        OutputPort::Buffer(output) => {
            output.borrow_mut().push_str(text);
            Ok(())
        }
    }
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
    let final_operands = expect_list_items(&final_operand, span.clone())?;

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

fn number_to_string(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    if !(1..=2).contains(&actual) {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual,
            span,
        });
    }

    let mut args = args.into_iter();
    let number = args.next().expect("arity check ensures number argument");
    let radix = match args.next() {
        Some(radix) => radix_argument(&radix, span.clone())?,
        None => 10,
    };

    if radix == 10 {
        return match number {
            Value::Integer(_) | Value::Rational(_) | Value::Decimal(_) | Value::Complex(_) => {
                Ok(string_value(number.to_string()))
            }
            _ => Err(EvalError::TypeError {
                expected: "number?",
                span,
            }),
        };
    }

    let integer = exact_integer(&number, span)?;
    Ok(string_value(integer.to_str_radix(radix)))
}

fn string_to_number_primitive(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    if !(1..=2).contains(&actual) {
        return Err(EvalError::ArityMismatch {
            expected: 1,
            actual,
            span,
        });
    }

    let mut args = args.into_iter();
    let string = args.next().expect("arity check ensures string argument");
    let text = match string {
        Value::String(text) => text,
        _ => {
            return Err(EvalError::TypeError {
                expected: "string?",
                span,
            });
        }
    };
    let radix = match args.next() {
        Some(radix) => radix_argument(&radix, span.clone())?,
        None => 10,
    };

    Ok(string_to_number_with_radix(text.borrow().as_str(), radix))
}

fn radix_argument(value: &Value, span: SourceSpan) -> Result<u32, EvalError> {
    let radix = exact_integer(value, span.clone())?;
    let Some(radix) = radix.to_u32() else {
        return Err(EvalError::TypeError {
            expected: "radix 2, 8, 10, or 16?",
            span,
        });
    };

    match radix {
        2 | 8 | 10 | 16 => Ok(radix),
        _ => Err(EvalError::TypeError {
            expected: "radix 2, 8, 10, or 16?",
            span,
        }),
    }
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
            exact_number(BigRational::new(numerator, denominator))
        }
        Token::Decimal(n) => Value::Decimal(n),
        Token::Complex(n) => Value::Complex(n),
        _ => Value::Boolean(false),
    }
}

fn string_to_number_with_radix(text: &str, radix: u32) -> Value {
    if radix == 10 {
        return string_to_number(text);
    }

    let text = text.trim();
    if text_has_explicit_radix(text) {
        return string_to_number(text);
    }

    match BigInt::from_str_radix(text, radix) {
        Ok(number) => Value::Integer(number),
        Err(_) => Value::Boolean(false),
    }
}

fn text_has_explicit_radix(text: &str) -> bool {
    let text = strip_exactness_prefix(text).unwrap_or(text);
    text.get(..2).is_some_and(is_radix_prefix)
}

fn strip_exactness_prefix(text: &str) -> Option<&str> {
    text.get(..2)
        .filter(|prefix| matches!(*prefix, "#e" | "#E" | "#i" | "#I"))
        .map(|_| &text[2..])
}

fn is_radix_prefix(prefix: &str) -> bool {
    matches!(
        prefix,
        "#b" | "#B" | "#o" | "#O" | "#d" | "#D" | "#x" | "#X"
    )
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
        (Value::String(left), Value::String(right)) => *left.borrow() == *right.borrow(),
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::InputPort(left), Value::InputPort(right)) => left == right,
        (Value::OutputPort(left), Value::OutputPort(right)) => left == right,
        (Value::Environment(left), Value::Environment(right)) => Rc::ptr_eq(&left.0, &right.0),
        (Value::Continuation(left), Value::Continuation(right)) => left == right,
        (Value::EofObject, Value::EofObject) => true,
        (Value::List(left), Value::List(right)) if left.is_empty() && right.is_empty() => true,
        _ => false,
    }
}

fn eq_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Promise(left), Value::Promise(right)) => Rc::ptr_eq(left, right),
        _ => eqv_value(left, right),
    }
}

fn equal_value(left: &Value, right: &Value) -> bool {
    equal_value_seen(left, right, &mut EqualitySeen::default())
}

#[derive(Default)]
struct EqualitySeen {
    pairs: HashSet<(PairPointer, PairPointer)>,
    vectors: HashSet<(VectorPointer, VectorPointer)>,
}

fn equal_value_seen(left: &Value, right: &Value, seen: &mut EqualitySeen) -> bool {
    match (left, right) {
        (Value::List(left), Value::List(right)) => {
            left.iter()
                .zip(right)
                .all(|(left, right)| equal_value_seen(left, right, seen))
                && left.len() == right.len()
        }
        (Value::List(_), Value::Pair(_)) | (Value::Pair(_), Value::List(_)) => {
            let Some(left) = list_items(left) else {
                return false;
            };
            let Some(right) = list_items(right) else {
                return false;
            };
            left.iter()
                .zip(right.iter())
                .all(|(left, right)| equal_value_seen(left, right, seen))
                && left.len() == right.len()
        }
        (Value::Pair(left), Value::Pair(right)) => {
            let key = (Rc::as_ptr(left), Rc::as_ptr(right));
            if !seen.pairs.insert(key) {
                return true;
            }
            let (left_car, left_cdr, right_car, right_cdr) = {
                let left = left.borrow();
                let right = right.borrow();
                (
                    left.car.clone(),
                    left.cdr.clone(),
                    right.car.clone(),
                    right.cdr.clone(),
                )
            };
            let equal = equal_value_seen(&left_car, &right_car, seen)
                && equal_value_seen(&left_cdr, &right_cdr, seen);
            seen.pairs.remove(&key);
            equal
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let key = (Rc::as_ptr(left), Rc::as_ptr(right));
            if !seen.vectors.insert(key) {
                return true;
            }
            let left = left.borrow();
            let right = right.borrow();
            let equal = left
                .iter()
                .zip(right.iter())
                .all(|(left, right)| equal_value_seen(left, right, seen))
                && left.len() == right.len();
            seen.vectors.remove(&key);
            equal
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
        Value::Pair(pair) => Ok(pair.borrow().car.clone()),
        _ => Err(EvalError::TypeError {
            expected: "pair?",
            span,
        }),
    }
}

fn cdr(value: Value, span: SourceSpan) -> Result<Value, EvalError> {
    match value {
        Value::List(items) if !items.is_empty() => Ok(list_value(items[1..].to_vec())),
        Value::Pair(pair) => Ok(pair.borrow().cdr.clone()),
        _ => Err(EvalError::TypeError {
            expected: "pair?",
            span,
        }),
    }
}

fn set_car(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [pair, value]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    match pair {
        Value::Pair(pair) => {
            pair.borrow_mut().car = value;
            Ok(Value::Unspecified)
        }
        _ => Err(EvalError::TypeError {
            expected: "mutable pair?",
            span,
        }),
    }
}

fn set_cdr(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [pair, value]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    match pair {
        Value::Pair(pair) => {
            pair.borrow_mut().cdr = value;
            Ok(Value::Unspecified)
        }
        _ => Err(EvalError::TypeError {
            expected: "mutable pair?",
            span,
        }),
    }
}

fn composed_accessor(
    name: &'static str,
    args: Vec<Value>,
    span: SourceSpan,
) -> Result<Value, EvalError> {
    let ops = composed_accessor_ops(name).expect("caller checks composed accessor name");
    unary(args, span.clone(), |value| {
        ops.into_iter().try_fold(value, |value, op| match op {
            'a' => car(value, span.clone()),
            'd' => cdr(value, span.clone()),
            _ => unreachable!("composed accessors only contain a and d"),
        })
    })
}

fn composed_accessor_ops(name: &str) -> Option<Vec<char>> {
    let inner = name.strip_prefix('c')?.strip_suffix('r')?;
    if !(2..=4).contains(&inner.len()) || !inner.chars().all(|ch| matches!(ch, 'a' | 'd')) {
        return None;
    }

    Some(inner.chars().rev().collect())
}

fn append(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(empty_list());
    }

    let mut args = args.into_iter().rev();
    let tail = args.next().expect("empty argument list was handled");
    args.try_fold(tail, |tail, list| {
        Ok(expect_list_items(&list, span.clone())?
            .into_iter()
            .rev()
            .fold(tail, |tail, head| cons_value(head, tail)))
    })
}

fn list_ref(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [list, index]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let index = exact_nonnegative_integer(&index, span.clone())?;

    car(list_tail_at(list, index, span.clone())?, span.clone()).map_err(|_| EvalError::TypeError {
        expected: "valid list index",
        span,
    })
}

fn list_tail(args: Vec<Value>, span: SourceSpan) -> Result<Value, EvalError> {
    let actual = args.len();
    let [list, index]: [Value; 2] = args.try_into().map_err(|_| EvalError::ArityMismatch {
        expected: 2,
        actual,
        span: span.clone(),
    })?;
    let index = exact_nonnegative_integer(&index, span.clone())?;

    list_tail_at(list, index, span)
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

    let mut tail = list;
    let mut seen = HashSet::new();
    loop {
        match tail.clone() {
            Value::Pair(pair) => {
                if !seen.insert(Rc::as_ptr(&pair)) {
                    return Err(EvalError::TypeError {
                        expected: "list?",
                        span,
                    });
                }
                let (car, cdr) = {
                    let pair = pair.borrow();
                    (pair.car.clone(), pair.cdr.clone())
                };
                if compare(&target, &car) {
                    return Ok(tail);
                }
                tail = cdr;
            }
            Value::List(items) => {
                return Ok(items
                    .iter()
                    .position(|item| compare(&target, item))
                    .map(|index| list_value(items[index..].to_vec()))
                    .unwrap_or(Value::Boolean(false)));
            }
            _ => {
                return Err(EvalError::TypeError {
                    expected: "list?",
                    span,
                });
            }
        }
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

    let entries = expect_list_items(&alist, span.clone())?;

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
            Value::Pair(pair) => {
                let matches = {
                    let pair = pair.borrow();
                    compare(&target, &pair.car)
                };
                if matches {
                    return Ok(entry);
                }
            }
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

    Ok(list_value(results))
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
    if !is_procedure(&procedure) {
        return Err(EvalError::NotProcedure { span: span.clone() });
    }

    let lists = args
        .map(|value| expect_list_items(&value, span.clone()))
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
            .map(list_value),
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
    Ok(list_value(vec![
        Value::Symbol(name.to_string()),
        datum_to_value(datum)?,
    ]))
}

fn value_to_datum(value: Value, span: SourceSpan) -> Result<Spanned<Datum>, EvalError> {
    let datum = match value {
        Value::Integer(n) => Datum::Atom(Atom::Integer(n)),
        Value::Rational(n) => Datum::Atom(Atom::Real(n.numer().clone(), n.denom().clone())),
        Value::Decimal(n) => Datum::Atom(Atom::Decimal(n)),
        Value::Complex(n) => Datum::Atom(Atom::Complex(n)),
        Value::Boolean(value) => Datum::Atom(Atom::Boolean(value)),
        Value::Character(ch) => Datum::Atom(Atom::Character(ch)),
        Value::String(text) => Datum::Atom(Atom::String(text.borrow().clone())),
        Value::Symbol(name) => Datum::Atom(Atom::Identifier(name)),
        Value::List(items) => Datum::List(
            items
                .into_iter()
                .map(|item| value_to_datum(item, span.clone()))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Value::Pair(pair) => {
            let pair = pair.borrow();
            return pair_to_datum(pair.car.clone(), pair.cdr.clone(), span);
        }
        Value::Vector(items) => Datum::Vector(
            items
                .borrow()
                .clone()
                .into_iter()
                .map(|item| value_to_datum(item, span.clone()))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        _ => {
            return Err(EvalError::TypeError {
                expected: "datum?",
                span,
            });
        }
    };

    Ok(Spanned::new(datum, span))
}

fn pair_to_datum(car: Value, cdr: Value, span: SourceSpan) -> Result<Spanned<Datum>, EvalError> {
    let mut items = vec![value_to_datum(car, span.clone())?];
    let mut tail = cdr;

    loop {
        match tail {
            Value::List(values) => {
                items.extend(
                    values
                        .into_iter()
                        .map(|value| value_to_datum(value, span.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                );
                return Ok(Spanned::new(Datum::List(items), span));
            }
            Value::Pair(pair) => {
                let pair = pair.borrow();
                items.push(value_to_datum(pair.car.clone(), span.clone())?);
                tail = pair.cdr.clone();
            }
            value => {
                let tail = value_to_datum(value, span.clone())?;
                return Ok(Spanned::new(Datum::DottedList(items, Box::new(tail)), span));
            }
        }
    }
}

fn eval_quasiquote(datum: &Spanned<Datum>, env: &Env, level: usize) -> Result<Value, EvalError> {
    match &datum.node {
        Datum::Unquote(inner) if level == 0 => eval_unquoted(inner, env),
        Datum::Unquote(inner) => Ok(list_value(vec![
            Value::Symbol("unquote".to_string()),
            eval_quasiquote(inner, env, level - 1)?,
        ])),
        Datum::UnquoteSplicing(_) if level == 0 => Err(EvalError::TypeError {
            expected: "unquote-splicing inside quasiquote list",
            span: datum.span.clone(),
        }),
        Datum::UnquoteSplicing(inner) => Ok(list_value(vec![
            Value::Symbol("unquote-splicing".to_string()),
            eval_quasiquote(inner, env, level - 1)?,
        ])),
        Datum::Quasiquote(inner) => Ok(list_value(vec![
            Value::Symbol("quasiquote".to_string()),
            eval_quasiquote(inner, env, level + 1)?,
        ])),
        Datum::List(items) => eval_quasiquote_list(items, None, env, level),
        Datum::DottedList(items, tail) => {
            eval_quasiquote_list(items, Some(tail.as_ref()), env, level)
        }
        Datum::Vector(items) => eval_quasiquote_items(items, env, level)
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
    let values = eval_quasiquote_items(items, env, level)?;

    match tail {
        Some(tail) => {
            let tail = eval_quasiquote(tail, env, level)?;
            Ok(values
                .into_iter()
                .rev()
                .fold(tail, |tail, head| cons_value(head, tail)))
        }
        None => Ok(list_value(values)),
    }
}

fn eval_quasiquote_items(
    items: &[Spanned<Datum>],
    env: &Env,
    level: usize,
) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::new();

    for item in items {
        match &item.node {
            Datum::UnquoteSplicing(inner) if level == 0 => {
                let spliced = eval_unquoted(inner, env)?;
                values.extend(expect_list_items(&spliced, inner.span.clone())?);
            }
            _ => values.push(eval_quasiquote(item, env, level)?),
        }
    }

    Ok(values)
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
        Atom::Real(n, d) => exact_number(BigRational::new(n.clone(), d.clone())),
        Atom::Complex(n) => Value::Complex(n.clone()),
        Atom::String(text) => string_value(text.clone()),
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

fn empty_list() -> Value {
    Value::List(Vec::new())
}

fn list_value(items: Vec<Value>) -> Value {
    items
        .into_iter()
        .rev()
        .fold(empty_list(), |tail, head| cons_value(head, tail))
}

fn is_empty_list(value: &Value) -> bool {
    matches!(value, Value::List(items) if items.is_empty())
}

fn list_items(value: &Value) -> Option<Vec<Value>> {
    let mut items = Vec::new();
    let mut tail = value.clone();
    let mut seen = HashSet::new();

    loop {
        match tail {
            Value::List(values) => {
                items.extend(values);
                return Some(items);
            }
            Value::Pair(pair) => {
                if !seen.insert(Rc::as_ptr(&pair)) {
                    return None;
                }
                let (car, cdr) = {
                    let pair = pair.borrow();
                    (pair.car.clone(), pair.cdr.clone())
                };
                items.push(car);
                tail = cdr;
            }
            _ => return None,
        }
    }
}

fn is_proper_list(value: &Value) -> bool {
    list_items(value).is_some()
}

fn expect_list_items(value: &Value, span: SourceSpan) -> Result<Vec<Value>, EvalError> {
    list_items(value).ok_or(EvalError::TypeError {
        expected: "list?",
        span,
    })
}

fn list_tail_at(value: Value, index: usize, span: SourceSpan) -> Result<Value, EvalError> {
    let mut tail = value;
    for _ in 0..index {
        match tail {
            Value::Pair(pair) => {
                tail = pair.borrow().cdr.clone();
            }
            Value::List(items) if !items.is_empty() => {
                tail = list_value(items[1..].to_vec());
            }
            _ => {
                return Err(EvalError::TypeError {
                    expected: "valid list index",
                    span,
                });
            }
        }
    }

    if is_proper_list(&tail) {
        Ok(tail)
    } else {
        Err(EvalError::TypeError {
            expected: "list?",
            span,
        })
    }
}

fn cons_value(head: Value, tail: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new(PairValue {
        car: head,
        cdr: tail,
    })))
}

fn string_value(text: impl Into<String>) -> Value {
    Value::String(Rc::new(RefCell::new(text.into())))
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_value(self, f, &mut WriteStack::default())
    }
}

#[derive(Default)]
struct WriteStack {
    pairs: HashSet<PairPointer>,
    vectors: HashSet<VectorPointer>,
}

fn write_value(value: &Value, f: &mut fmt::Formatter<'_>, stack: &mut WriteStack) -> fmt::Result {
    match value {
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
        Value::String(text) => write_string_literal(f, &text.borrow()),
        Value::Symbol(name) => write!(f, "{name}"),
        Value::List(items) => write_list_items(items, f, stack),
        Value::Pair(pair) if stack.pairs.contains(&Rc::as_ptr(pair)) => {
            write!(f, "#<circular-pair>")
        }
        Value::Pair(pair) => write_pair_value(pair.clone(), f, stack),
        Value::Vector(items) if stack.vectors.contains(&Rc::as_ptr(items)) => {
            write!(f, "#<circular-vector>")
        }
        Value::Vector(items) => {
            let pointer = Rc::as_ptr(items);
            stack.vectors.insert(pointer);
            write!(f, "#(")?;
            for (index, item) in items.borrow().iter().enumerate() {
                if index > 0 {
                    write!(f, " ")?;
                }
                write_value(item, f, stack)?;
            }
            write!(f, ")")?;
            stack.vectors.remove(&pointer);
            Ok(())
        }
        Value::InputPort(_) => write!(f, "#<input-port>"),
        Value::OutputPort(_) => write!(f, "#<output-port>"),
        Value::Promise(_) => write!(f, "#<promise>"),
        Value::Values(values) => {
            write!(f, "(values")?;
            for value in values {
                write!(f, " ")?;
                write_value(value, f, stack)?;
            }
            write!(f, ")")
        }
        Value::EofObject => write!(f, "#<eof>"),
        Value::Environment(_) => write!(f, "#<environment>"),
        Value::Continuation(_) => write!(f, "#<continuation>"),
        Value::Procedure(_) | Value::Primitive(_) => write!(f, "#<procedure>"),
        Value::Unspecified => write!(f, "#<unspecified>"),
        Value::Uninitialized => write!(f, "#<uninitialized>"),
    }
}

fn write_list_items(
    items: &[Value],
    f: &mut fmt::Formatter<'_>,
    stack: &mut WriteStack,
) -> fmt::Result {
    write!(f, "(")?;
    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            write!(f, " ")?;
        }
        write_value(item, f, stack)?;
    }
    write!(f, ")")
}

fn write_pair_value(
    pair: Rc<RefCell<PairValue>>,
    f: &mut fmt::Formatter<'_>,
    stack: &mut WriteStack,
) -> fmt::Result {
    let mut inserted = Vec::new();
    write!(f, "(")?;
    let mut tail = Value::Pair(pair);
    let mut first = true;

    loop {
        match tail {
            Value::Pair(pair) => {
                let pointer = Rc::as_ptr(&pair);
                if !stack.pairs.insert(pointer) {
                    write!(f, " . #<circular-pair>)")?;
                    remove_pair_stack_entries(stack, inserted);
                    return Ok(());
                }
                inserted.push(pointer);
                let (car, cdr) = {
                    let pair = pair.borrow();
                    (pair.car.clone(), pair.cdr.clone())
                };
                if !first {
                    write!(f, " ")?;
                }
                write_value(&car, f, stack)?;
                tail = cdr;
                first = false;
            }
            Value::List(items) if items.is_empty() => {
                write!(f, ")")?;
                remove_pair_stack_entries(stack, inserted);
                return Ok(());
            }
            Value::List(items) => {
                for item in items {
                    if !first {
                        write!(f, " ")?;
                    }
                    write_value(&item, f, stack)?;
                    first = false;
                }
                write!(f, ")")?;
                remove_pair_stack_entries(stack, inserted);
                return Ok(());
            }
            value => {
                write!(f, " . ")?;
                write_value(&value, f, stack)?;
                write!(f, ")")?;
                remove_pair_stack_entries(stack, inserted);
                return Ok(());
            }
        }
    }
}

fn remove_pair_stack_entries(stack: &mut WriteStack, inserted: Vec<PairPointer>) {
    for pointer in inserted {
        stack.pairs.remove(&pointer);
    }
}

fn write_string_literal(f: &mut fmt::Formatter<'_>, text: &str) -> fmt::Result {
    write!(f, "\"")?;
    for ch in text.chars() {
        match ch {
            '"' => write!(f, "\\\"")?,
            '\\' => write!(f, "\\\\")?,
            '\n' => write!(f, "\\n")?,
            '\t' => write!(f, "\\t")?,
            ch => write!(f, "{ch}")?,
        }
    }
    write!(f, "\"")
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use super::{
        EvalError, OutputMode, OutputPort, TRANSCRIPT_PORT, Value, mirror_transcript, newline,
        output_value, string_value, write_char,
    };

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

    fn eval_error(input: &str) -> EvalError {
        let datums = parse(input).unwrap();
        let program = classify_program(&datums).unwrap();
        let env = Env::new();
        eval_program(&program, &env).unwrap_err()
    }

    #[test]
    fn displays_eval_error_details() {
        assert_eq!(
            eval_error("(car)").to_string(),
            "wrong number of arguments: expected 1, got 0"
        );
    }

    #[test]
    fn evaluates_top_level_syntax_rules_macros() {
        assert_eq!(
            eval_one(
                "(define-syntax when
                   (syntax-rules ()
                     ((when test body ...)
                      (if test (begin body ...)))))
                 (define x 0)
                 (when #t (set! x 1) (set! x (+ x 1)))
                 x"
            ),
            "2"
        );
        assert_eq!(
            eval_one(
                "(define-syntax pick
                   (syntax-rules (else)
                     ((pick else value) value)
                     ((pick other value) other)))
                 (pick else 4)"
            ),
            "4"
        );
        assert_eq!(
            eval_one(
                "(define-syntax pick
                   (syntax-rules (else)
                     ((pick else value) value)
                     ((pick other value) other)))
                 (pick 3 4)"
            ),
            "3"
        );
        assert_eq!(
            eval_one(
                "(begin
                   (define-syntax id
                     (syntax-rules ()
                       ((id x) x)))
                   (define x (id 4)))
                 x"
            ),
            "4"
        );
        assert_eq!(
            eval_one(
                "(define-syntax defs
                   (syntax-rules ()
                     ((defs) (begin (define x 1) (define y 2)))))
                 (defs)
                 (+ x y)"
            ),
            "3"
        );
        assert_eq!(
            eval_one(
                "(let ((x 0))
                   (let-syntax
                     ((twice
                        (syntax-rules ()
                          ((twice expr) (begin expr expr)))))
                     (twice (set! x (+ x 1)))
                     x))"
            ),
            "2"
        );
        assert_eq!(
            eval_one(
                "(letrec-syntax
                   ((a (syntax-rules () ((a x) (b x))))
                    (b (syntax-rules () ((b x) (+ x 1)))))
                   (a 4))"
            ),
            "5"
        );
        assert_eq!(
            eval_one(
                "(define-syntax install-id
                   (syntax-rules ()
                     ((install-id)
                      (define-syntax id
                        (syntax-rules ()
                          ((id x) x))))))
                 (install-id)
                 (id 9)"
            ),
            "9"
        );
        assert_eq!(
            eval_one(
                "(define-syntax keep
                   (syntax-rules ()
                     ((keep _) _)))
                 (keep 11)"
            ),
            "11"
        );
        assert_eq!(
            eval_one(
                "(define-syntax tail
                   (syntax-rules ()
                     ((tail head . rest) 'rest)))
                 (tail 1 2 3)"
            ),
            "(2 3)"
        );
        assert_eq!(
            eval_one(
                "(define-syntax tail
                   (syntax-rules ()
                     ((tail head . rest) 'rest)))
                 (tail 1 . 2)"
            ),
            "2"
        );
        assert_eq!(
            eval_one(
                "(define-syntax split
                   (syntax-rules ()
                     ((split x ... . rest) (list 'x ... 'rest))))
                 (split 1 2 . 3)"
            ),
            "(1 2 3)"
        );
    }

    #[test]
    fn evaluates_internal_syntax_definitions() {
        assert_eq!(
            eval_one(
                "((lambda ()
                    (define-syntax id
                      (syntax-rules ()
                        ((id x) x)))
                    (id 5)))"
            ),
            "5"
        );
        assert_eq!(
            eval_one(
                "(define (use-id)
                   (define-syntax id
                     (syntax-rules ()
                       ((id x) x)))
                   (id 7))
                 (use-id)"
            ),
            "7"
        );
        assert_eq!(
            eval_one(
                "(let ()
                   (define-syntax twice
                     (syntax-rules ()
                       ((twice expr) (begin expr expr))))
                   (define x 0)
                   (twice (set! x (+ x 1)))
                   x)"
            ),
            "2"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((x (syntax-rules () ((x) 9))))
                   ((lambda (x) x) 4))"
            ),
            "4"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((x (syntax-rules () ((x) 9))))
                   (define (f x) x)
                   (f 4))"
            ),
            "4"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((x (syntax-rules () ((x) 9))))
                   (let ((x 4)) x))"
            ),
            "4"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((x (syntax-rules () ((x) 9))))
                   (let* ((x 4)) x))"
            ),
            "4"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((x (syntax-rules () ((x) 9))))
                   (letrec ((x (lambda () 4))) (x)))"
            ),
            "4"
        );
    }

    #[test]
    fn preserves_cond_and_case_clause_syntax_during_macro_expansion() {
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((else (syntax-rules () ((else) 99))))
                   (cond (else 1)))"
            ),
            "1"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((=> (syntax-rules () ((=>) 99))))
                   (cond ((+ 1 1) => (lambda (x) x))))"
            ),
            "2"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((else (syntax-rules () ((else) 99))))
                   (case 'x (else 1)))"
            ),
            "1"
        );
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((a (syntax-rules () ((a) 99))))
                   (case 'a ((a) 1) (else 2)))"
            ),
            "1"
        );
    }

    #[test]
    fn preserves_do_binding_syntax_during_macro_expansion() {
        assert_eq!(
            eval_one(
                "(let-syntax
                   ((i (syntax-rules () ((i) 99))))
                   (do ((i 0 (+ i 1)))
                       ((= i 3) i)))"
            ),
            "3"
        );
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
    fn evaluates_numeric_predicates_and_integer_operations() {
        assert_eq!(eval_one("(complex? 1+2i)"), "#t");
        assert_eq!(eval_one("(real? 1+0i)"), "#t");
        assert_eq!(eval_one("(rational? 1+2i)"), "#f");
        assert_eq!(eval_one("(integer? 2.0)"), "#t");
        assert_eq!(eval_one("(exact? 1/2)"), "#t");
        assert_eq!(eval_one("(inexact? 1.5)"), "#t");
        assert_eq!(eval_one("(zero? 0+0i)"), "#t");
        assert_eq!(eval_one("(positive? 3/2)"), "#t");
        assert_eq!(eval_one("(negative? -1)"), "#t");
        assert_eq!(eval_one("(odd? 5)"), "#t");
        assert_eq!(eval_one("(even? 4)"), "#t");
        assert_eq!(eval_one("(max 1 5 3)"), "5");
        assert_eq!(eval_one("(min 3 1/2 2)"), "1/2");
        assert_eq!(eval_one("(abs -5/2)"), "5/2");
        assert_eq!(eval_one("(quotient 13 5)"), "2");
        assert_eq!(eval_one("(remainder -13 5)"), "-3");
        assert_eq!(eval_one("(modulo -13 5)"), "2");
        assert_eq!(eval_one("(gcd 32 -36)"), "4");
        assert_eq!(eval_one("(lcm 4 6)"), "12");
        assert_eq!(eval_one("(numerator 6/8)"), "3");
        assert_eq!(eval_one("(denominator 6/8)"), "4");
        assert_eq!(eval_one("(floor 3/2)"), "1");
        assert_eq!(eval_one("(ceiling 3/2)"), "2");
        assert_eq!(eval_one("(truncate -3/2)"), "-1");
        assert_eq!(eval_one("(round 5/2)"), "2");
        assert_eq!(eval_one("(round 7/2)"), "4");
        assert_eq!(eval_one("(floor -1.2)"), "-2");
        assert_eq!(eval_one("(ceiling -1.2)"), "-1");
        assert_eq!(eval_one("(exact->inexact 1/2)"), "0.5");
        assert_eq!(eval_one("(inexact->exact 1.25)"), "5/4");
        assert_eq!(eval_one("(inexact->exact 2.0)"), "2");
        assert_eq!(eval_one("(inexact? (exact->inexact 1))"), "#t");
        assert_eq!(eval_one("(exact? (inexact->exact 1.25))"), "#t");
        assert_eq!(eval_one("(make-rectangular 1 2)"), "1+2i");
        assert_eq!(eval_one("(make-polar 2 0)"), "2+0i");
        assert_eq!(eval_one("(real-part 1+2i)"), "1");
        assert_eq!(eval_one("(imag-part 1+2i)"), "2");
        assert_eq!(eval_one("(imag-part 5)"), "0");
        assert_eq!(eval_one("(magnitude 3+4i)"), "5");
        assert_eq!(eval_one("(angle 1+0i)"), "0");
        assert_eq!(eval_one("(exp 0)"), "1");
        assert_eq!(eval_one("(log 1)"), "0");
        assert_eq!(eval_one("(sin 0)"), "0");
        assert_eq!(eval_one("(cos 0)"), "1");
        assert_eq!(eval_one("(tan 0)"), "0");
        assert_eq!(eval_one("(asin 0)"), "0");
        assert_eq!(eval_one("(acos 1)"), "0");
        assert_eq!(eval_one("(atan 0)"), "0");
        assert_eq!(eval_one("(rationalize 1.3 0.1)"), "4/3");
        assert_eq!(eval_one("(rationalize 1/3 1/100)"), "1/3");
        assert_eq!(
            eval_one("(+ (make-rectangular 1 2) (make-rectangular 3 4))"),
            "4+6i"
        );
        assert_eq!(eval_one("(sqrt 4)"), "2");
        assert_eq!(eval_one("(sqrt -4)"), "0+2i");
        assert_eq!(eval_one("(expt 2 3)"), "8");
        assert_eq!(eval_one("(expt 2 -1)"), "1/2");
        assert_eq!(eval_one("(expt 1.5 2)"), "2.25");
        assert_eq!(eval_one("(expt 1+2i 2)"), "-3+4i");
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
    fn evaluates_top_level_begin_splicing() {
        assert_eq!(eval_one("(begin (define x 1) x)"), "1");
        assert_eq!(eval_one("(begin (begin (define x 1)) x)"), "1");
    }

    #[test]
    fn evaluates_rest_lambda_formals() {
        assert_eq!(eval_one("((lambda args args) 1 2 3)"), "(1 2 3)");
        assert_eq!(eval_one("((lambda (x y . rest) rest) 1 2 3 4)"), "(3 4)");
        assert_eq!(
            eval_one("(define (collect x . rest) rest) (collect 1 2 3)"),
            "(2 3)"
        );
        assert_eq!(eval_one("((lambda (x . rest) rest) 1)"), "()");
    }

    #[test]
    fn evaluates_internal_definitions() {
        assert_eq!(eval_one("((lambda () (define x 1) x))"), "1");
        assert_eq!(
            eval_one("((lambda () (define (add1 x) (+ x 1)) (add1 4)))"),
            "5"
        );
        assert_eq!(eval_one("(let () (define x 1) x)"), "1");
        assert_eq!(eval_one("(let* () (define x 1) x)"), "1");
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
    fn evaluates_tail_calls_iteratively() {
        assert_eq!(
            eval_one("(let loop ((n 5000)) (if (= n 0) 'done (loop (- n 1))))"),
            "done"
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
    fn evaluates_multiple_values() {
        assert_eq!(
            eval_one("(call-with-values (lambda () (values 1 2)) +)"),
            "3"
        );
        assert_eq!(
            eval_one("(call-with-values (lambda () 4) (lambda (x) (+ x 1)))"),
            "5"
        );
        assert_eq!(eval_one("(values 1 2)"), "(values 1 2)");
    }

    #[test]
    fn evaluates_dynamic_wind() {
        assert_eq!(
            eval_one("(dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))"),
            "2"
        );
        assert_eq!(
            eval_one(
                "(define xs '())
                 (dynamic-wind
                   (lambda () (set! xs (append xs '(before))))
                   (lambda () (set! xs (append xs '(during))) 'value)
                   (lambda () (set! xs (append xs '(after)))))
                 xs"
            ),
            "(before during after)"
        );
    }

    #[test]
    fn evaluates_escape_continuations() {
        assert_eq!(eval_one("(call/cc (lambda (k) 42))"), "42");
        assert_eq!(eval_one("(call/cc (lambda (k) (procedure? k)))"), "#t");
        assert_eq!(eval_one("(+ 1 (call/cc (lambda (k) (k 5) 10)))"), "6");
        assert_eq!(
            eval_one(
                "(call-with-current-continuation
                   (lambda (exit)
                     (for-each
                       (lambda (x) (if (= x 3) (exit x) #f))
                       '(1 2 3 4))
                     0))"
            ),
            "3"
        );
    }

    #[test]
    fn evaluates_if_with_scheme_truthiness() {
        assert_eq!(eval_one("(if #f 1 2)"), "2");
        assert_eq!(eval_one("(if #F 1 2)"), "2");
        assert_eq!(eval_one("(if #T 1 2)"), "1");
        assert_eq!(eval_one("(if '() 1 2)"), "1");
    }

    #[test]
    fn evaluates_list_reverse_and_char_comparison() {
        assert_eq!(eval_one("(reverse (list 1 2 3))"), "(3 2 1)");
        assert_eq!(eval_one("(char=? #\\a #\\a)"), "#t");
        assert_eq!(eval_one("(char<? #\\a #\\b)"), "#t");
        assert_eq!(eval_one("(char<=? #\\a #\\a #\\b)"), "#t");
        assert_eq!(eval_one("(char>=? #\\b #\\a #\\a)"), "#t");
        assert_eq!(eval_one("(char-ci=? #\\A #\\a)"), "#t");
        assert_eq!(eval_one("(char-ci<? #\\a #\\B)"), "#t");
        assert_eq!(eval_one("(eqv? 'a 'a)"), "#t");
    }

    #[test]
    fn evaluates_character_classification_and_case() {
        assert_eq!(eval_one("(char-alphabetic? #\\a)"), "#t");
        assert_eq!(eval_one("(char-numeric? #\\7)"), "#t");
        assert_eq!(eval_one("(char-whitespace? #\\space)"), "#t");
        assert_eq!(eval_one("(char-upper-case? #\\A)"), "#t");
        assert_eq!(eval_one("(char-lower-case? #\\a)"), "#t");
        assert_eq!(eval_one("(char-upcase #\\a)"), "#\\A");
        assert_eq!(eval_one("(char-downcase #\\A)"), "#\\a");
    }

    #[test]
    fn evaluates_equality_predicates() {
        assert_eq!(eval_one("(eq? 'a 'a)"), "#t");
        assert_eq!(eval_one("(equal? '(1 (2)) '(1 (2)))"), "#t");
        assert_eq!(eval_one("(equal? (vector 1 2) (vector 1 2))"), "#t");
        assert_eq!(eval_one("(eq? (vector 1 2) (vector 1 2))"), "#f");
        assert_eq!(eval_one("(define v (vector 1 2)) (eq? v v)"), "#t");
        assert_eq!(
            eval_one("(define p (cons 1 '())) (set-cdr! p p) (equal? p p)"),
            "#t"
        );
        assert_eq!(
            eval_one(
                "(define p (cons 1 '()))
                 (define q (cons 1 '()))
                 (set-cdr! p p)
                 (set-cdr! q q)
                 (equal? p q)"
            ),
            "#t"
        );
        assert_eq!(
            eval_one(
                "(define p (cons 1 '()))
                 (define q (cons 1 '()))
                 (set-cdr! p p)
                 (equal? p q)"
            ),
            "#f"
        );
        assert_eq!(
            eval_one("(define v (vector 1)) (vector-set! v 0 v) (equal? v v)"),
            "#t"
        );
        assert_eq!(
            eval_one(
                "(define v (vector 1))
                 (define w (vector 1))
                 (vector-set! v 0 v)
                 (vector-set! w 0 w)
                 (equal? v w)"
            ),
            "#t"
        );
        assert_eq!(
            eval_one(
                "(define v (vector 1))
                 (define w (vector 1))
                 (vector-set! v 0 v)
                 (equal? v w)"
            ),
            "#f"
        );
    }

    #[test]
    fn evaluates_string_comparisons() {
        assert_eq!(eval_one("(string=? \"a\" \"a\")"), "#t");
        assert_eq!(eval_one("(string<? \"a\" \"b\")"), "#t");
        assert_eq!(eval_one("(string>? \"b\" \"a\")"), "#t");
        assert_eq!(eval_one("(string<=? \"a\" \"a\" \"b\")"), "#t");
        assert_eq!(eval_one("(string>=? \"b\" \"a\" \"a\")"), "#t");
        assert_eq!(eval_one("(string-ci=? \"A\" \"a\")"), "#t");
        assert_eq!(eval_one("(string-ci<? \"a\" \"B\")"), "#t");
    }

    #[test]
    fn evaluates_string_construction_access_conversion_and_mutation() {
        assert_eq!(eval_one("(make-string 3 #\\x)"), "\"xxx\"");
        assert_eq!(eval_one("(string #\\a #\\b)"), "\"ab\"");
        assert_eq!(eval_one("(string-ref \"abc\" 1)"), "#\\b");
        assert_eq!(eval_one("(substring \"abcdef\" 1 4)"), "\"bcd\"");
        assert_eq!(eval_one("(string-append \"a\" \"b\" \"c\")"), "\"abc\"");
        assert_eq!(eval_one("(string->list \"ab\")"), "(#\\a #\\b)");
        assert_eq!(eval_one("(list->string '(#\\a #\\b))"), "\"ab\"");
        assert_eq!(
            eval_one("(define s (string #\\a #\\b)) (define t s) (string-set! t 0 #\\z) s"),
            "\"zb\""
        );
        assert_eq!(
            eval_one(
                "(define s (string #\\a #\\b)) (define c (string-copy s)) (string-set! s 0 #\\z) c"
            ),
            "\"ab\""
        );
        assert_eq!(
            eval_one("(define s (string #\\a #\\b)) (string-fill! s #\\x) s"),
            "\"xx\""
        );
    }

    #[test]
    fn evaluates_unimplemented_resource_predicates_conservatively() {
        assert_eq!(eval_one("(port? 1)"), "#f");
        assert_eq!(eval_one("(input-port? 1)"), "#f");
        assert_eq!(eval_one("(output-port? 1)"), "#f");
        assert_eq!(eval_one("(eof-object? 1)"), "#f");
    }

    #[test]
    fn evaluates_input_primitives() {
        let path = std::env::temp_dir().join(format!("lavu-input-{}.ss", std::process::id()));
        std::fs::write(&path, "ab").unwrap();
        let input = format!(
            "(define p (open-input-file \"{}\"))
             (list (input-port? p)
                   (port? p)
                   (char-ready? p)
                   (read-char p)
                   (peek-char p)
                   (read-char p)
                   (eof-object? (read-char p))
                   (begin (close-input-port p) 'closed))",
            path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "(#t #t #t #\\a #\\b #\\b #t closed)");
        std::fs::remove_file(path).unwrap();
        assert_eq!(eval_one("(current-input-port)"), "#<input-port>");

        let read_path = std::env::temp_dir().join(format!("lavu-read-{}.ss", std::process::id()));
        std::fs::write(&read_path, "(a 1) #\\z").unwrap();
        let input = format!(
            "(define p (open-input-file \"{}\"))
             (list (read p) (read p) (eof-object? (read p)))",
            read_path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "((a 1) #\\z #t)");
        std::fs::remove_file(read_path).unwrap();

        let call_path =
            std::env::temp_dir().join(format!("lavu-call-input-{}.ss", std::process::id()));
        std::fs::write(&call_path, "(ok 1)").unwrap();
        let input = format!(
            "(call-with-input-file \"{}\" (lambda (p) (read p)))",
            call_path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "(ok 1)");
        std::fs::remove_file(call_path).unwrap();

        let with_path =
            std::env::temp_dir().join(format!("lavu-with-input-{}.ss", std::process::id()));
        std::fs::write(&with_path, "(current 2)").unwrap();
        let input = format!(
            "(with-input-from-file \"{}\" (lambda () (read)))",
            with_path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "(current 2)");
        std::fs::remove_file(with_path).unwrap();
    }

    #[test]
    fn evaluates_load_in_current_environment() {
        let path = std::env::temp_dir().join(format!("lavu-load-{}.ss", std::process::id()));
        std::fs::write(&path, "(define loaded 41) (+ loaded 1)").unwrap();
        assert_eq!(
            eval_one(&format!("(load \"{}\")", path.to_string_lossy())),
            "42"
        );
        let input = format!("(load \"{}\") loaded", path.to_string_lossy());

        assert_eq!(eval_one(&input), "41");
        std::fs::remove_file(path).unwrap();
    }

    #[test]
    fn evaluates_eval_with_environments() {
        assert_eq!(
            eval_one("(eval '(+ 1 2) (scheme-report-environment 5))"),
            "3"
        );
        assert_eq!(
            eval_one("(eval '((lambda (x) x) 7) (null-environment 5))"),
            "7"
        );
        assert_eq!(
            eval_one("(define x 4) (eval 'x (interaction-environment))"),
            "4"
        );
        assert_eq!(eval_one("(interaction-environment)"), "#<environment>");
    }

    #[test]
    fn evaluates_conversion_primitives() {
        assert_eq!(eval_one("(symbol->string 'hello)"), "\"hello\"");
        assert_eq!(eval_one("(string->symbol \"hello\")"), "hello");
        assert_eq!(eval_one("(char->integer #\\A)"), "65");
        assert_eq!(eval_one("(integer->char 65)"), "#\\A");
        assert_eq!(eval_one("(number->string 1/2)"), "\"1/2\"");
        assert_eq!(eval_one("(number->string 16 16)"), "\"10\"");
        assert_eq!(eval_one("(number->string 10 2)"), "\"1010\"");
        assert_eq!(eval_one("(string->number \"#x10\")"), "16");
        assert_eq!(eval_one("(string->number \"#x-ff\")"), "-255");
        assert_eq!(eval_one("(string->number \"#b+1010\")"), "10");
        assert_eq!(eval_one("(string->number \"#e1.5\")"), "3/2");
        assert_eq!(eval_one("(string->number \"#i1/2\")"), "0.5");
        assert_eq!(eval_one("(string->number \"#i#x10\")"), "16");
        assert_eq!(eval_one("(string->number \"#x#i10\" 2)"), "16");
        assert_eq!(eval_one("(string->number \"#b101/10\")"), "5/2");
        assert_eq!(eval_one("(string->number \"#d3/2\")"), "3/2");
        assert_eq!(eval_one("(string->number \"#d1.5\")"), "1.5");
        assert_eq!(eval_one("(string->number \"#x10/4\")"), "4");
        assert_eq!(eval_one("(string->number \"#i#b101/10\")"), "2.5");
        assert_eq!(eval_one("(string->number \".5\")"), "0.5");
        assert_eq!(eval_one("(string->number \"1.\")"), "1");
        assert_eq!(eval_one("(= (string->number \"1e2\") 100)"), "#t");
        assert_eq!(eval_one("(string->number \"#e1e2\")"), "100");
        assert_eq!(eval_one("(string->number \"#e1.25e1\")"), "25/2");
        assert_eq!(eval_one("(string->number \"#e1.25e-1\")"), "1/8");
        assert_eq!(eval_one("(string->number \"1/0\")"), "#f");
        assert_eq!(eval_one("(string->number \"#x10/0\")"), "#f");
        assert_eq!(eval_one("(string->number \"10\" 16)"), "16");
        assert_eq!(eval_one("(string->number \"101\" 2)"), "5");
        assert_eq!(eval_one("(string->number \"1.5\" 10)"), "1.5");
        assert_eq!(eval_one("(string->number \"12\" 2)"), "#f");
        assert_eq!(eval_one("(string->number \"wat\")"), "#f");
        assert_eq!(
            eval_one("(list (exact? #e1.5) (inexact? #i1/2))"),
            "(#t #t)"
        );
    }

    #[test]
    fn evaluates_output_primitives() {
        assert_eq!(eval_one("(output-port? (current-output-port))"), "#t");
        assert_eq!(eval_one("(port? (current-output-port))"), "#t");
        assert_eq!(eval_one("(current-output-port)"), "#<output-port>");
    }

    #[test]
    fn writes_to_output_ports() {
        let output = std::rc::Rc::new(std::cell::RefCell::new(String::new()));
        let port = Value::OutputPort(OutputPort::Buffer(output.clone()));

        output_value(
            vec![string_value("x"), port.clone()],
            0..0,
            OutputMode::Write,
        )
        .unwrap();
        newline(vec![port.clone()], 0..0).unwrap();
        output_value(
            vec![string_value("y"), port.clone()],
            0..0,
            OutputMode::Display,
        )
        .unwrap();
        write_char(vec![Value::Character('z'), port], 0..0).unwrap();

        assert_eq!(*output.borrow(), "\"x\"\nyz");
    }

    #[test]
    fn writes_escaped_string_literals() {
        let output = std::rc::Rc::new(std::cell::RefCell::new(String::new()));
        let port = Value::OutputPort(OutputPort::Buffer(output.clone()));

        output_value(
            vec![string_value("a\"b\\c\n"), port.clone()],
            0..0,
            OutputMode::Write,
        )
        .unwrap();
        output_value(
            vec![string_value(" raw\" "), port],
            0..0,
            OutputMode::Display,
        )
        .unwrap();

        assert_eq!(*output.borrow(), "\"a\\\"b\\\\c\\n\" raw\" ");
    }

    #[test]
    fn writes_to_output_files() {
        let path = std::env::temp_dir().join(format!("lavu-output-{}.ss", std::process::id()));
        let input = format!(
            "(define p (open-output-file \"{}\"))
             (display \"x\" p)
             (write '(a 1) p)
             (close-output-port p)",
            path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "#<unspecified>");
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "x(a 1)");
        std::fs::remove_file(path).unwrap();

        let call_path =
            std::env::temp_dir().join(format!("lavu-call-output-{}.ss", std::process::id()));
        let input = format!(
            "(call-with-output-file \"{}\" (lambda (p) (display \"y\" p) (write '(b 2) p)))",
            call_path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "#<unspecified>");
        assert_eq!(std::fs::read_to_string(&call_path).unwrap(), "y(b 2)");
        std::fs::remove_file(call_path).unwrap();

        let with_path =
            std::env::temp_dir().join(format!("lavu-with-output-{}.ss", std::process::id()));
        let input = format!(
            "(with-output-to-file \"{}\" (lambda () (display \"z\") (write '(c 3))))",
            with_path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "#<unspecified>");
        assert_eq!(std::fs::read_to_string(&with_path).unwrap(), "z(c 3)");
        std::fs::remove_file(with_path).unwrap();
    }

    #[test]
    fn evaluates_transcript_primitives() {
        let path = std::env::temp_dir().join(format!(
            "lavu-transcript-{}-{}.ss",
            std::process::id(),
            line!()
        ));
        let input = format!(
            "(transcript-on \"{}\")
             (transcript-off)",
            path.to_string_lossy()
        );

        assert_eq!(eval_one(&input), "#<unspecified>");
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "");
        std::fs::remove_file(path).unwrap();
    }

    #[test]
    fn mirrors_stdout_to_active_transcript() {
        let transcript = Rc::new(RefCell::new(String::new()));
        let explicit = Rc::new(RefCell::new(String::new()));
        TRANSCRIPT_PORT.with(|current| {
            current.replace(Some(OutputPort::Buffer(transcript.clone())));
        });

        mirror_transcript("x", &OutputPort::Stdout, 0..0).unwrap();
        mirror_transcript("y", &OutputPort::Buffer(explicit.clone()), 0..0).unwrap();

        TRANSCRIPT_PORT.with(|current| {
            current.replace(None);
        });
        assert_eq!(*transcript.borrow(), "x");
        assert_eq!(*explicit.borrow(), "");
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
        assert_eq!(
            eval_one("(define v (vector 1)) (vector-set! v 0 v) v"),
            "#(#<circular-vector>)"
        );
    }

    #[test]
    fn evaluates_pair_and_list_primitives() {
        assert_eq!(eval_one("(cons 1 (list 2 3))"), "(1 2 3)");
        assert_eq!(eval_one("(cons 1 2)"), "(1 . 2)");
        assert_eq!(
            eval_one("(define p (cons 1 2)) (set-car! p 9) p"),
            "(9 . 2)"
        );
        assert_eq!(
            eval_one("(define p (cons 1 2)) (set-cdr! p 9) p"),
            "(1 . 9)"
        );
        assert_eq!(eval_one("(define p (list 1 2)) (set-car! p 9) p"), "(9 2)");
        assert_eq!(
            eval_one("(define p (list 1 2)) (define tail (cdr p)) (set-car! tail 9) p"),
            "(1 9)"
        );
        assert_eq!(
            eval_one("(define p (list 1 2)) (set-cdr! p (list 3 4)) p"),
            "(1 3 4)"
        );
        assert_eq!(eval_one("(pair? (list 1))"), "#t");
        assert_eq!(eval_one("(list? (cons 1 (cons 2 '())))"), "#t");
        assert_eq!(eval_one("(list? (cons 1 2))"), "#f");
        assert_eq!(
            eval_one("(define p (cons 1 '())) (set-cdr! p p) (list? p)"),
            "#f"
        );
        assert_eq!(
            eval_one("(define p (cons 1 '())) (set-cdr! p p) p"),
            "(1 . #<circular-pair>)"
        );
        assert_eq!(
            eval_one("(define p (cons 1 '())) (set-car! p p) p"),
            "(#<circular-pair>)"
        );
        assert_eq!(eval_one("(car (list 1 2 3))"), "1");
        assert_eq!(eval_one("(cdr (list 1 2 3))"), "(2 3)");
        assert_eq!(eval_one("(length (list 1 2 3))"), "3");
        assert_eq!(eval_one("(caar '((1 2) (3 4)))"), "1");
        assert_eq!(eval_one("(cadr '(1 2 3))"), "2");
        assert_eq!(eval_one("(caddr '(1 2 3))"), "3");
        assert_eq!(eval_one("(cadddr '(1 2 3 4))"), "4");
        assert_eq!(eval_one("(append (list 1) (list 2 3))"), "(1 2 3)");
        assert_eq!(eval_one("(append '(a b) '(c . d))"), "(a b c . d)");
        assert_eq!(eval_one("(append '() 'a)"), "a");
        assert_eq!(eval_one("(list-ref (list 'a 'b 'c) 1)"), "b");
        assert_eq!(eval_one("(list-tail (list 'a 'b 'c) 1)"), "(b c)");
    }

    #[test]
    fn evaluates_membership_primitives() {
        assert_eq!(eval_one("(memq 'b '(a b c))"), "(b c)");
        assert_eq!(eval_one("(memq 'x '(a b c))"), "#f");
        assert_eq!(eval_one("(member '(1) '((0) (1) (2)))"), "((1) (2))");
        assert!(matches!(
            eval_error("(define p (list 1)) (set-cdr! p p) (member 2 p)"),
            EvalError::TypeError {
                expected: "list?",
                ..
            }
        ));
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
        assert!(matches!(
            eval_error("(map 1 '())"),
            EvalError::NotProcedure { .. }
        ));
        assert!(matches!(
            eval_error("(for-each 1 '())"),
            EvalError::NotProcedure { .. }
        ));
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
        assert_eq!(eval_one("`#(1 ,(+ 1 1) 3)"), "#(1 2 3)");
        assert_eq!(eval_one("(let ((xs '(2 3))) `#(1 ,@xs 4))"), "#(1 2 3 4)");
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
        assert_eq!(
            eval_one("(cond ((+ 1 2) => (lambda (x) (+ x 10))) (else 0))"),
            "13"
        );
        assert_eq!(
            eval_one("(define x 0) (cond ((begin (set! x (+ x 1)) x)) (else 0)) x"),
            "1"
        );
        assert_eq!(
            eval_one("(define x 0) (cond ((begin (set! x (+ x 1)) x) => (lambda (v) x)) (else 0))"),
            "1"
        );
    }

    #[test]
    fn evaluates_desugared_case() {
        assert_eq!(eval_one("(case 'b ((a c) 10) ((b d) 20) (else 30))"), "20");
    }
}
