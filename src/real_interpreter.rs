use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Range;
use std::rc::Rc;

use crate::ast::ast1::Atom;
use crate::ast::ast16::{Definition, Expression, Program, TopLevelForm};
use bigdecimal::BigDecimal;
use num::{BigInt, BigRational, Complex};

/// Represents a Scheme value
#[derive(Clone)]
pub enum Value {
    // Exact numeric tower
    Integer(BigInt),
    Rational(BigRational),
    Real(BigDecimal),
    Complex(Complex<BigDecimal>),

    // Other basic types
    Boolean(bool),
    Character(char),
    String(String),
    Symbol(String),

    // Compound values
    Pair(Rc<(Value, Value)>),
    Vector(Rc<Vec<Value>>),
    Nil,

    // Procedures
    Procedure(Rc<Procedure>),
    PrimitiveProcedure(
        String,
        Rc<dyn Fn(&[Value], &mut Interpreter) -> Result<Value, String>>,
    ),

    // Continuations
    Continuation(Rc<Continuation>),
}

// Manual Debug implementation for Value
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "Boolean({:?})", b),
            Value::Integer(n) => write!(f, "Integer({:?})", n),
            Value::Rational(r) => write!(f, "Rational({:?}/{:?})", r.numer(), r.denom()),
            Value::Real(d) => write!(f, "Real({:?})", d),
            Value::Complex(c) => write!(f, "Complex({:?}+{:?}i)", c.re, c.im),
            Value::Character(c) => write!(f, "Character({:?})", c),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Symbol(s) => write!(f, "Symbol({:?})", s),
            Value::Pair(p) => write!(f, "Pair({:?}, {:?})", p.0, p.1),
            Value::Vector(v) => write!(f, "Vector({:?})", v),
            Value::Nil => write!(f, "Nil"),
            Value::Procedure(p) => write!(f, "Procedure({:?})", p),
            Value::PrimitiveProcedure(name, _) => {
                write!(f, "PrimitiveProcedure({:?}, <function>)", name)
            }
            Value::Continuation(_) => write!(f, "Continuation(<continuation>)"),
        }
    }
}

/// Represents a user-defined procedure
#[derive(Debug)]
pub struct Procedure {
    pub params: Vec<String>,
    pub varargs: Option<String>, // For (lambda (a b . rest) ...)
    pub body: Vec<Expression>,
    pub env: Rc<RefCell<Environment>>,
}

/// Represents a continuation - the "rest of the computation"
///
/// Continuations need special cases for different expression types because:
/// 1. Each expression type requires different handling of its subexpressions
/// 2. We need to track what to do next after evaluating each part
/// 3. This explicit representation allows capturing and reusing control flow
/// 4. It enables proper tail calls by avoiding stack growth
#[derive(Debug)]
pub enum Continuation {
    /// Terminal continuation - returns the final value
    Done,

    /// Evaluating a sequence of expressions in a body
    EvalBody {
        exps: Vec<Expression>,
        env: Rc<RefCell<Environment>>,
        index: usize,
        next: Box<Continuation>,
    },

    /// Applying a procedure after evaluating it and its arguments
    ApplyProc {
        proc: Value,
        args: Vec<Value>,
        unevaluated: Vec<Expression>,
        env: Rc<RefCell<Environment>>,
        next: Box<Continuation>,
    },

    /// Selecting and evaluating a branch in an if expression
    IfBranch {
        then_exp: Expression,
        else_exp: Option<Expression>,
        env: Rc<RefCell<Environment>>,
        next: Box<Continuation>,
    },

    /// Setting a variable after evaluating the value
    SetVar {
        name: String,
        env: Rc<RefCell<Environment>>,
        next: Box<Continuation>,
    },

    /// Evaluating the test expression in a cond
    CondTest {
        test: Expression,
        consequents: Vec<Expression>,
        remaining_clauses: Vec<(Expression, Vec<Expression>)>,
        else_clause: Option<Vec<Expression>>,
        env: Rc<RefCell<Environment>>,
        next: Box<Continuation>,
    },

    /// Creating a list after evaluating all elements
    MakeList {
        evaluated: Vec<Value>,
        unevaluated: Vec<Expression>,
        env: Rc<RefCell<Environment>>,
        next: Box<Continuation>,
    },

    /// Creating a vector after evaluating all elements
    MakeVector {
        evaluated: Vec<Value>,
        unevaluated: Vec<Expression>,
        env: Rc<RefCell<Environment>>,
        next: Box<Continuation>,
    },
}

/// Represents a lexical environment
#[derive(Debug)]
pub struct Environment {
    pub bindings: HashMap<String, Value>,
    pub parent: Option<Rc<RefCell<Environment>>>,
}

/// Result of an evaluation step
#[derive(Debug)]
pub enum EvalResult {
    Value(Value),
    Continue(Expression, Rc<RefCell<Environment>>, Box<Continuation>),
}

/// The main interpreter structure
#[derive(Debug)]
pub struct Interpreter {
    pub global_env: Rc<RefCell<Environment>>,
    pub current_exp: Option<Expression>,
    pub current_env: Option<Rc<RefCell<Environment>>>,
    pub current_cont: Box<Continuation>,
    pub last_result: Option<Value>,
}

impl Interpreter {
    /// Create a new interpreter with standard library
    pub fn new() -> Self {
        let global_env = Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: None,
        }));

        // Initialize with a Done continuation
        let interpreter = Interpreter {
            global_env: global_env.clone(),
            current_exp: None,
            current_env: None,
            current_cont: Box::new(Continuation::Done),
            last_result: None,
        };

        // We'll implement this later
        // interpreter.initialize_stdlib();

        interpreter
    }

    // Add methods later for evaluation
}

impl Environment {
    /// Create a new empty environment
    pub fn new() -> Self {
        Environment {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new environment with a parent
    pub fn extend(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            bindings: HashMap::new(),
            parent: Some(parent),
        }
    }

    /// Look up a variable in the environment chain
    pub fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.get(name) {
            return Some(value.clone());
        }

        match &self.parent {
            Some(parent) => parent.borrow().lookup(name),
            None => None,
        }
    }

    /// Define a new binding in this environment
    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    /// Set an existing binding, looking up the chain if needed
    pub fn set(&mut self, name: &str, value: Value) -> Result<(), String> {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), value);
            return Ok(());
        }

        match &self.parent {
            Some(parent) => parent.borrow_mut().set(name, value),
            None => Err(format!("Unbound variable: {}", name)),
        }
    }
}

// Implement Display for Value for nice printing
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Value::Integer(n) => write!(f, "{}", n),
            Value::Rational(r) => write!(f, "{}/{}", r.numer(), r.denom()),
            Value::Real(d) => write!(f, "{}", d),
            Value::Complex(c) => {
                if c.im.is_zero() {
                    write!(f, "{}", c.re)
                } else if c.im.is_sign_positive() {
                    write!(f, "{}+{}i", c.re, c.im)
                } else {
                    write!(f, "{}{}i", c.re, c.im)
                }
            }
            Value::Character(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                _ => write!(f, "#\\{}", c),
            },
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Pair(p) => {
                write!(f, "(")?;
                self.display_list(&p.0, &p.1, f)?;
                write!(f, ")")
            }
            Value::Vector(v) => {
                write!(f, "#(")?;
                for (i, val) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, ")")
            }
            Value::Nil => write!(f, "()"),
            Value::Procedure(_) => write!(f, "#<procedure>"),
            Value::PrimitiveProcedure(name, _) => write!(f, "#<primitive:{}>", name),
            Value::Continuation(_) => write!(f, "#<continuation>"),
        }
    }
}

impl Value {
    fn display_list(&self, head: &Value, tail: &Value, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", head)?;

        match tail {
            Value::Nil => Ok(()),
            Value::Pair(p) => {
                write!(f, " ")?;
                self.display_list(&p.0, &p.1, f)
            }
            _ => write!(f, " . {}", tail),
        }
    }
}
