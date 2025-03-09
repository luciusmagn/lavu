use bigdecimal::BigDecimal;
use num::BigInt;
use num::BigRational;
use num::Complex;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Represents a Scheme expression in R5RS
#[derive(Clone, Debug)]
pub enum Expr {
    // Atomic literals
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(Symbol),

    // Compound data structures
    Pair(Rc<Expr>, Rc<Expr>),
    EmptyList,
    Vector(Vec<Rc<Expr>>),

    // Special forms
    Quote(Rc<Expr>),
    Quasiquote(Rc<Expr>),
    Unquote(Rc<Expr>),
    UnquoteSplicing(Rc<Expr>),

    Lambda(LambdaParam, Vec<Rc<Expr>>),
    If(Rc<Expr>, Rc<Expr>, Option<Rc<Expr>>),
    Set(String, Rc<Expr>),
    Define(DefineForm),
    Begin(Vec<Rc<Expr>>),

    // Extended special forms
    Cond(Vec<CondClause>),
    Case(Rc<Expr>, Vec<CaseClause>),
    And(Vec<Rc<Expr>>),
    Or(Vec<Rc<Expr>>),
    Let(LetType, Vec<(String, Rc<Expr>)>, Vec<Rc<Expr>>),

    // Delay and force
    Delay(Rc<Expr>),
    Promise(
        RefCell<Option<Rc<Expr>>>,
        Rc<Expr>,
        Rc<Environment>,
    ),

    // Continuations and dynamic wind
    CallCC(Rc<Expr>),
    Continuation(Continuation),
    DynamicWind(Rc<Expr>, Rc<Expr>, Rc<Expr>),

    // Syntax-rules macros
    SyntaxRules(Vec<Symbol>, Vec<SyntaxRule>),
    MacroUse(String, Vec<Rc<Expr>>),

    // Procedure application
    Application(Rc<Expr>, Vec<Rc<Expr>>),

    // Procedures (first-class)
    Procedure(Procedure),

    // Port
    Port(Port),

    // EOF object
    Eof,
}

/// Symbol with hygiene information
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: String,
    pub uid: Option<usize>, // For hygiene in macros
}

/// Definition forms
#[derive(Clone, Debug)]
pub enum DefineForm {
    Variable(String, Rc<Expr>),
    Procedure(String, LambdaParam, Vec<Rc<Expr>>),
    SyntaxRules(String, Vec<Symbol>, Vec<SyntaxRule>),
}

/// Parameters for lambda expressions
#[derive(Clone, Debug)]
pub enum LambdaParam {
    Fixed(Vec<String>),
    Variadic(Vec<String>, String),
    SingleVariadic(String),
}

/// Cond clauses
#[derive(Clone, Debug)]
pub enum CondClause {
    Regular(Rc<Expr>, Vec<Rc<Expr>>),
    Else(Vec<Rc<Expr>>),
    Arrow(Rc<Expr>, Rc<Expr>),
}

/// Case clauses
#[derive(Clone, Debug)]
pub enum CaseClause {
    Regular(Vec<Rc<Expr>>, Vec<Rc<Expr>>),
    Else(Vec<Rc<Expr>>),
}

/// Let binding variants
#[derive(Clone, Debug)]
pub enum LetType {
    Standard,
    Named(String),
    Star,
    Rec,
}

/// Number types in the R5RS numerical tower
#[derive(Clone, Debug)]
pub enum Number {
    // Exact numbers
    ExactInteger(BigInt),
    ExactRational(BigRational),
    ExactReal(BigRational),

    // Inexact numbers
    InexactInteger(f64),
    InexactRational(f64),
    InexactReal(f64),

    // Complex numbers
    ExactComplex(Complex<BigRational>),
    InexactComplex(Complex<f64>),
}

/// Procedure types
#[derive(Clone, Debug)]
pub enum Procedure {
    Primitive(
        String,
        fn(Vec<Rc<Expr>>) -> Result<Rc<Expr>, String>,
    ),
    Compound(LambdaParam, Vec<Rc<Expr>>, Rc<Environment>),
}

/// Continuation for call/cc
#[derive(Clone, Debug)]
pub struct Continuation {
    stack: Vec<StackFrame>,
}

/// Stack frame for continuations
#[derive(Clone, Debug)]
pub enum StackFrame {
    Sequence(usize, Vec<Rc<Expr>>),
    If(Rc<Expr>, Option<Rc<Expr>>),
    Application(Rc<Expr>, Vec<Rc<Expr>>, usize),
    // Other frames as needed
}

/// Syntax rules for macros
#[derive(Clone, Debug)]
pub struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

/// Pattern for syntax-rules
#[derive(Clone, Debug)]
pub enum Pattern {
    Underscore,
    Identifier(Symbol),
    Literal(Symbol),
    Pair(Box<Pattern>, Box<Pattern>),
    Vector(Vec<Pattern>),
    Ellipsis(Box<Pattern>),
    EmptyList,
}

/// Template for syntax-rules
#[derive(Clone, Debug)]
pub enum Template {
    Identifier(Symbol),
    Literal(Rc<Expr>),
    Pair(Box<Template>, Box<Template>),
    Vector(Vec<Template>),
    Ellipsis(Box<Template>),
    EmptyList,
}

/// Port types for I/O
#[derive(Clone, Debug)]
pub enum Port {
    Input(Rc<std::fs::File>),
    Output(Rc<std::fs::File>),
    InputString(String, usize),
    OutputString(RefCell<String>),
}

/// Environment for storing variable bindings
#[derive(Clone, Debug)]
pub struct Environment {
    bindings: HashMap<String, Rc<Expr>>,
    parent: Option<Rc<Environment>>,
}
