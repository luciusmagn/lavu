use logos::Span as LogosSpan;
use std::ops::Range;

use crate::ast::ast1::Atom;
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Regular expression variants (from ast7)
    Atom(Atom, Range<usize>),
    List(Vec<Expression>, Range<usize>),
    Vector(Vec<Expression>, Range<usize>),
    Quote(Box<Expression>, Range<usize>),
    Quasiquote(Box<Expression>, Range<usize>),
    Unquote(Box<Expression>, Range<usize>),
    UnquoteSplicing(Box<Expression>, Range<usize>),
    SymbolLiteral(String, Range<usize>),
    Lambda(
        Vec<(String, Range<usize>)>, // arguments
        Vec<Expression>,             // body
        Range<usize>,                // span
    ),
    SetBang(
        (String, Range<usize>), // variable name
        Box<Expression>,        // value expression
        Range<usize>,           // span
    ),
    Begin(
        Vec<Expression>, // sequence of expressions
        Range<usize>,    // span
    ),
    If(
        Box<Expression>,         // condition
        Box<Expression>,         // then-expression
        Option<Box<Expression>>, // else-expression (optional)
        Range<usize>,            // span
    ),
    // New let form (unnamed)
    Let(
        Vec<((String, Range<usize>), Expression)>, // bindings: (var expr) pairs
        Vec<Expression>,                           // body expressions
        Range<usize>,                              // span
    ),
}

impl Expression {
    pub fn span(&self) -> &Range<usize> {
        match self {
            Expression::Atom(_, span) => span,
            Expression::List(_, span) => span,
            Expression::Vector(_, span) => span,
            Expression::Quote(_, span) => span,
            Expression::Quasiquote(_, span) => span,
            Expression::Unquote(_, span) => span,
            Expression::UnquoteSplicing(_, span) => span,
            Expression::SymbolLiteral(_, span) => span,
            Expression::Lambda(_, _, span) => span,
            Expression::SetBang(_, _, span) => span,
            Expression::Begin(_, span) => span,
            Expression::If(_, _, _, span) => span,
            Expression::Let(_, _, span) => span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Variable((String, Range<usize>), Box<Expression>),
}

impl Definition {
    pub fn name(&self) -> String {
        match self {
            Definition::Variable((name, _), _) => name.clone(),
        }
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Definition::Variable((_, span), _) => span.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelForm {
    Definition(Definition),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub forms: Vec<TopLevelForm>,
    pub source: String,
    pub tokens: Vec<(Token, String, LogosSpan)>,
}
