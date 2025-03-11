use logos::Span as LogosSpan;
use std::ops::Range;

use crate::ast::ast1::Atom;
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Regular expression variants (from ast3)
    Atom(Atom, Range<usize>),
    List(Vec<Expression>, Range<usize>),
    Vector(Vec<Expression>, Range<usize>),
    Quote(Box<Expression>, Range<usize>),
    Quasiquote(Box<Expression>, Range<usize>),
    Unquote(Box<Expression>, Range<usize>),
    UnquoteSplicing(Box<Expression>, Range<usize>),
    SymbolLiteral(String, Range<usize>),
    // New lambda form
    Lambda(
        Vec<(String, Range<usize>)>, // arguments
        Vec<Expression>,             // body
        Range<usize>,                // span
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
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    // Only variable form, since procedure form will be standardized
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
