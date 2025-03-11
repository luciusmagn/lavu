use logos::Span as LogosSpan;
use std::ops::Range;

use crate::ast::ast1::{Atom, SExp};
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Regular S-expression variants
    Atom(Atom, Range<usize>),
    List(Vec<Expression>, Range<usize>),
    Vector(Vec<Expression>, Range<usize>),
    Quote(Box<Expression>, Range<usize>),
    Quasiquote(Box<Expression>, Range<usize>),
    Unquote(Box<Expression>, Range<usize>),
    UnquoteSplicing(Box<Expression>, Range<usize>),
    // New type for symbol literals
    SymbolLiteral(String, Range<usize>),
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
        }
    }

    pub fn from_sexp(sexp: &SExp) -> Expression {
        match sexp {
            SExp::Atom(atom, span) => {
                Expression::Atom(atom.clone(), span.clone())
            }
            SExp::List(elements, span) => {
                // Check for (quote symbol) form
                if elements.len() == 2 {
                    if let SExp::Atom(Atom::Identifier(sym), _) = &elements[0] {
                        if sym == "quote" {
                            if let SExp::Atom(Atom::Identifier(name), _) =
                                &elements[1]
                            {
                                return Expression::SymbolLiteral(
                                    name.clone(),
                                    span.clone(),
                                );
                            }
                        }
                    }
                }

                Expression::List(
                    elements.iter().map(|e| Expression::from_sexp(e)).collect(),
                    span.clone(),
                )
            }
            SExp::Vector(elements, span) => Expression::Vector(
                elements.iter().map(|e| Expression::from_sexp(e)).collect(),
                span.clone(),
            ),
            SExp::Quote(inner, span) => {
                // Check for 'symbol form
                if let SExp::Atom(Atom::Identifier(name), _) = inner.as_ref() {
                    Expression::SymbolLiteral(name.clone(), span.clone())
                } else {
                    Expression::Quote(
                        Box::new(Expression::from_sexp(inner)),
                        span.clone(),
                    )
                }
            }
            SExp::Quasiquote(inner, span) => Expression::Quasiquote(
                Box::new(Expression::from_sexp(inner)),
                span.clone(),
            ),
            SExp::Unquote(inner, span) => Expression::Unquote(
                Box::new(Expression::from_sexp(inner)),
                span.clone(),
            ),
            SExp::UnquoteSplicing(inner, span) => Expression::UnquoteSplicing(
                Box::new(Expression::from_sexp(inner)),
                span.clone(),
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    // Variable definition: (define var expr)
    Variable((String, Range<usize>), Box<Expression>),
    // Procedure shorthand: (define (name args...) body...)
    Procedure(
        (String, Range<usize>),
        Vec<(String, Range<usize>)>,
        Vec<Expression>,
    ),
}

impl Definition {
    pub fn name(&self) -> String {
        match self {
            Definition::Variable((name, _), _) => name.clone(),
            Definition::Procedure((name, _), _, _) => name.clone(),
        }
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Definition::Variable((_, span), _) => span.clone(),
            Definition::Procedure((_, span), _, _) => span.clone(),
        }
    }

    pub fn from_ast2_definition(
        def: &crate::ast::ast2::Definition,
    ) -> Definition {
        match def {
            crate::ast::ast2::Definition::Variable((name, span), expr) => {
                Definition::Variable(
                    (name.clone(), span.clone()),
                    Box::new(Expression::from_sexp(expr)),
                )
            }
            crate::ast::ast2::Definition::Procedure(
                (name, span),
                args,
                body,
            ) => Definition::Procedure(
                (name.clone(), span.clone()),
                args.clone(),
                body.iter()
                    .map(|expr| Expression::from_sexp(expr))
                    .collect(),
            ),
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

#[derive(Debug, PartialEq, Clone)]
pub enum LambdaBodyForm {
    Definition(Definition),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LambdaBody {
    pub forms: Vec<LambdaBodyForm>,
    pub has_definitions: bool,
}
