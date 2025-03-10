use ariadne::{Color, Label, Report, ReportKind, Source};
use bigdecimal::BigDecimal;
use color_eyre::eyre::Result;
use logos::Span as LogosSpan;
use num::{complex, BigInt};

use std::fmt::Debug;
use std::ops::Range;

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Identifier(String),
    Integer(num::BigInt),
    Decimal(bigdecimal::BigDecimal),
    Real((BigInt, BigInt)),
    Complex(complex::Complex<BigDecimal>),
    String(String),
    Boolean(bool),
    Character(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SExp {
    Atom(Atom, Range<usize>),
    List(Vec<SExp>, Range<usize>),
    Vector(Vec<SExp>, Range<usize>),
    Quote(Box<SExp>, Range<usize>),
    Quasiquote(Box<SExp>, Range<usize>),
    Unquote(Box<SExp>, Range<usize>),
    UnquoteSplicing(Box<SExp>, Range<usize>),
}

impl SExp {
    pub fn span(&self) -> &Range<usize> {
        match self {
            SExp::Atom(_, span) => span,
            SExp::List(_, span) => span,
            SExp::Vector(_, span) => span,
            SExp::Quote(_, span) => span,
            SExp::Quasiquote(_, span) => span,
            SExp::Unquote(_, span) => span,
            SExp::UnquoteSplicing(_, span) => span,
        }
    }

    pub fn convert_span(
        &self,
        tokens: &[(Token, &str, LogosSpan)],
    ) -> Range<usize> {
        let og_span = self.span();

        // TODO: This is contigent on the fact that we do keep skipping WS and comment tokens
        let tokens_nws = tokens
            .iter()
            .filter(|(t, _, _)| !t.is_whitespace() && !t.is_line_comment())
            .collect::<Vec<_>>();

        tokens_nws[og_span.start].2.start..tokens_nws[og_span.end - 1].2.end
    }

    pub fn children(&self) -> Vec<SExp> {
        use SExp::*;
        match self {
            Atom(_, _) => vec![],
            List(children, _) => children.clone(),
            Vector(children, _) => children.clone(),
            Quote(child, _) => vec![child.as_ref().clone()],
            Quasiquote(child, _) => vec![child.as_ref().clone()],
            Unquote(child, _) => vec![child.as_ref().clone()],
            UnquoteSplicing(child, _) => vec![child.as_ref().clone()],
        }
    }

    pub fn name(&self) -> &'static str {
        use SExp::*;
        match self {
            Atom(_, _) => "atom",
            List(_, _) => "list",
            Vector(_, _) => "vector",
            Quote(_, _) => "quote expression",
            Quasiquote(_, _) => "quasiquote expression",
            Unquote(_, _) => "unquote expression",
            UnquoteSplicing(_, _) => "unquot esplicing expression",
        }
    }

    pub fn report<'a>(
        &self,
        input: &str,
        filename: &'a str,
        tokens: &[(Token, &str, LogosSpan)],
    ) -> Report<(&'a str, Range<usize>)> {
        Report::build(ReportKind::Advice, (filename, 0..input.len()))
            .with_message(format!("Parsed {}", self.name()))
            .with_label(
                Label::new((filename, self.convert_span(tokens)))
                    .with_message(format!("This is {}", self.name()))
                    .with_color(Color::Green),
            )
            .finish()
    }
}

pub fn ariadne_yap(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    sexps: &[SExp],
) -> Result<()> {
    for sexp in sexps {
        sexp.report(input, "input.ss", tokens)
            .eprint(("input.ss", Source::from(input)))?;

        ariadne_yap(input, tokens, &sexp.children())?;
    }

    Ok(())
}
