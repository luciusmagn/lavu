use chumsky::span::Span;
use logos::Logos;
use logos::Span as LogosSpan;

use std::fmt::{Debug, Display};
use std::ops::Range;

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Identifier(String),
    Integer(num::BigInt),
    Decimal(bigdecimal::BigDecimal),
    String(String),
    Boolean(bool),
    Character(char),
    Special(Token),
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
}

pub fn ariadne_yap(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    sexps: &[SExp],
) {
    use ariadne::{Color, Label, Report, ReportKind, Source};

    let tokens_nws = tokens
        .iter()
        .filter(|(t, _, _)| !t.is_whitespace() && !t.is_line_comment())
        .collect::<Vec<_>>();

    for sexp in sexps {
        match sexp {
            SExp::Atom(_token, span) => {
                Report::build(ReportKind::Advice, ("input.ss", 0..input.len()))
                    .with_message("Parsed an atom")
                    .with_label(
                        Label::new((
                            "input.ss",
                            tokens_nws[span.start].2.start
                                ..tokens_nws[span.end - 1].2.end,
                        ))
                        .with_message("This is an atom")
                        .with_color(Color::Green),
                    )
                    .finish()
                    .eprint(("input.ss", Source::from(input)));
            }
            SExp::List(children, span) => {
                Report::build(ReportKind::Advice, ("input.ss", 0..input.len()))
                    .with_message("Parsed a list")
                    .with_label(
                        Label::new((
                            "input.ss",
                            tokens_nws[span.start].2.start
                                ..tokens_nws[span.end - 1].2.end,
                        ))
                        .with_message("This is a list")
                        .with_color(Color::Green),
                    )
                    .finish()
                    .eprint(("input.ss", Source::from(input)));

                ariadne_yap(
                    input,  //&input[tokens[span.start].2.start..tokens[span.end].2.end],
                    tokens, //&tokens[span.clone()],
                    children.as_slice(),
                );
            }
            SExp::Vector(children, span) => {
                Report::build(ReportKind::Advice, ("input.ss", 0..input.len()))
                    .with_message("Parsed a vector")
                    .with_label(
                        Label::new((
                            "input.ss",
                            tokens_nws[span.start].2.start
                                ..tokens_nws[span.end - 1].2.end,
                        ))
                        .with_message("This is a vector")
                        .with_color(Color::Green),
                    )
                    .finish()
                    .eprint(("input.ss", Source::from(input)));

                ariadne_yap(
                    input,  //&input[tokens[span.start].2.start..tokens[span.end].2.end],
                    tokens, //&tokens[span.clone()],
                    children.as_slice(),
                );
            }
            SExp::Quote(children, span) => {
                Report::build(ReportKind::Advice, ("input.ss", 0..input.len()))
                    .with_message("Parsed a quoted expression")
                    .with_label(
                        Label::new((
                            "input.ss",
                            tokens_nws[span.start].2.start
                                ..tokens_nws[span.end - 1].2.end,
                        ))
                        .with_message("This is a quoted expression")
                        .with_color(Color::Green),
                    )
                    .finish()
                    .eprint(("input.ss", Source::from(input)));

                ariadne_yap(
                    input,  //&input[tokens[span.start].2.start..tokens[span.end].2.end],
                    tokens, //&tokens[span.clone()],
                    &[children.as_ref().clone()],
                );
            }
            SExp::Quasiquote(children, span) => {
                Report::build(ReportKind::Advice, ("input.ss", 0..input.len()))
                    .with_message("Parsed a quasiquote")
                    .with_label(
                        Label::new((
                            "input.ss",
                            tokens_nws[span.start].2.start
                                ..tokens_nws[span.end - 1].2.end,
                        ))
                        .with_message("This is a quasiquote")
                        .with_color(Color::Green),
                    )
                    .finish()
                    .eprint(("input.ss", Source::from(input)));

                ariadne_yap(
                    input,  //&input[tokens[span.start].2.start..tokens[span.end].2.end],
                    tokens, //&tokens[span.clone()],
                    &[children.as_ref().clone()],
                );
            }
            SExp::Unquote(children, span) => {
                Report::build(ReportKind::Advice, ("input.ss", 0..input.len()))
                    .with_message("Parsed an unquote expression")
                    .with_label(
                        Label::new((
                            "input.ss",
                            tokens_nws[span.start].2.start
                                ..tokens_nws[span.end - 1].2.end,
                        ))
                        .with_message("This is an unquote")
                        .with_color(Color::Green),
                    )
                    .finish()
                    .eprint(("input.ss", Source::from(input)));

                ariadne_yap(
                    input,  //&input[tokens[span.start].2.start..tokens[span.end].2.end],
                    tokens, //&tokens[span.clone()],
                    &[children.as_ref().clone()],
                );
            }
            SExp::UnquoteSplicing(children, span) => {
                Report::build(ReportKind::Advice, ("input.ss", 0..input.len()))
                    .with_message("Parsed an unquote splice")
                    .with_label(
                        Label::new((
                            "input.ss",
                            tokens_nws[span.start].2.start
                                ..tokens_nws[span.end - 1].2.end,
                        ))
                        .with_message("This is an unquote splice")
                        .with_color(Color::Green),
                    )
                    .finish()
                    .eprint(("input.ss", Source::from(input)));

                ariadne_yap(
                    input,  //&input[tokens[span.start].2.start..tokens[span.end].2.end],
                    tokens, //&tokens[span.clone()],
                    &[children.as_ref().clone()],
                );
            }
            _ => unimplemented!(),
        }
    }
}
