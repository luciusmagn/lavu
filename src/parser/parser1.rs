use chumsky::prelude::*;
use chumsky::primitive::*;
use color_eyre::Result;
use logos::Span;

use std::ops::Range;

use crate::ast::ast1::{Atom, SExp};
use crate::lexer::Token;

type TokenWithSpan<'a> = (Token, &'a str, Span);

pub fn atom() -> FilterMap<
    impl Fn(Range<usize>, Token) -> Result<SExp, Simple<Token>>,
    Simple<Token>,
> {
    filter_map(|span: Range<usize>, token: Token| match token {
        Token::Identifier(name) => Ok(SExp::Atom(Atom::Identifier(name), span)),
        Token::Integer(n) => Ok(SExp::Atom(Atom::Integer(n), span)),
        Token::Decimal(d) => Ok(SExp::Atom(Atom::Decimal(d), span)),
        Token::Complex(c) => Ok(SExp::Atom(Atom::Complex(c), span)),
        Token::Real((n1, n2)) => Ok(SExp::Atom(Atom::Real((n1, n2)), span)),
        Token::String(s) => Ok(SExp::Atom(Atom::String(s), span)),
        Token::Character(c) => Ok(SExp::Atom(Atom::Character(c), span)),
        Token::True => Ok(SExp::Atom(Atom::Boolean(true), span)),
        Token::False => Ok(SExp::Atom(Atom::Boolean(false), span)),
        Token::Binary(integer)
        | Token::Octal(integer)
        | Token::Hex(integer) => Ok(SExp::Atom(Atom::Integer(integer), span)),
        _ => Err(Simple::custom(
            span,
            format!("Expected atom, got {:?}", token),
        )),
    })
}

pub fn parse(
    tokens: &[TokenWithSpan],
) -> Result<Vec<SExp>, Vec<Simple<Token>>> {
    // Filter out whitespace and comments
    let filtered_tokens: Vec<_> = tokens
        .iter()
        .filter(|(token, _, _)| {
            !matches!(token, Token::Whitespace(_) | Token::LineComment)
        })
        .cloned()
        .collect();

    // Extract just the tokens for the parser
    let tokens_only: Vec<Token> = filtered_tokens
        .iter()
        .map(|(token, _, _)| token.clone())
        .collect();

    // Build the parser
    let expr = recursive(|expr| {
        let list = expr
            .clone()
            .repeated()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with_span(|elements, span| SExp::List(elements, span));

        let vector = just(Token::VectorStart)
            .ignore_then(expr.clone().repeated())
            .then_ignore(just(Token::RParen))
            .map_with_span(|elements, span| SExp::Vector(elements, span));

        let quote = just(Token::Quote)
            .ignore_then(expr.clone())
            .map_with_span(|quoted, span| SExp::Quote(Box::new(quoted), span));

        let quasiquote = just(Token::Backquote)
            .ignore_then(expr.clone())
            .map_with_span(|quoted, span| {
                SExp::Quasiquote(Box::new(quoted), span)
            });

        let unquote = just(Token::Unquote)
            .ignore_then(expr.clone())
            .map_with_span(|quoted, span| {
                SExp::Unquote(Box::new(quoted), span)
            });

        let unquote_splicing = just(Token::UnquoteSplicing)
            .ignore_then(expr.clone())
            .map_with_span(|quoted, span| {
                SExp::UnquoteSplicing(Box::new(quoted), span)
            });

        choice((
            atom(),
            list,
            vector,
            quote,
            quasiquote,
            unquote,
            unquote_splicing,
        ))
    });

    let parser = expr.repeated().then_ignore(end());

    parser.parse(tokens_only).map_err(|e| e)
}

pub fn find_unclosed_sexp(tokens: &[TokenWithSpan]) -> Option<usize> {
    let mut paren_stack: Vec<usize> = Vec::new();

    for (i, (token, _, _)) in tokens.iter().enumerate() {
        match token {
            Token::LParen | Token::VectorStart => {
                paren_stack.push(i);
            }
            Token::RParen => {
                if paren_stack.is_empty() {
                    // Extra closing parenthesis (not handling this case now)
                } else {
                    paren_stack.pop();
                }
            }
            _ => {}
        }
    }

    // If there are unclosed parentheses, return the index of the first one
    // This is usually where the error starts
    paren_stack.first().copied()
}

pub fn create_diagnostic(
    input: &str,
    tokens: &[TokenWithSpan],
    error_index: usize,
) -> Result<()> {
    use ariadne::{Color, Label, Report, ReportKind, Source};

    if error_index >= tokens.len() {
        return Ok(());
    }

    let (_, _, span) = &tokens[error_index];

    let report = Report::build(ReportKind::Error, ("input.ss", span.clone()))
        .with_message("Unclosed delimiter")
        .with_label(
            Label::new(("input.ss", span.clone()))
                .with_message("This delimiter is never closed")
                .with_color(Color::Red),
        )
        .finish();

    report.eprint(("input.ss", Source::from(input)))?;

    Ok(())
}
