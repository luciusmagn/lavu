use logos::Span as LogosSpan;
use thiserror::Error;

use crate::lexer::{tokenize, Token};
use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum DatumParseError {
    #[error("unexpected end of input while parsing {context}")]
    UnexpectedEnd {
        context: &'static str,
        span: SourceSpan,
    },

    #[error("unexpected token while parsing {context}")]
    UnexpectedToken {
        context: &'static str,
        span: SourceSpan,
    },

    #[error("unclosed delimiter")]
    UnclosedDelimiter { span: SourceSpan },
}

#[derive(Debug, Clone)]
struct Lexeme {
    token: Token,
    span: SourceSpan,
}

pub fn parse(input: &str) -> Result<Vec<Spanned<Datum>>, DatumParseError> {
    parse_tokens(&tokenize(input))
}

pub fn parse_tokens(
    tokens: &[(Token, &str, LogosSpan)],
) -> Result<Vec<Spanned<Datum>>, DatumParseError> {
    Parser::new(tokens).parse_program()
}

struct Parser {
    tokens: Vec<Lexeme>,
    index: usize,
}

impl Parser {
    fn new(tokens: &[(Token, &str, LogosSpan)]) -> Self {
        let tokens = tokens
            .iter()
            .filter(|(token, _, _)| !matches!(token, Token::Whitespace(_) | Token::LineComment))
            .map(|(token, _, span)| Lexeme {
                token: token.clone(),
                span: span.clone(),
            })
            .collect();

        Self { tokens, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Spanned<Datum>>, DatumParseError> {
        let mut datums = Vec::new();

        while !self.is_done() {
            datums.push(self.parse_datum()?);
        }

        Ok(datums)
    }

    fn parse_datum(&mut self) -> Result<Spanned<Datum>, DatumParseError> {
        let lexeme = self.bump("datum")?;

        match lexeme.token {
            Token::LParen => self.parse_list(lexeme.span),
            Token::VectorStart => self.parse_vector(lexeme.span),
            Token::Quote => self.parse_prefixed(lexeme.span, Datum::Quote, "quote"),
            Token::Backquote => self.parse_prefixed(lexeme.span, Datum::Quasiquote, "quasiquote"),
            Token::Unquote => self.parse_prefixed(lexeme.span, Datum::Unquote, "unquote"),
            Token::UnquoteSplicing => {
                self.parse_prefixed(lexeme.span, Datum::UnquoteSplicing, "unquote-splicing")
            }
            token => atom_from_token(token)
                .map(|atom| Spanned::new(Datum::Atom(atom), lexeme.span.clone()))
                .ok_or(DatumParseError::UnexpectedToken {
                    context: "datum",
                    span: lexeme.span,
                }),
        }
    }

    fn parse_list(&mut self, open_span: SourceSpan) -> Result<Spanned<Datum>, DatumParseError> {
        let (items, close_span) = self.parse_until_close(open_span.clone(), "list")?;
        Ok(Spanned::new(
            Datum::List(items),
            open_span.start..close_span.end,
        ))
    }

    fn parse_vector(&mut self, open_span: SourceSpan) -> Result<Spanned<Datum>, DatumParseError> {
        let (items, close_span) = self.parse_until_close(open_span.clone(), "vector")?;
        Ok(Spanned::new(
            Datum::Vector(items),
            open_span.start..close_span.end,
        ))
    }

    fn parse_until_close(
        &mut self,
        open_span: SourceSpan,
        context: &'static str,
    ) -> Result<(Vec<Spanned<Datum>>, SourceSpan), DatumParseError> {
        let mut items = Vec::new();

        loop {
            let Some(lexeme) = self.peek() else {
                return Err(DatumParseError::UnclosedDelimiter { span: open_span });
            };

            if matches!(lexeme.token, Token::RParen) {
                let close_span = self.bump(context)?.span;
                return Ok((items, close_span));
            }

            items.push(self.parse_datum()?);
        }
    }

    fn parse_prefixed(
        &mut self,
        prefix_span: SourceSpan,
        constructor: impl FnOnce(Box<Spanned<Datum>>) -> Datum,
        context: &'static str,
    ) -> Result<Spanned<Datum>, DatumParseError> {
        let datum = self.parse_datum().map_err(|err| match err {
            DatumParseError::UnexpectedEnd { span, .. } => {
                DatumParseError::UnexpectedEnd { context, span }
            }
            other => other,
        })?;
        let span = prefix_span.start..datum.span.end;
        Ok(Spanned::new(constructor(Box::new(datum)), span))
    }

    fn bump(&mut self, context: &'static str) -> Result<Lexeme, DatumParseError> {
        if let Some(lexeme) = self.tokens.get(self.index) {
            self.index += 1;
            Ok(lexeme.clone())
        } else {
            Err(DatumParseError::UnexpectedEnd {
                context,
                span: 0..0,
            })
        }
    }

    fn peek(&self) -> Option<&Lexeme> {
        self.tokens.get(self.index)
    }

    fn is_done(&self) -> bool {
        self.index >= self.tokens.len()
    }
}

fn atom_from_token(token: Token) -> Option<Atom> {
    match token {
        Token::Identifier(name) => Some(Atom::Identifier(name)),
        Token::Integer(n)
        | Token::Binary(n)
        | Token::Octal(n)
        | Token::Hex(n)
        | Token::DecInteger(n) => Some(Atom::Integer(n)),
        Token::Decimal(n) => Some(Atom::Decimal(n)),
        Token::Real((numerator, denominator)) => Some(Atom::Real(numerator, denominator)),
        Token::Complex(n) => Some(Atom::Complex(n)),
        Token::String(s) => Some(Atom::String(unescape_string_token(&s))),
        Token::Character(c) => Some(Atom::Character(c)),
        Token::True => Some(Atom::Boolean(true)),
        Token::False => Some(Atom::Boolean(false)),
        _ => None,
    }
}

fn unescape_string_token(token: &str) -> String {
    let body = token
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(token);

    let mut output = String::new();
    let mut chars = body.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('"') => output.push('"'),
                Some('\\') => output.push('\\'),
                Some('n') => output.push('\n'),
                Some('t') => output.push('\t'),
                Some(other) => {
                    output.push('\\');
                    output.push(other);
                }
                None => output.push('\\'),
            }
        } else {
            output.push(ch);
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use super::{parse, DatumParseError};
    use crate::syntax::{Atom, Datum};

    #[test]
    fn parses_atom_datums_with_byte_spans() {
        let datums = parse("#t 42 \"hi\" #\\space").unwrap();

        assert_eq!(datums.len(), 4);
        assert_eq!(datums[0].node, Datum::Atom(Atom::Boolean(true)));
        assert_eq!(datums[0].span, 0..2);
        assert!(matches!(datums[1].node, Datum::Atom(Atom::Integer(_))));
        assert_eq!(datums[1].span, 3..5);
        assert_eq!(datums[2].node, Datum::Atom(Atom::String("hi".to_string())));
        assert_eq!(datums[2].span, 6..10);
        assert_eq!(datums[3].node, Datum::Atom(Atom::Character(' ')));
        assert_eq!(datums[3].span, 11..18);
    }

    #[test]
    fn parses_nested_lists_vectors_and_quotes() {
        let datums = parse("'(and #(1 2) x)").unwrap();

        assert_eq!(datums.len(), 1);
        assert_eq!(datums[0].span, 0..15);
        let Datum::Quote(inner) = &datums[0].node else {
            panic!("expected quote");
        };
        let Datum::List(items) = &inner.node else {
            panic!("expected quoted list");
        };

        assert_eq!(items.len(), 3);
        assert_eq!(
            items[0].node,
            Datum::Atom(Atom::Identifier("and".to_string()))
        );
        assert!(matches!(items[1].node, Datum::Vector(_)));
    }

    #[test]
    fn reports_unclosed_lists_at_the_opening_span() {
        let error = parse("(1 2").unwrap_err();

        assert_eq!(error, DatumParseError::UnclosedDelimiter { span: 0..1 });
    }

    #[test]
    fn reports_unexpected_closing_delimiters() {
        let error = parse(")").unwrap_err();

        assert_eq!(
            error,
            DatumParseError::UnexpectedToken {
                context: "datum",
                span: 0..1
            }
        );
    }
}
