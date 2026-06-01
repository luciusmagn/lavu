use logos::{Logos, Span as LogosSpan};
use thiserror::Error;

use crate::lexer::{DelimiterState, LexerError, Token, tokenize_checked};
use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum DatumParseError {
    #[error("lexer error: {error}")]
    Lexer { error: LexerError, span: SourceSpan },

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

    #[error("unsupported reader syntax: {syntax}")]
    UnsupportedReaderSyntax {
        syntax: &'static str,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone)]
struct Lexeme {
    token: Token,
    span: SourceSpan,
}

pub fn parse(input: &str) -> Result<Vec<Spanned<Datum>>, DatumParseError> {
    let tokens = tokenize_checked(input).map_err(|error| DatumParseError::Lexer {
        error: error.error,
        span: error.span,
    })?;
    parse_tokens(&tokens)
}

pub fn parse_one(input: &str) -> Result<Option<Spanned<Datum>>, DatumParseError> {
    let mut lexer = Token::lexer(input);
    let mut delimiters = DelimiterState::default();
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        let span = lexer.span();
        let token = match token {
            Ok(token) => token,
            Err(error) => {
                if lexer_error_starts_delimiter(input, &span)
                    && let Some(datum) = complete_prefix_datum(&tokens)?
                {
                    return Ok(Some(datum));
                }
                return Err(DatumParseError::Lexer { error, span });
            }
        };
        delimiters
            .observe(&token, span.clone())
            .map_err(|error| DatumParseError::Lexer {
                error: error.error,
                span: error.span,
            })?;
        tokens.push((token, &input[span.clone()], span));

        let mut parser = Parser::new(&tokens);
        match parser.parse_datum() {
            Ok(datum) if delimiters.ready_to_end_datum() => return Ok(Some(datum)),
            Ok(_) => {}
            Err(error) if is_incomplete_prefix(&error) => {}
            Err(error) => return Err(error),
        }
    }

    let mut parser = Parser::new(&tokens);
    if parser.is_done() {
        Ok(None)
    } else {
        parser.parse_datum().map(Some)
    }
}

pub fn parse_tokens(
    tokens: &[(Token, &str, LogosSpan)],
) -> Result<Vec<Spanned<Datum>>, DatumParseError> {
    Parser::new(tokens).parse_program()
}

fn is_incomplete_prefix(error: &DatumParseError) -> bool {
    matches!(
        error,
        DatumParseError::UnexpectedEnd { .. } | DatumParseError::UnclosedDelimiter { .. }
    )
}

fn lexer_error_starts_delimiter(input: &str, span: &LogosSpan) -> bool {
    input
        .get(span.clone())
        .is_some_and(|text| text.starts_with('"'))
}

fn complete_prefix_datum(
    tokens: &[(Token, &str, LogosSpan)],
) -> Result<Option<Spanned<Datum>>, DatumParseError> {
    let mut parser = Parser::new(tokens);
    match parser.parse_datum() {
        Ok(datum) => Ok(Some(datum)),
        Err(error) if is_incomplete_prefix(&error) => Ok(None),
        Err(error) => Err(error),
    }
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
            Token::BlockComment => {
                unsupported_reader_syntax("#| ... |# block comment", lexeme.span)
            }
            Token::SyntaxQuote => unsupported_reader_syntax("#' syntax quote", lexeme.span),
            Token::DatumComment => unsupported_reader_syntax("#; datum comment", lexeme.span),
            Token::LBracket | Token::RBracket => {
                unsupported_reader_syntax("square bracket delimiter", lexeme.span)
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
        let mut items = Vec::new();

        loop {
            let Some(lexeme) = self.peek() else {
                return Err(DatumParseError::UnclosedDelimiter { span: open_span });
            };

            match lexeme.token {
                Token::RParen => {
                    let close_span = self.bump("list")?.span;
                    return Ok(Spanned::new(
                        Datum::List(items),
                        open_span.start..close_span.end,
                    ));
                }
                Token::Dot => {
                    let dot_span = self.bump("dotted list")?.span;
                    if items.is_empty() {
                        return Err(DatumParseError::UnexpectedToken {
                            context: "dotted list",
                            span: dot_span,
                        });
                    }

                    let tail = self.parse_datum()?;
                    let close = self.bump("dotted list close")?;
                    if !matches!(close.token, Token::RParen) {
                        return Err(DatumParseError::UnexpectedToken {
                            context: "dotted list close",
                            span: close.span,
                        });
                    }

                    return Ok(Spanned::new(
                        Datum::DottedList(items, Box::new(tail)),
                        open_span.start..close.span.end,
                    ));
                }
                _ => items.push(self.parse_datum()?),
            }
        }
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

fn unsupported_reader_syntax<T>(
    syntax: &'static str,
    span: SourceSpan,
) -> Result<T, DatumParseError> {
    Err(DatumParseError::UnsupportedReaderSyntax { syntax, span })
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
        Token::ExactComplex(n) => Some(Atom::ExactComplex(n)),
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
    use super::{DatumParseError, parse, parse_one};
    use crate::lexer::LexerError;
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
    fn skips_line_comments() {
        let datums = parse("1 ; line\n 2").unwrap();

        assert_eq!(datums.len(), 2);
        assert_eq!(datums[0].span, 0..1);
        assert_eq!(datums[1].span, 10..11);
    }

    #[test]
    fn rejects_non_r5rs_reader_extensions() {
        for (input, syntax, span) in [
            ("#| comment |#", "#| ... |# block comment", 0..13),
            ("#;1", "#; datum comment", 0..2),
            ("#'x", "#' syntax quote", 0..2),
            ("[1]", "square bracket delimiter", 0..1),
        ] {
            assert_eq!(
                parse(input).unwrap_err(),
                DatumParseError::UnsupportedReaderSyntax { syntax, span },
                "{input}"
            );
        }
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
    fn parses_dotted_lists_as_data() {
        let datums = parse("'(1 2 . tail)").unwrap();

        let Datum::Quote(inner) = &datums[0].node else {
            panic!("expected quote");
        };
        let Datum::DottedList(items, tail) = &inner.node else {
            panic!("expected dotted list");
        };

        assert_eq!(items.len(), 2);
        assert_eq!(tail.node, Datum::Atom(Atom::Identifier("tail".to_string())));
    }

    #[test]
    fn parses_one_datum_without_requiring_valid_following_data() {
        let datum = parse_one("1 )").unwrap().unwrap();
        assert_eq!(datum.node, Datum::Atom(Atom::Integer(1.into())));
        assert_eq!(datum.span, 0..1);

        let datum = parse_one("  ; skip\n 2 \"unterminated").unwrap().unwrap();
        assert_eq!(datum.node, Datum::Atom(Atom::Integer(2.into())));
        assert_eq!(datum.span, 10..11);

        let datum = parse_one("1\"unterminated").unwrap().unwrap();
        assert_eq!(datum.node, Datum::Atom(Atom::Integer(1.into())));
        assert_eq!(datum.span, 0..1);

        assert!(parse_one(" ; only trivia\n").unwrap().is_none());
    }

    #[test]
    fn rejects_missing_reader_delimiters() {
        assert_eq!(
            parse("1abc").unwrap_err(),
            DatumParseError::Lexer {
                error: LexerError::MissingDelimiter,
                span: 0..4,
            }
        );

        assert_eq!(
            parse_one("1abc").unwrap_err(),
            DatumParseError::Lexer {
                error: LexerError::MissingDelimiter,
                span: 0..4,
            }
        );
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

    #[test]
    fn reports_lexer_errors_with_spans() {
        assert_eq!(
            parse("#| unclosed").unwrap_err(),
            DatumParseError::Lexer {
                error: LexerError::UnclosedBlockComment,
                span: 0..2,
            }
        );

        let DatumParseError::Lexer { error, span } = parse("#\\notachar").unwrap_err() else {
            panic!("expected lexer error");
        };
        assert!(matches!(error, LexerError::CharParseError(_)));
        assert_eq!(span, 0..10);
    }
}
