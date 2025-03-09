use bigdecimal::{BigDecimal, ParseBigDecimalError};
use logos::{Logos, Span};
use num::{bigint::ParseBigIntError, BigInt};
use strum::EnumIs;
use thiserror::Error;

use std::str::FromStr;

use crate::chars::{parse_char, ParseCharError};

#[derive(Error, PartialEq, Debug, Clone)]
pub enum LexerError {
    #[error("decimal parse error: {0}")]
    DecimalParseError(#[from] ParseBigDecimalError),

    #[error("integer parse error: {0}")]
    IntegerParseError(#[from] ParseBigIntError),

    #[error("char parse error: {0}")]
    CharParseError(#[from] ParseCharError),

    #[error("other error")]
    DefaultError,
}

impl Default for LexerError {
    fn default() -> Self {
        Self::DefaultError
    }
}

#[derive(Logos, Debug, PartialEq, EnumIs)]
#[logos(error = LexerError)]
pub enum Token {
    // Identifiers
    #[regex(
        r"([a-zA-Z!$%&*/:<=>?^_~+@-][a-zA-Z0-9!$%&*/:<>=?^_~+@-]*)|(\.\.\.)",
        |lex| lex.slice().to_string()
    )]
    Identifier(String),

    // Numbers
    #[regex(
        r"[+-]?[0-9]+",
        priority = 3,
        callback = |lex| BigInt::from_str(lex.slice())
    )]
    Integer(BigInt),

    #[regex(
        r"[+-]?[0-9]+\.[0-9]+",
        |lex| BigDecimal::from_str(lex.slice())
    )]
    Decimal(BigDecimal),

    #[regex(r"#[bB][01]+")]
    Binary,

    #[regex(r"#[oO][0-7]+")]
    Octal,

    #[regex(r"#[xX][0-9a-fA-F]+")]
    Hex,

    // Strings
    #[regex(
        r#""([^"\\]|\\["\\nt])*""#,
        |lex| lex.slice().to_string()
    )]
    String(String),

    // Characters
    #[regex(
        r"#\\[a-zA-Z]+|#\\.",
        |lex| parse_char(lex.slice())
    )]
    Character(char),

    // Boolean
    #[token("#t")]
    True,

    #[token("#f")]
    False,

    // Quotes
    #[token("'")]
    Quote,

    #[token("`")]
    Backquote,

    #[token(",")]
    Unquote,

    #[token(",@")]
    UnquoteSplicing,

    // Delimiters
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("#(")]
    VectorStart,

    // Special syntax
    #[token(".")]
    Dot,

    #[token("#'")]
    SyntaxQuote,

    #[token("#;")]
    DatumComment,

    // Comments
    #[regex(r";[^\n]*")]
    LineComment,
    // TODO: block comment

    // Whitespace (preserved)
    #[regex(
        r"[ \t\n\r]+",
        |lex| lex.slice().to_string()
    )]
    Whitespace(String),
}

pub fn special_forms() -> &'static [&'static str] {
    &[
        "define",
        "set!",
        "lambda",
        "if",
        "cond",
        "case",
        "and",
        "or",
        "let",
        "let*",
        "letrec",
        "begin",
        "do",
        "delay",
        "qoute",
        "quasiqoute",
        "unqoute",
        "unqoute-splicing",
        "else",
    ]
}

pub fn operators() -> &'static [&'static str] {
    &[">", ">=", "<", "<=", "=", "+", "-", "/", "*"]
}

pub fn is_special_form(s: &str) -> bool {
    special_forms().contains(&s)
}

pub fn is_conversion(s: &str) -> bool {
    s.contains("->")
}

pub fn is_mutator(s: &str) -> bool {
    s.ends_with("!")
}

pub fn is_predicate(s: &str) -> bool {
    s.ends_with("?")
}

pub fn is_keywordy(s: &str) -> bool {
    s.ends_with(":") || s.starts_with(":")
}

pub fn is_operator(s: &str) -> bool {
    operators().contains(&s)
}

pub fn tokenize(input: &str) -> Vec<(Token, &str, Span)> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        if let Ok(token) = token {
            let span = lexer.span();
            tokens.push((token, &input[span.clone()], span));
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    use color_eyre::eyre::ensure;
    use color_eyre::Result;

    #[track_caller]
    fn test_single(
        s: &str,
        pred: impl Fn(&Token) -> bool,
    ) -> Result<()> {
        let tokens = tokenize(s);

        ensure!(tokens.len() > 0, "no tokens parsed: {}", s);
        ensure!(pred(&tokens[0].0), "test failed: {}", s);

        Ok(())
    }

    #[test]
    fn test_identifiers() {
        let input = "foo bar+ set! string->symbol";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 7); // 4 identifiers + 3 whitespaces
        assert!(tokens[0].0.is_identifier());
        assert_eq!(tokens[0].1, "foo");
        assert!(tokens[2].0.is_identifier());
        assert_eq!(tokens[2].1, "bar+");
        assert!(tokens[4].0.is_identifier());
        assert_eq!(tokens[4].1, "set!");
        assert!(tokens[6].0.is_identifier());
        assert_eq!(tokens[6].1, "string->symbol");
    }

    #[test]
    fn test_identifiers2() -> Result<()> {
        test_single("!", Token::is_identifier)?;
        test_single("$", Token::is_identifier)?;
        test_single("%", Token::is_identifier)?;
        test_single("&", Token::is_identifier)?;
        test_single("*", Token::is_identifier)?;
        test_single("+", Token::is_identifier)?;
        test_single("-", Token::is_identifier)?;
        test_single("/", Token::is_identifier)?;
        test_single(":", Token::is_identifier)?;
        test_single("<", Token::is_identifier)?;
        test_single("=", Token::is_identifier)?;
        test_single(">", Token::is_identifier)?;
        test_single("?", Token::is_identifier)?;
        test_single("@", Token::is_identifier)?;
        test_single("^", Token::is_identifier)?;
        test_single("_", Token::is_identifier)?;
        test_single("~", Token::is_identifier)?;
        // dot omitted because it is not a valid identifier on its own

        test_single("lambda", Token::is_identifier)?;
        test_single("list->vector", Token::is_identifier)?;
        test_single("<=?", Token::is_identifier)?;
        test_single(
            "the-word-recursion-has-many-meanings",
            Token::is_identifier,
        )?;
        test_single("q", Token::is_identifier)?;
        test_single("soup", Token::is_identifier)?;
        test_single("V17a", Token::is_identifier)?;
        test_single("a34kTMNs", Token::is_identifier)?;

        test_single("...", Token::is_identifier)?;

        Ok(())
    }

    #[test]
    fn test_numbers() {
        let input = "42 -3.14 #b1010 #o777 #xFF";
        let tokens = tokenize(input);

        assert!(tokens[0].0.is_integer());
        assert!(tokens[2].0.is_decimal());
        assert!(tokens[4].0.is_binary());
        assert!(tokens[6].0.is_octal());
        assert!(tokens[8].0.is_hex());
    }

    #[test]
    fn test_strings_and_chars() {
        let input =
            r#""hello world" "escaped \"quotes\"" #\a #\space"#;
        let tokens = tokenize(input);

        assert!(tokens[0].0.is_string());
        assert!(tokens[2].0.is_string());
        assert!(tokens[4].0.is_character());
        assert!(tokens[6].0.is_character());
    }

    // TODO: convert the rest
    #[test]
    fn test_lists_and_vectors() {
        let input = "(define (square x) (* x x)) #(1 2 3)";
        let tokens = tokenize(input);

        assert_eq!(tokens[0].0, Token::LParen);
        assert!(tokens[1].0.is_identifier()); // define
        assert_eq!(tokens[3].0, Token::LParen);
        assert!(tokens[4].0.is_identifier()); // square
        assert!(tokens[6].0.is_identifier()); // x
        assert_eq!(tokens[7].0, Token::RParen);
        assert_eq!(tokens[15].0, Token::RParen);
        assert_eq!(tokens[16].0, Token::RParen);
        assert_eq!(tokens[18].0, Token::VectorStart);
    }

    #[test]
    fn test_quotes_and_special_forms() {
        let input = "'(quote) `(quasiquote) ,(unquote) ,@(unquote-splicing)";
        let tokens = tokenize(input);

        assert_eq!(tokens[0].0, Token::Quote);
        assert_eq!(tokens[5].0, Token::Backquote);
        assert_eq!(tokens[10].0, Token::Unquote);
        assert_eq!(tokens[15].0, Token::UnquoteSplicing);
    }

    #[test]
    fn test_comments() {
        let input = "; line comment\n#| block comment |#";
        let tokens = tokenize(input);

        assert_eq!(tokens[0].0, Token::LineComment);
        //assert_eq!(tokens[2].0, Token::BlockComment);
    }

    #[test]
    fn test_booleans() {
        let input = "#t #f";
        let tokens = tokenize(input);

        assert_eq!(tokens[0].0, Token::True);
        assert_eq!(tokens[2].0, Token::False);
    }

    #[test]
    fn test_complex_expression() {
        let input = "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))";
        let tokens = tokenize(input);
        // This just checks if lexing completes without error
        assert!(tokens.len() > 0);
    }
}
