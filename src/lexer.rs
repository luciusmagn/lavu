use bigdecimal::{BigDecimal, ParseBigDecimalError};
use logos::{Lexer as LogosLexer, Logos, Span};
use num::{
    BigInt, BigRational, Complex, FromPrimitive, Num, ToPrimitive, Zero, bigint::ParseBigIntError,
};
use strum::EnumIs;
use thiserror::Error;

use std::str::FromStr;

use crate::chars::{ParseCharError, parse_char};

#[derive(Debug, PartialEq, Clone)]
pub struct SpannedLexerError {
    pub error: LexerError,
    pub span: Span,
}

#[derive(Error, PartialEq, Debug, Clone, Default)]
pub enum LexerError {
    #[error("decimal parse error: {0}")]
    DecimalParseError(#[from] ParseBigDecimalError),

    #[error("integer parse error: {0}")]
    IntegerParseError(#[from] ParseBigIntError),

    #[error("char parse error: {0}")]
    CharParseError(#[from] ParseCharError),

    #[error("zero denominator in rational literal")]
    ZeroDenominator,

    #[error("unclosed block comment")]
    UnclosedBlockComment,

    #[error("missing delimiter after token")]
    MissingDelimiter,

    #[error("other error")]
    #[default]
    DefaultError,
}

#[derive(Logos, Debug, PartialEq, EnumIs, Hash, Eq, Clone)]
#[logos(error = LexerError)]
pub enum Token {
    // Identifiers
    #[regex(
        r"([a-zA-Z!$%&*/:<=>?^_~][a-zA-Z0-9!$%&*/:<>=?^_~+@.\-]*)|(\+)|(-)|(\.\.\.)",
        |lex| lex.slice().to_ascii_lowercase()
    )]
    Identifier(String),

    // Numbers
    #[regex(
        r"[+-]?[0-9]+",
        priority = 3,
        callback = |lex| BigInt::from_str(lex.slice())
    )]
    #[regex(
        r"#[eE][+-]?[0-9]+",
        priority = 4,
        callback = |lex| BigInt::from_str(&lex.slice()[2..])
    )]
    Integer(BigInt),

    #[regex(
        r"[+-]?[0-9]+/[0-9]+",
        priority = 3,
        callback = |lex| parse_ratio_literal(lex.slice())
    )]
    #[regex(
        r"#[eE][+-]?[0-9]+/[0-9]+",
        priority = 6,
        callback = |lex| parse_ratio_literal(&lex.slice()[2..])
    )]
    #[regex(
        r"#[dD][+-]?[0-9]+/[0-9]+",
        priority = 6,
        callback = |lex| parse_ratio_literal(&lex.slice()[2..])
    )]
    #[regex(
        r"#[eE]#[dD][+-]?[0-9]+/[0-9]+",
        priority = 8,
        callback = |lex| parse_ratio_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[eE][+-]?[0-9]+/[0-9]+",
        priority = 8,
        callback = |lex| parse_ratio_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[bB][+-]?[01]+/[01]+",
        priority = 6,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[2..], 2)
    )]
    #[regex(
        r"#[eE]#[bB][+-]?[01]+/[01]+",
        priority = 8,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[4..], 2)
    )]
    #[regex(
        r"#[bB]#[eE][+-]?[01]+/[01]+",
        priority = 8,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[4..], 2)
    )]
    #[regex(
        r"#[oO][+-]?[0-7]+/[0-7]+",
        priority = 6,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[2..], 8)
    )]
    #[regex(
        r"#[eE]#[oO][+-]?[0-7]+/[0-7]+",
        priority = 8,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[4..], 8)
    )]
    #[regex(
        r"#[oO]#[eE][+-]?[0-7]+/[0-7]+",
        priority = 8,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[4..], 8)
    )]
    #[regex(
        r"#[xX][+-]?[0-9a-fA-F]+/[0-9a-fA-F]+",
        priority = 6,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[2..], 16)
    )]
    #[regex(
        r"#[eE]#[xX][+-]?[0-9a-fA-F]+/[0-9a-fA-F]+",
        priority = 8,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[4..], 16)
    )]
    #[regex(
        r"#[xX]#[eE][+-]?[0-9a-fA-F]+/[0-9a-fA-F]+",
        priority = 8,
        callback = |lex| parse_radix_ratio_literal(&lex.slice()[4..], 16)
    )]
    #[regex(
        r"#[eE][+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 6,
        callback = |lex| parse_exact_decimal_literal(&lex.slice()[2..])
    )]
    #[regex(
        r"#[eE][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 8,
        callback = |lex| parse_exact_decimal_literal(&lex.slice()[2..])
    )]
    #[regex(
        r"#[eE]#[dD][+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 8,
        callback = |lex| parse_exact_decimal_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[eE]#[dD][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 10,
        callback = |lex| parse_exact_decimal_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[eE][+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 8,
        callback = |lex| parse_exact_decimal_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[eE][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 10,
        callback = |lex| parse_exact_decimal_literal(&lex.slice()[4..])
    )]
    Real((BigInt, BigInt)),

    #[regex(
        r"[+-]?([0-9]+(/[0-9]+)?)[+-]([0-9]+(/[0-9]+)?)i",
        priority = 9,
        callback = |lex| parse_exact_rectangular_complex(lex.slice())
    )]
    #[regex(
        r"#[eE][+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)[+-]([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)i",
        priority = 12,
        callback = |lex| parse_exact_rectangular_complex(&lex.slice()[2..])
    )]
    #[regex(
        r"#[dD][+-]?([0-9]+(/[0-9]+)?)[+-]([0-9]+(/[0-9]+)?)i",
        priority = 11,
        callback = |lex| parse_exact_rectangular_complex(&lex.slice()[2..])
    )]
    #[regex(
        r"(#[eE]#[dD]|#[dD]#[eE])[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)[+-]([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)i",
        priority = 14,
        callback = |lex| parse_exact_rectangular_complex(&lex.slice()[4..])
    )]
    #[regex(
        r"[+-]i",
        priority = 7,
        callback = |lex| parse_exact_imaginary_unit(lex.slice())
    )]
    #[regex(
        r"#[eE][+-]i",
        priority = 9,
        callback = |lex| parse_exact_imaginary_unit(&lex.slice()[2..])
    )]
    #[regex(
        r"(#[dD]|#[eE]#[dD]|#[dD]#[eE])[+-]i",
        priority = 11,
        callback = |lex| parse_exact_imaginary_unit(strip_number_prefixes(lex.slice()))
    )]
    #[regex(
        r"[+-]?([0-9]+(/[0-9]+)?)i",
        priority = 7,
        callback = |lex| parse_exact_pure_imaginary(lex.slice())
    )]
    #[regex(
        r"#[eE][+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)i",
        priority = 10,
        callback = |lex| parse_exact_pure_imaginary(&lex.slice()[2..])
    )]
    #[regex(
        r"#[dD][+-]?([0-9]+(/[0-9]+)?)i",
        priority = 9,
        callback = |lex| parse_exact_pure_imaginary(&lex.slice()[2..])
    )]
    #[regex(
        r"(#[eE]#[dD]|#[dD]#[eE])[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)i",
        priority = 12,
        callback = |lex| parse_exact_pure_imaginary(&lex.slice()[4..])
    )]
    #[regex(
        r"[+-]?([0-9]+(/[0-9]+)?)[+-]i",
        priority = 7,
        callback = |lex| parse_exact_unit_imaginary_complex(lex.slice())
    )]
    #[regex(
        r"#[eE][+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)[+-]i",
        priority = 10,
        callback = |lex| parse_exact_unit_imaginary_complex(&lex.slice()[2..])
    )]
    #[regex(
        r"#[dD][+-]?([0-9]+(/[0-9]+)?)[+-]i",
        priority = 9,
        callback = |lex| parse_exact_unit_imaginary_complex(&lex.slice()[2..])
    )]
    #[regex(
        r"(#[eE]#[dD]|#[dD]#[eE])[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)[+-]i",
        priority = 12,
        callback = |lex| parse_exact_unit_imaginary_complex(&lex.slice()[4..])
    )]
    #[regex(
        r"(#[bBoOxX]|#[eE]#[bBoOxX]|#[bBoOxX]#[eE])[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?[+-][0-9a-fA-F]+(/[0-9a-fA-F]+)?i",
        priority = 12,
        callback = |lex| parse_radix_exact_rectangular_complex(lex.slice())
    )]
    #[regex(
        r"(#[bBoOxX]|#[eE]#[bBoOxX]|#[bBoOxX]#[eE])[+-]i",
        priority = 10,
        callback = |lex| parse_radix_exact_imaginary_unit(lex.slice())
    )]
    #[regex(
        r"(#[bBoOxX]|#[eE]#[bBoOxX]|#[bBoOxX]#[eE])[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?i",
        priority = 10,
        callback = |lex| parse_radix_exact_pure_imaginary(lex.slice())
    )]
    #[regex(
        r"(#[bBoOxX]|#[eE]#[bBoOxX]|#[bBoOxX]#[eE])[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?[+-]i",
        priority = 10,
        callback = |lex| parse_radix_exact_unit_imaginary_complex(lex.slice())
    )]
    ExactComplex(Complex<BigRational>),

    #[regex(
        r"[+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[+-](([0-9]+(\.[0-9]*)?)|(\.[0-9]+))i",
        priority = 7,
        callback = |lex| parse_rectangular_complex(lex.slice())
    )]
    #[regex(
        r"[+-]i",
        priority = 6,
        callback = |lex| parse_imaginary_unit(lex.slice())
    )]
    #[regex(
        r"[+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))i",
        priority = 6,
        callback = |lex| parse_pure_imaginary(lex.slice())
    )]
    #[regex(
        r"[+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[+-]i",
        priority = 6,
        callback = |lex| parse_unit_imaginary_complex(lex.slice())
    )]
    #[regex(
        r"#[iI][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[+-](([0-9]+(\.[0-9]*)?)|(\.[0-9]+))i",
        priority = 9,
        callback = |lex| parse_rectangular_complex(&lex.slice()[2..])
    )]
    #[regex(
        r"#[iI][+-]i",
        priority = 8,
        callback = |lex| parse_imaginary_unit(&lex.slice()[2..])
    )]
    #[regex(
        r"#[iI][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))i",
        priority = 8,
        callback = |lex| parse_pure_imaginary(&lex.slice()[2..])
    )]
    #[regex(
        r"#[iI][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[+-]i",
        priority = 8,
        callback = |lex| parse_unit_imaginary_complex(&lex.slice()[2..])
    )]
    #[regex(
        r"(#[dD]|#[iI]#[dD]|#[dD]#[iI])[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)[+-]([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)i",
        priority = 8,
        callback = |lex| parse_inexact_decimal_radix_rectangular_complex(lex.slice())
    )]
    #[regex(
        r"(#[iI]#[dD]|#[dD]#[iI])[+-]i",
        priority = 10,
        callback = |lex| parse_inexact_decimal_radix_imaginary_unit(lex.slice())
    )]
    #[regex(
        r"(#[dD]|#[iI]#[dD]|#[dD]#[iI])[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)i",
        priority = 8,
        callback = |lex| parse_inexact_decimal_radix_pure_imaginary(lex.slice())
    )]
    #[regex(
        r"(#[dD]|#[iI]#[dD]|#[dD]#[iI])[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)[+-]i",
        priority = 8,
        callback = |lex| parse_inexact_decimal_radix_unit_imaginary_complex(lex.slice())
    )]
    #[regex(
        r"(#[iI]#[bBoOxX]|#[bBoOxX]#[iI])[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?[+-][0-9a-fA-F]+(/[0-9a-fA-F]+)?i",
        priority = 12,
        callback = |lex| parse_inexact_radix_rectangular_complex(lex.slice())
    )]
    #[regex(
        r"(#[iI]#[bBoOxX]|#[bBoOxX]#[iI])[+-]i",
        priority = 10,
        callback = |lex| parse_inexact_radix_imaginary_unit(lex.slice())
    )]
    #[regex(
        r"(#[iI]#[bBoOxX]|#[bBoOxX]#[iI])[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?i",
        priority = 10,
        callback = |lex| parse_inexact_radix_pure_imaginary(lex.slice())
    )]
    #[regex(
        r"(#[iI]#[bBoOxX]|#[bBoOxX]#[iI])[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?[+-]i",
        priority = 10,
        callback = |lex| parse_inexact_radix_unit_imaginary_complex(lex.slice())
    )]
    #[regex(
        r"[+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))@[+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))",
        priority = 7,
        callback = |lex| parse_polar_complex(lex.slice())
    )]
    #[regex(
        r"#[iI][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))@[+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))",
        priority = 9,
        callback = |lex| parse_polar_complex(&lex.slice()[2..])
    )]
    #[regex(
        r"(#[dD]|#[iI]#[dD]|#[dD]#[iI])[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)@[+-]?([0-9]+(/[0-9]+)?|(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))([eEsSfFdDlL][+-]?[0-9]+)?)",
        priority = 9,
        callback = |lex| parse_inexact_decimal_radix_polar_complex(lex.slice())
    )]
    Complex(Complex<BigDecimal>),

    #[regex(
        r"[+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 4,
        callback = |lex| BigDecimal::from_str(lex.slice())
    )]
    #[regex(
        r"[+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 5,
        callback = |lex| parse_decimal_literal(lex.slice())
    )]
    #[regex(
        r"#[iI][+-]?[0-9]+",
        priority = 4,
        callback = |lex| BigDecimal::from_str(&lex.slice()[2..])
    )]
    #[regex(
        r"#[iI][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 8,
        callback = |lex| parse_decimal_literal(&lex.slice()[2..])
    )]
    #[regex(
        r"#[iI]#[dD][+-]?[0-9]+",
        priority = 6,
        callback = |lex| BigDecimal::from_str(&lex.slice()[4..])
    )]
    #[regex(
        r"#[iI]#[dD][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 10,
        callback = |lex| parse_decimal_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[iI][+-]?[0-9]+",
        priority = 6,
        callback = |lex| BigDecimal::from_str(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[iI][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 10,
        callback = |lex| parse_decimal_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[iI]#[bB][+-]?[01]+",
        priority = 6,
        callback = |lex| parse_inexact_radix_literal(&lex.slice()[4..], 2)
    )]
    #[regex(
        r"#[bB]#[iI][+-]?[01]+",
        priority = 6,
        callback = |lex| parse_inexact_radix_literal(&lex.slice()[4..], 2)
    )]
    #[regex(
        r"#[iI]#[oO][+-]?[0-7]+",
        priority = 6,
        callback = |lex| parse_inexact_radix_literal(&lex.slice()[4..], 8)
    )]
    #[regex(
        r"#[oO]#[iI][+-]?[0-7]+",
        priority = 6,
        callback = |lex| parse_inexact_radix_literal(&lex.slice()[4..], 8)
    )]
    #[regex(
        r"#[iI]#[xX][+-]?[0-9a-fA-F]+",
        priority = 6,
        callback = |lex| parse_inexact_radix_literal(&lex.slice()[4..], 16)
    )]
    #[regex(
        r"#[xX]#[iI][+-]?[0-9a-fA-F]+",
        priority = 6,
        callback = |lex| parse_inexact_radix_literal(&lex.slice()[4..], 16)
    )]
    #[regex(
        r"#[iI][+-]?[0-9]+/[0-9]+",
        priority = 6,
        callback = |lex| parse_inexact_fraction_literal(&lex.slice()[2..])
    )]
    #[regex(
        r"#[iI]#[dD][+-]?[0-9]+/[0-9]+",
        priority = 8,
        callback = |lex| parse_inexact_fraction_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[iI][+-]?[0-9]+/[0-9]+",
        priority = 8,
        callback = |lex| parse_inexact_fraction_literal(&lex.slice()[4..])
    )]
    #[regex(
        r"#[iI]#[bB][+-]?[01]+/[01]+",
        priority = 8,
        callback = |lex| parse_inexact_radix_fraction_literal(&lex.slice()[4..], 2)
    )]
    #[regex(
        r"#[bB]#[iI][+-]?[01]+/[01]+",
        priority = 8,
        callback = |lex| parse_inexact_radix_fraction_literal(&lex.slice()[4..], 2)
    )]
    #[regex(
        r"#[iI]#[oO][+-]?[0-7]+/[0-7]+",
        priority = 8,
        callback = |lex| parse_inexact_radix_fraction_literal(&lex.slice()[4..], 8)
    )]
    #[regex(
        r"#[oO]#[iI][+-]?[0-7]+/[0-7]+",
        priority = 8,
        callback = |lex| parse_inexact_radix_fraction_literal(&lex.slice()[4..], 8)
    )]
    #[regex(
        r"#[iI]#[xX][+-]?[0-9a-fA-F]+/[0-9a-fA-F]+",
        priority = 8,
        callback = |lex| parse_inexact_radix_fraction_literal(&lex.slice()[4..], 16)
    )]
    #[regex(
        r"#[xX]#[iI][+-]?[0-9a-fA-F]+/[0-9a-fA-F]+",
        priority = 8,
        callback = |lex| parse_inexact_radix_fraction_literal(&lex.slice()[4..], 16)
    )]
    #[regex(
        r"#[iI][+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 6,
        callback = |lex| BigDecimal::from_str(&lex.slice()[2..])
    )]
    #[regex(
        r"#[dD][+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 6,
        callback = |lex| BigDecimal::from_str(&lex.slice()[2..])
    )]
    #[regex(
        r"#[dD][+-]?(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))[eEsSfFdDlL][+-]?[0-9]+",
        priority = 8,
        callback = |lex| parse_decimal_literal(&lex.slice()[2..])
    )]
    #[regex(
        r"#[iI]#[dD][+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 8,
        callback = |lex| BigDecimal::from_str(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[iI][+-]?([0-9]+\.[0-9]*|\.[0-9]+)",
        priority = 8,
        callback = |lex| BigDecimal::from_str(&lex.slice()[4..])
    )]
    Decimal(BigDecimal),

    #[regex(
        r"#[bB][+-]?[01]+",
        |lex| BigInt::from_str_radix(&lex.slice()[2..], 2)
    )]
    #[regex(
        r"#[eE]#[bB][+-]?[01]+",
        priority = 6,
        callback = |lex| BigInt::from_str_radix(&lex.slice()[4..], 2)
    )]
    #[regex(
        r"#[bB]#[eE][+-]?[01]+",
        priority = 6,
        callback = |lex| BigInt::from_str_radix(&lex.slice()[4..], 2)
    )]
    Binary(BigInt),

    #[regex(
        r"#[oO][+-]?[0-7]+",
        |lex| BigInt::from_str_radix(&lex.slice()[2..], 8)
    )]
    #[regex(
        r"#[eE]#[oO][+-]?[0-7]+",
        priority = 6,
        callback = |lex| BigInt::from_str_radix(&lex.slice()[4..], 8)
    )]
    #[regex(
        r"#[oO]#[eE][+-]?[0-7]+",
        priority = 6,
        callback = |lex| BigInt::from_str_radix(&lex.slice()[4..], 8)
    )]
    Octal(BigInt),

    #[regex(
        r"#[xX][+-]?[0-9a-fA-F]+",
        |lex| BigInt::from_str_radix(&lex.slice()[2..], 16)
    )]
    #[regex(
        r"#[eE]#[xX][+-]?[0-9a-fA-F]+",
        priority = 6,
        callback = |lex| BigInt::from_str_radix(&lex.slice()[4..], 16)
    )]
    #[regex(
        r"#[xX]#[eE][+-]?[0-9a-fA-F]+",
        priority = 6,
        callback = |lex| BigInt::from_str_radix(&lex.slice()[4..], 16)
    )]
    Hex(BigInt),

    #[regex(
        r"#[dD][+-]?[0-9]+",
        |lex| BigInt::from_str(&lex.slice()[2..])
    )]
    #[regex(
        r"#[eE]#[dD][+-]?[0-9]+",
        priority = 6,
        callback = |lex| BigInt::from_str(&lex.slice()[4..])
    )]
    #[regex(
        r"#[dD]#[eE][+-]?[0-9]+",
        priority = 6,
        callback = |lex| BigInt::from_str(&lex.slice()[4..])
    )]
    DecInteger(BigInt),

    // Strings
    #[regex(
        r#""([^"\\]|\\["\\])*""#,
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
    #[regex(r"#[tT]")]
    True,

    #[regex(r"#[fF]")]
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
    #[token("#|", lex_block_comment)]
    BlockComment,

    #[regex(r";[^\n]*")]
    LineComment,

    // Whitespace (preserved)
    #[regex(
        r"[ \t\n\r]+",
        |lex| lex.slice().to_string()
    )]
    Whitespace(String),
}

fn lex_block_comment(lex: &mut LogosLexer<'_, Token>) -> Result<(), LexerError> {
    let mut depth = 1usize;
    let mut consumed = 0usize;
    let remainder = lex.remainder();

    while consumed < remainder.len() {
        let text = &remainder[consumed..];
        if text.starts_with("#|") {
            depth += 1;
            consumed += 2;
        } else if text.starts_with("|#") {
            depth -= 1;
            consumed += 2;
            if depth == 0 {
                lex.bump(consumed);
                return Ok(());
            }
        } else {
            let ch = text
                .chars()
                .next()
                .expect("loop condition ensures remaining input");
            consumed += ch.len_utf8();
        }
    }

    Err(LexerError::UnclosedBlockComment)
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
        "let-syntax",
        "letrec-syntax",
        "begin",
        "do",
        "delay",
        "define-syntax",
        "syntax-rules",
        "quote",
        "quasiquote",
        "unquote",
        "unquote-splicing",
        "else",
        "=>",
        "...",
    ]
}

pub fn operators() -> &'static [&'static str] {
    &[">", ">=", "<", "<=", "=", "+", "-", "/", "*"]
}

pub fn is_special_form(s: &str) -> bool {
    let lower = s.to_ascii_lowercase();
    special_forms().contains(&lower.as_str())
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

fn parse_ratio_literal(slice: &str) -> Result<(BigInt, BigInt), LexerError> {
    let (numerator, denominator) = slice
        .split_once('/')
        .expect("ratio token regex guarantees a slash");

    let numerator = BigInt::from_str(numerator)?;
    let denominator = BigInt::from_str(denominator)?;
    if denominator.is_zero() {
        return Err(LexerError::ZeroDenominator);
    }

    Ok((numerator, denominator))
}

fn parse_radix_ratio_literal(slice: &str, radix: u32) -> Result<(BigInt, BigInt), LexerError> {
    let (numerator, denominator) = slice
        .split_once('/')
        .expect("ratio token regex guarantees a slash");

    let numerator = BigInt::from_str_radix(numerator, radix)?;
    let denominator = BigInt::from_str_radix(denominator, radix)?;
    if denominator.is_zero() {
        return Err(LexerError::ZeroDenominator);
    }

    Ok((numerator, denominator))
}

fn parse_exact_decimal_literal(slice: &str) -> Result<(BigInt, BigInt), LexerError> {
    let (sign, magnitude) = match slice.as_bytes().first() {
        Some(b'-') => (-1, &slice[1..]),
        Some(b'+') => (1, &slice[1..]),
        _ => (1, slice),
    };
    let (mantissa, exponent) = split_decimal_exponent(magnitude)?;
    let (whole, fractional) = mantissa.split_once('.').unwrap_or((mantissa, ""));
    let digits = format!("{whole}{fractional}");
    let mut numerator = if digits.is_empty() {
        BigInt::from(0)
    } else {
        BigInt::from_str(&digits)?
    };
    if sign < 0 {
        numerator = -numerator;
    }
    let scale = i64::try_from(fractional.len()).map_err(|_| LexerError::DefaultError)?;
    let adjusted_scale = scale - exponent;
    let denominator = if adjusted_scale <= 0 {
        numerator *= decimal_scale(-adjusted_scale)?;
        BigInt::from(1)
    } else {
        decimal_scale(adjusted_scale)?
    };

    Ok((numerator, denominator))
}

fn parse_decimal_literal(slice: &str) -> Result<BigDecimal, ParseBigDecimalError> {
    BigDecimal::from_str(&normalize_decimal_exponent(slice))
}

fn parse_exact_rectangular_complex(slice: &str) -> Result<Complex<BigRational>, LexerError> {
    let sign_index = slice
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_exact_component(&slice[..sign_index])?;
    let imaginary = parse_exact_component(
        slice[sign_index..]
            .strip_suffix('i')
            .ok_or(LexerError::DefaultError)?,
    )?;

    Ok(Complex::new(real, imaginary))
}

fn parse_exact_imaginary_unit(slice: &str) -> Complex<BigRational> {
    let imaginary = if slice.starts_with('-') { -1 } else { 1 };
    Complex::new(
        BigRational::zero(),
        BigRational::from_integer(BigInt::from(imaginary)),
    )
}

fn parse_exact_pure_imaginary(slice: &str) -> Result<Complex<BigRational>, LexerError> {
    let imaginary = parse_exact_component(&slice[..slice.len() - 1])?;
    Ok(Complex::new(BigRational::zero(), imaginary))
}

fn parse_exact_unit_imaginary_complex(slice: &str) -> Result<Complex<BigRational>, LexerError> {
    let sign_index = slice
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_exact_component(&slice[..sign_index])?;
    let imaginary = if slice[sign_index..].starts_with('-') {
        -1
    } else {
        1
    };

    Ok(Complex::new(
        real,
        BigRational::from_integer(BigInt::from(imaginary)),
    ))
}

fn parse_exact_component(slice: &str) -> Result<BigRational, LexerError> {
    let slice = slice.strip_prefix('+').unwrap_or(slice);
    let Some((numerator, denominator)) = slice.split_once('/') else {
        if has_decimal_syntax(slice) {
            let (numerator, denominator) = parse_exact_decimal_literal(slice)?;
            return Ok(BigRational::new(numerator, denominator));
        }
        return Ok(BigRational::from_integer(BigInt::from_str(slice)?));
    };

    let numerator = BigInt::from_str(numerator)?;
    let denominator = BigInt::from_str(denominator)?;
    if denominator.is_zero() {
        return Err(LexerError::ZeroDenominator);
    }

    Ok(BigRational::new(numerator, denominator))
}

fn parse_radix_exact_rectangular_complex(slice: &str) -> Result<Complex<BigRational>, LexerError> {
    let (radix, body) = strip_exact_radix_prefix(slice)?;
    let sign_index = body
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_exact_radix_component(&body[..sign_index], radix)?;
    let imaginary = parse_exact_radix_component(
        body[sign_index..]
            .strip_suffix('i')
            .ok_or(LexerError::DefaultError)?,
        radix,
    )?;

    Ok(Complex::new(real, imaginary))
}

fn parse_radix_exact_imaginary_unit(slice: &str) -> Result<Complex<BigRational>, LexerError> {
    let (_, body) = strip_exact_radix_prefix(slice)?;
    Ok(parse_exact_imaginary_unit(body))
}

fn parse_radix_exact_pure_imaginary(slice: &str) -> Result<Complex<BigRational>, LexerError> {
    let (radix, body) = strip_exact_radix_prefix(slice)?;
    let imaginary = parse_exact_radix_component(&body[..body.len() - 1], radix)?;
    Ok(Complex::new(BigRational::zero(), imaginary))
}

fn parse_radix_exact_unit_imaginary_complex(
    slice: &str,
) -> Result<Complex<BigRational>, LexerError> {
    let (radix, body) = strip_exact_radix_prefix(slice)?;
    let sign_index = body
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_exact_radix_component(&body[..sign_index], radix)?;
    let imaginary = if body[sign_index..].starts_with('-') {
        -1
    } else {
        1
    };

    Ok(Complex::new(
        real,
        BigRational::from_integer(BigInt::from(imaginary)),
    ))
}

fn strip_exact_radix_prefix(slice: &str) -> Result<(u32, &str), LexerError> {
    let bytes = slice.as_bytes();
    match bytes {
        [b'#', radix, rest @ ..] if is_radix_prefix_byte(*radix) => {
            Ok((radix_value(*radix)?, &slice[slice.len() - rest.len()..]))
        }
        [b'#', exactness, b'#', radix, rest @ ..] if is_exactness_prefix_byte(*exactness) => {
            Ok((radix_value(*radix)?, &slice[slice.len() - rest.len()..]))
        }
        [b'#', radix, b'#', exactness, rest @ ..] if is_exactness_prefix_byte(*exactness) => {
            Ok((radix_value(*radix)?, &slice[slice.len() - rest.len()..]))
        }
        _ => Err(LexerError::DefaultError),
    }
}

fn is_exactness_prefix_byte(byte: u8) -> bool {
    matches!(byte, b'e' | b'E')
}

fn is_radix_prefix_byte(byte: u8) -> bool {
    matches!(byte, b'b' | b'B' | b'o' | b'O' | b'x' | b'X')
}

fn radix_value(byte: u8) -> Result<u32, LexerError> {
    match byte {
        b'b' | b'B' => Ok(2),
        b'o' | b'O' => Ok(8),
        b'x' | b'X' => Ok(16),
        _ => Err(LexerError::DefaultError),
    }
}

fn parse_exact_radix_component(slice: &str, radix: u32) -> Result<BigRational, LexerError> {
    let slice = slice.strip_prefix('+').unwrap_or(slice);
    let Some((numerator, denominator)) = slice.split_once('/') else {
        return BigInt::from_str_radix(slice, radix)
            .map(BigRational::from_integer)
            .map_err(LexerError::from);
    };

    let numerator = BigInt::from_str_radix(numerator, radix)?;
    let denominator = BigInt::from_str_radix(denominator, radix)?;
    if denominator.is_zero() {
        return Err(LexerError::ZeroDenominator);
    }

    Ok(BigRational::new(numerator, denominator))
}

fn has_decimal_syntax(slice: &str) -> bool {
    slice.chars().any(|ch| {
        matches!(
            ch,
            '.' | 'e' | 'E' | 's' | 'S' | 'f' | 'F' | 'd' | 'D' | 'l' | 'L'
        )
    })
}

fn parse_rectangular_complex(slice: &str) -> Result<Complex<BigDecimal>, LexerError> {
    let sign_index = slice
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_decimal_component(&slice[..sign_index])?;
    let imaginary = parse_decimal_component(
        slice[sign_index..]
            .strip_suffix('i')
            .ok_or(LexerError::DefaultError)?,
    )?;

    Ok(Complex::new(real, imaginary))
}

fn parse_decimal_component(slice: &str) -> Result<BigDecimal, ParseBigDecimalError> {
    BigDecimal::from_str(slice.strip_prefix('+').unwrap_or(slice))
}

fn parse_imaginary_unit(slice: &str) -> Complex<BigDecimal> {
    let imaginary = if slice.starts_with('-') { -1 } else { 1 };
    Complex::new(BigDecimal::from(0), BigDecimal::from(imaginary))
}

fn parse_pure_imaginary(slice: &str) -> Result<Complex<BigDecimal>, LexerError> {
    let imaginary = BigDecimal::from_str(&slice[..slice.len() - 1])?;
    Ok(Complex::new(BigDecimal::from(0), imaginary))
}

fn parse_unit_imaginary_complex(slice: &str) -> Result<Complex<BigDecimal>, LexerError> {
    let sign_index = slice
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = BigDecimal::from_str(&slice[..sign_index])?;
    let imaginary = if slice[sign_index..].starts_with('-') {
        -1
    } else {
        1
    };

    Ok(Complex::new(real, BigDecimal::from(imaginary)))
}

fn parse_inexact_decimal_radix_rectangular_complex(
    slice: &str,
) -> Result<Complex<BigDecimal>, LexerError> {
    let body = strip_decimal_radix_prefixes(slice)?;
    let sign_index = body
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_inexact_decimal_radix_component(&body[..sign_index])?;
    let imaginary = parse_inexact_decimal_radix_component(
        body[sign_index..]
            .strip_suffix('i')
            .ok_or(LexerError::DefaultError)?,
    )?;

    Ok(Complex::new(real, imaginary))
}

fn parse_inexact_decimal_radix_imaginary_unit(
    slice: &str,
) -> Result<Complex<BigDecimal>, LexerError> {
    let body = strip_decimal_radix_prefixes(slice)?;
    Ok(parse_imaginary_unit(body))
}

fn parse_inexact_decimal_radix_pure_imaginary(
    slice: &str,
) -> Result<Complex<BigDecimal>, LexerError> {
    let body = strip_decimal_radix_prefixes(slice)?;
    let imaginary = parse_inexact_decimal_radix_component(&body[..body.len() - 1])?;
    Ok(Complex::new(BigDecimal::from(0), imaginary))
}

fn parse_inexact_decimal_radix_unit_imaginary_complex(
    slice: &str,
) -> Result<Complex<BigDecimal>, LexerError> {
    let body = strip_decimal_radix_prefixes(slice)?;
    let sign_index = body
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_inexact_decimal_radix_component(&body[..sign_index])?;
    let imaginary = if body[sign_index..].starts_with('-') {
        -1
    } else {
        1
    };

    Ok(Complex::new(real, BigDecimal::from(imaginary)))
}

fn strip_number_prefixes(slice: &str) -> &str {
    if matches!(
        slice.get(..4),
        Some(
            "#e#d"
                | "#E#d"
                | "#e#D"
                | "#E#D"
                | "#d#e"
                | "#D#e"
                | "#d#E"
                | "#D#E"
                | "#i#d"
                | "#I#d"
                | "#i#D"
                | "#I#D"
                | "#d#i"
                | "#D#i"
                | "#d#I"
                | "#D#I"
        )
    ) {
        &slice[4..]
    } else {
        &slice[2..]
    }
}

fn strip_decimal_radix_prefixes(slice: &str) -> Result<&str, LexerError> {
    match slice.get(..4) {
        Some("#i#d" | "#I#d" | "#i#D" | "#I#D" | "#d#i" | "#D#i" | "#d#I" | "#D#I") => {
            Ok(&slice[4..])
        }
        _ if matches!(slice.get(..2), Some("#d" | "#D")) => Ok(&slice[2..]),
        _ => Err(LexerError::DefaultError),
    }
}

fn parse_inexact_decimal_radix_component(slice: &str) -> Result<BigDecimal, LexerError> {
    let slice = slice.strip_prefix('+').unwrap_or(slice);
    let Some((numerator, denominator)) = slice.split_once('/') else {
        if has_decimal_syntax(slice) {
            return parse_decimal_literal(slice).map_err(LexerError::from);
        }
        return BigDecimal::from_str(slice).map_err(LexerError::from);
    };

    let numerator = BigInt::from_str(numerator)?;
    let denominator = BigInt::from_str(denominator)?;
    if denominator.is_zero() {
        return Err(LexerError::ZeroDenominator);
    }

    Ok(BigDecimal::from(numerator) / BigDecimal::from(denominator))
}

fn parse_inexact_decimal_radix_polar_complex(
    slice: &str,
) -> Result<Complex<BigDecimal>, LexerError> {
    let body = strip_decimal_radix_prefixes(slice)?;
    let (magnitude, angle) = body
        .split_once('@')
        .expect("polar complex token regex guarantees an at sign");
    polar_from_decimal_components(
        parse_inexact_decimal_radix_component(magnitude)?,
        parse_inexact_decimal_radix_component(angle)?,
    )
}

fn parse_inexact_radix_rectangular_complex(slice: &str) -> Result<Complex<BigDecimal>, LexerError> {
    let (radix, body) = strip_inexact_radix_prefix(slice)?;
    let sign_index = body
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_inexact_radix_component(&body[..sign_index], radix)?;
    let imaginary = parse_inexact_radix_component(
        body[sign_index..]
            .strip_suffix('i')
            .ok_or(LexerError::DefaultError)?,
        radix,
    )?;

    Ok(Complex::new(real, imaginary))
}

fn parse_inexact_radix_imaginary_unit(slice: &str) -> Result<Complex<BigDecimal>, LexerError> {
    let (_, body) = strip_inexact_radix_prefix(slice)?;
    Ok(parse_imaginary_unit(body))
}

fn parse_inexact_radix_pure_imaginary(slice: &str) -> Result<Complex<BigDecimal>, LexerError> {
    let (radix, body) = strip_inexact_radix_prefix(slice)?;
    let imaginary = parse_inexact_radix_component(&body[..body.len() - 1], radix)?;
    Ok(Complex::new(BigDecimal::from(0), imaginary))
}

fn parse_inexact_radix_unit_imaginary_complex(
    slice: &str,
) -> Result<Complex<BigDecimal>, LexerError> {
    let (radix, body) = strip_inexact_radix_prefix(slice)?;
    let sign_index = body
        .char_indices()
        .skip(1)
        .find(|(_, ch)| matches!(ch, '+' | '-'))
        .map(|(index, _)| index)
        .ok_or(LexerError::DefaultError)?;
    let real = parse_inexact_radix_component(&body[..sign_index], radix)?;
    let imaginary = if body[sign_index..].starts_with('-') {
        -1
    } else {
        1
    };

    Ok(Complex::new(real, BigDecimal::from(imaginary)))
}

fn strip_inexact_radix_prefix(slice: &str) -> Result<(u32, &str), LexerError> {
    let bytes = slice.as_bytes();
    match bytes {
        [b'#', exactness, b'#', radix, rest @ ..] if is_inexactness_prefix_byte(*exactness) => {
            Ok((radix_value(*radix)?, &slice[slice.len() - rest.len()..]))
        }
        [b'#', radix, b'#', exactness, rest @ ..] if is_inexactness_prefix_byte(*exactness) => {
            Ok((radix_value(*radix)?, &slice[slice.len() - rest.len()..]))
        }
        _ => Err(LexerError::DefaultError),
    }
}

fn is_inexactness_prefix_byte(byte: u8) -> bool {
    matches!(byte, b'i' | b'I')
}

fn parse_inexact_radix_component(slice: &str, radix: u32) -> Result<BigDecimal, LexerError> {
    let slice = slice.strip_prefix('+').unwrap_or(slice);
    let Some((numerator, denominator)) = slice.split_once('/') else {
        return parse_inexact_radix_literal(slice, radix);
    };

    let numerator = BigInt::from_str_radix(numerator, radix)?;
    let denominator = BigInt::from_str_radix(denominator, radix)?;
    if denominator.is_zero() {
        return Err(LexerError::ZeroDenominator);
    }

    Ok(BigDecimal::from(numerator) / BigDecimal::from(denominator))
}

fn parse_polar_complex(slice: &str) -> Result<Complex<BigDecimal>, LexerError> {
    let (magnitude, angle) = slice
        .split_once('@')
        .expect("polar complex token regex guarantees an at sign");
    let magnitude = BigDecimal::from_str(magnitude)?;
    let angle = BigDecimal::from_str(angle)?;
    polar_from_decimal_components(magnitude, angle)
}

fn polar_from_decimal_components(
    magnitude: BigDecimal,
    angle: BigDecimal,
) -> Result<Complex<BigDecimal>, LexerError> {
    let magnitude = magnitude.to_f64().ok_or(LexerError::DefaultError)?;
    let angle = angle.to_f64().ok_or(LexerError::DefaultError)?;

    Ok(Complex::new(
        decimal_from_f64(magnitude * angle.cos())?,
        decimal_from_f64(magnitude * angle.sin())?,
    ))
}

fn decimal_from_f64(number: f64) -> Result<BigDecimal, LexerError> {
    BigDecimal::from_f64(number)
        .map(|number| number.normalized())
        .ok_or(LexerError::DefaultError)
}

fn normalize_decimal_exponent(slice: &str) -> String {
    slice
        .chars()
        .map(|ch| {
            if is_decimal_exponent_marker(ch) {
                'e'
            } else {
                ch
            }
        })
        .collect()
}

fn split_decimal_exponent(magnitude: &str) -> Result<(&str, i64), LexerError> {
    let Some((index, _)) = magnitude
        .char_indices()
        .find(|(_, ch)| is_decimal_exponent_marker(*ch))
    else {
        return Ok((magnitude, 0));
    };

    let exponent = BigInt::from_str(&magnitude[index + 1..])?
        .to_i64()
        .ok_or(LexerError::DefaultError)?;
    Ok((&magnitude[..index], exponent))
}

fn is_decimal_exponent_marker(ch: char) -> bool {
    matches!(
        ch,
        'e' | 'E' | 's' | 'S' | 'f' | 'F' | 'd' | 'D' | 'l' | 'L'
    )
}

fn decimal_scale(power: i64) -> Result<BigInt, LexerError> {
    let power = u32::try_from(power).map_err(|_| LexerError::DefaultError)?;
    Ok(BigInt::from(10).pow(power))
}

fn parse_inexact_fraction_literal(slice: &str) -> Result<BigDecimal, LexerError> {
    let (numerator, denominator) = parse_ratio_literal(slice)?;

    Ok(BigDecimal::from(numerator) / BigDecimal::from(denominator))
}

fn parse_inexact_radix_literal(slice: &str, radix: u32) -> Result<BigDecimal, LexerError> {
    BigInt::from_str_radix(slice, radix)
        .map(BigDecimal::from)
        .map_err(LexerError::from)
}

fn parse_inexact_radix_fraction_literal(slice: &str, radix: u32) -> Result<BigDecimal, LexerError> {
    let (numerator, denominator) = parse_radix_ratio_literal(slice, radix)?;

    Ok(BigDecimal::from(numerator) / BigDecimal::from(denominator))
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

pub fn tokenize_checked(input: &str) -> Result<Vec<(Token, &str, Span)>, SpannedLexerError> {
    let mut lexer = Token::lexer(input);
    let mut delimiters = DelimiterState::default();
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        let span = lexer.span();
        match token {
            Ok(token) => {
                delimiters.observe(&token, span.clone())?;
                tokens.push((token, &input[span.clone()], span));
            }
            Err(error) => return Err(SpannedLexerError { error, span }),
        }
    }

    Ok(tokens)
}

#[derive(Default)]
pub(crate) struct DelimiterState {
    pending: Option<Span>,
    saw_delimiter: bool,
}

impl DelimiterState {
    pub(crate) fn observe(&mut self, token: &Token, span: Span) -> Result<(), SpannedLexerError> {
        if token_is_delimiter_trivia(token) {
            self.saw_delimiter = self.pending.is_some();
            return Ok(());
        }

        if let Some(pending) = &self.pending
            && !self.saw_delimiter
            && !token_starts_delimiter(token)
        {
            return Err(SpannedLexerError {
                error: LexerError::MissingDelimiter,
                span: pending.start..span.end,
            });
        }

        self.pending = token_requires_delimiter(token).then_some(span);
        self.saw_delimiter = false;
        Ok(())
    }

    pub(crate) fn ready_to_end_datum(&self) -> bool {
        self.pending.is_none() || self.saw_delimiter
    }
}

fn token_is_delimiter_trivia(token: &Token) -> bool {
    matches!(
        token,
        Token::Whitespace(_) | Token::LineComment | Token::BlockComment
    )
}

fn token_starts_delimiter(token: &Token) -> bool {
    matches!(
        token,
        Token::LParen | Token::RParen | Token::LBracket | Token::RBracket | Token::String(_)
    )
}

fn token_requires_delimiter(token: &Token) -> bool {
    matches!(
        token,
        Token::Identifier(_)
            | Token::Integer(_)
            | Token::Real(_)
            | Token::ExactComplex(_)
            | Token::Complex(_)
            | Token::Decimal(_)
            | Token::Binary(_)
            | Token::Octal(_)
            | Token::Hex(_)
            | Token::DecInteger(_)
            | Token::Character(_)
            | Token::True
            | Token::False
            | Token::Dot
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use color_eyre::Result;
    use color_eyre::eyre::ensure;

    #[track_caller]
    fn test_single(s: &str, pred: impl Fn(&Token) -> bool) -> Result<()> {
        let tokens = tokenize(s);

        ensure!(!tokens.is_empty(), "no tokens parsed: {}", s);
        ensure!(pred(&tokens[0].0), "test failed: {}", s);

        Ok(())
    }

    #[test]
    fn test_identifiers() {
        let input = "foo bar+ set! string->symbol MixedCase foo.bar";
        let tokens = tokenize(input);

        assert_eq!(tokens.len(), 11); // 6 identifiers + 5 whitespaces
        assert!(tokens[0].0.is_identifier());
        assert_eq!(tokens[0].1, "foo");
        assert!(tokens[2].0.is_identifier());
        assert_eq!(tokens[2].1, "bar+");
        assert!(tokens[4].0.is_identifier());
        assert_eq!(tokens[4].1, "set!");
        assert!(tokens[6].0.is_identifier());
        assert_eq!(tokens[6].1, "string->symbol");
        assert_eq!(tokens[8].0, Token::Identifier("mixedcase".to_string()));
        assert_eq!(tokens[10].0, Token::Identifier("foo.bar".to_string()));
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
        test_single("^", Token::is_identifier)?;
        test_single("_", Token::is_identifier)?;
        test_single("~", Token::is_identifier)?;
        // dot omitted because it is not a valid identifier on its own

        test_single("lambda", Token::is_identifier)?;
        test_single("list->vector", Token::is_identifier)?;
        test_single("<=?", Token::is_identifier)?;
        test_single("the-word-recursion-has-many-meanings", Token::is_identifier)?;
        test_single("one.two", Token::is_identifier)?;
        test_single("q", Token::is_identifier)?;
        test_single("soup", Token::is_identifier)?;
        test_single("V17a", Token::is_identifier)?;
        test_single("a34kTMNs", Token::is_identifier)?;

        test_single("...", Token::is_identifier)?;

        Ok(())
    }

    #[test]
    fn rejects_non_r5rs_identifier_initials() {
        assert!(matches!(
            tokenize_checked("@"),
            Err(SpannedLexerError {
                error: LexerError::DefaultError,
                ..
            })
        ));
        for input in ["+abc", "-abc"] {
            assert!(
                matches!(
                    tokenize_checked(input),
                    Err(SpannedLexerError {
                        error: LexerError::MissingDelimiter,
                        ..
                    })
                ),
                "{input} should not start a general identifier"
            );
        }
    }

    #[test]
    fn test_linecomment() -> Result<()> {
        test_single(
            ";;; The Fact procedure computes the factorial",
            Token::is_line_comment,
        )?;
        test_single(";;", Token::is_line_comment)?;
        test_single(";", Token::is_line_comment)?;

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

        let decimals = tokenize(".5 1. -0.");
        assert!(decimals[0].0.is_decimal());
        assert_eq!(decimals[0].1, ".5");
        assert!(decimals[2].0.is_decimal());
        assert_eq!(decimals[2].1, "1.");
        assert!(decimals[4].0.is_decimal());
        assert_eq!(decimals[4].1, "-0.");

        let signed = tokenize("#b-1010 #o+10 #x-ff #d-12");
        assert_eq!(signed[0].0, Token::Binary(BigInt::from(-10)));
        assert_eq!(signed[2].0, Token::Octal(BigInt::from(8)));
        assert_eq!(signed[4].0, Token::Hex(BigInt::from(-255)));
        assert_eq!(signed[6].0, Token::DecInteger(BigInt::from(-12)));

        let exactness = tokenize("#e10 #i10 #e3/2 #i1/2 #e1.5 #i1.5 #e#x10 #x#i10");
        assert_eq!(exactness[0].0, Token::Integer(BigInt::from(10)));
        assert_eq!(exactness[2].0, Token::Decimal(BigDecimal::from(10)));
        assert_eq!(
            exactness[4].0,
            Token::Real((BigInt::from(3), BigInt::from(2)))
        );
        assert_eq!(
            exactness[6].0,
            Token::Decimal(BigDecimal::from_str("0.5").unwrap())
        );
        assert_eq!(
            exactness[8].0,
            Token::Real((BigInt::from(15), BigInt::from(10)))
        );
        assert_eq!(
            exactness[10].0,
            Token::Decimal(BigDecimal::from_str("1.5").unwrap())
        );
        assert_eq!(exactness[12].0, Token::Hex(BigInt::from(16)));
        assert_eq!(exactness[14].0, Token::Decimal(BigDecimal::from(16)));

        let exponents = tokenize("1e2 -1.5e+2 .5e1 #i1e2 #d1e2 #e1e2 #e1.25e1 #e1.25e-1");
        assert_eq!(
            exponents[0].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );
        assert_eq!(
            exponents[2].0,
            Token::Decimal(BigDecimal::from_str("-1.5e+2").unwrap())
        );
        assert_eq!(
            exponents[4].0,
            Token::Decimal(BigDecimal::from_str(".5e1").unwrap())
        );
        assert_eq!(
            exponents[6].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );
        assert_eq!(
            exponents[8].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );
        assert_eq!(
            exponents[10].0,
            Token::Real((BigInt::from(100), BigInt::from(1)))
        );
        assert_eq!(
            exponents[12].0,
            Token::Real((BigInt::from(125), BigInt::from(10)))
        );
        assert_eq!(
            exponents[14].0,
            Token::Real((BigInt::from(125), BigInt::from(1000)))
        );

        let exponent_markers = tokenize("1s2 1f2 1d2 1l2 #e1d2 #d#i1l2");
        assert_eq!(
            exponent_markers[0].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );
        assert_eq!(
            exponent_markers[2].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );
        assert_eq!(
            exponent_markers[4].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );
        assert_eq!(
            exponent_markers[6].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );
        assert_eq!(
            exponent_markers[8].0,
            Token::Real((BigInt::from(100), BigInt::from(1)))
        );
        assert_eq!(
            exponent_markers[10].0,
            Token::Decimal(BigDecimal::from_str("1e2").unwrap())
        );

        let radix_rationals = tokenize("#d3/2 #d1.5 #b101/10 #o10/4 #x10/4 #i#b101/10");
        assert_eq!(
            radix_rationals[0].0,
            Token::Real((BigInt::from(3), BigInt::from(2)))
        );
        assert_eq!(
            radix_rationals[2].0,
            Token::Decimal(BigDecimal::from_str("1.5").unwrap())
        );
        assert_eq!(
            radix_rationals[4].0,
            Token::Real((BigInt::from(5), BigInt::from(2)))
        );
        assert_eq!(
            radix_rationals[6].0,
            Token::Real((BigInt::from(8), BigInt::from(4)))
        );
        assert_eq!(
            radix_rationals[8].0,
            Token::Real((BigInt::from(16), BigInt::from(4)))
        );
        assert_eq!(
            radix_rationals[10].0,
            Token::Decimal(BigDecimal::from_str("2.5").unwrap())
        );

        let imaginary = tokenize("+i -i 2i -2.5i 1+i 1-i #i+i");
        assert_eq!(
            imaginary[0].0,
            Token::ExactComplex(Complex::new(
                BigRational::zero(),
                BigRational::from_integer(BigInt::from(1))
            ))
        );
        assert_eq!(
            imaginary[2].0,
            Token::ExactComplex(Complex::new(
                BigRational::zero(),
                BigRational::from_integer(BigInt::from(-1))
            ))
        );
        assert_eq!(
            imaginary[4].0,
            Token::ExactComplex(Complex::new(
                BigRational::zero(),
                BigRational::from_integer(BigInt::from(2))
            ))
        );
        assert_eq!(
            imaginary[6].0,
            Token::Complex(Complex::new(
                BigDecimal::from(0),
                BigDecimal::from_str("-2.5").unwrap()
            ))
        );
        assert_eq!(
            imaginary[8].0,
            Token::ExactComplex(Complex::new(
                BigRational::from_integer(BigInt::from(1)),
                BigRational::from_integer(BigInt::from(1))
            ))
        );
        assert_eq!(
            imaginary[10].0,
            Token::ExactComplex(Complex::new(
                BigRational::from_integer(BigInt::from(1)),
                BigRational::from_integer(BigInt::from(-1))
            ))
        );
        assert_eq!(
            imaginary[12].0,
            Token::Complex(Complex::new(BigDecimal::from(0), BigDecimal::from(1)))
        );

        let exact_rectangular = tokenize(
            "1/2+3/4i #e1/2+3/4i #e+i #e1.5+2.25i #e.5+1e2i #b101+10i #x1/2+3/4i #e#x1+2i #x#e+i #d1+2i #e#d1.5+2.25i #d#e.5+1e2i #d+i",
        );
        assert_eq!(
            exact_rectangular[0].0,
            Token::ExactComplex(Complex::new(
                BigRational::new(BigInt::from(1), BigInt::from(2)),
                BigRational::new(BigInt::from(3), BigInt::from(4))
            ))
        );
        assert_eq!(
            exact_rectangular[2].0,
            Token::ExactComplex(Complex::new(
                BigRational::new(BigInt::from(1), BigInt::from(2)),
                BigRational::new(BigInt::from(3), BigInt::from(4))
            ))
        );
        assert_eq!(
            exact_rectangular[4].0,
            Token::ExactComplex(Complex::new(
                BigRational::zero(),
                BigRational::from_integer(BigInt::from(1))
            ))
        );
        assert_eq!(
            exact_rectangular[6].0,
            Token::ExactComplex(Complex::new(
                BigRational::new(BigInt::from(15), BigInt::from(10)),
                BigRational::new(BigInt::from(225), BigInt::from(100))
            ))
        );
        assert_eq!(
            exact_rectangular[8].0,
            Token::ExactComplex(Complex::new(
                BigRational::new(BigInt::from(5), BigInt::from(10)),
                BigRational::from_integer(BigInt::from(100))
            ))
        );
        assert_eq!(
            exact_rectangular[10].0,
            Token::ExactComplex(Complex::new(
                BigRational::from_integer(BigInt::from(5)),
                BigRational::from_integer(BigInt::from(2))
            ))
        );
        assert_eq!(
            exact_rectangular[12].0,
            Token::ExactComplex(Complex::new(
                BigRational::new(BigInt::from(1), BigInt::from(2)),
                BigRational::new(BigInt::from(3), BigInt::from(4))
            ))
        );
        assert_eq!(
            exact_rectangular[14].0,
            Token::ExactComplex(Complex::new(
                BigRational::from_integer(BigInt::from(1)),
                BigRational::from_integer(BigInt::from(2))
            ))
        );
        assert_eq!(
            exact_rectangular[16].0,
            Token::ExactComplex(Complex::new(
                BigRational::zero(),
                BigRational::from_integer(BigInt::from(1))
            ))
        );
        assert_eq!(
            exact_rectangular[18].0,
            Token::ExactComplex(Complex::new(
                BigRational::from_integer(BigInt::from(1)),
                BigRational::from_integer(BigInt::from(2))
            ))
        );
        assert_eq!(
            exact_rectangular[20].0,
            Token::ExactComplex(Complex::new(
                BigRational::new(BigInt::from(15), BigInt::from(10)),
                BigRational::new(BigInt::from(225), BigInt::from(100))
            ))
        );
        assert_eq!(
            exact_rectangular[22].0,
            Token::ExactComplex(Complex::new(
                BigRational::new(BigInt::from(5), BigInt::from(10)),
                BigRational::from_integer(BigInt::from(100))
            ))
        );
        assert_eq!(
            exact_rectangular[24].0,
            Token::ExactComplex(Complex::new(
                BigRational::zero(),
                BigRational::from_integer(BigInt::from(1))
            ))
        );

        let inexact_radix_rectangular =
            tokenize("#i#b101+10i #b#i101/10+1/10i #i#x+i #x#i101-i #i#o10i");
        assert_eq!(
            inexact_radix_rectangular[0].0,
            Token::Complex(Complex::new(BigDecimal::from(5), BigDecimal::from(2)))
        );
        assert_eq!(
            inexact_radix_rectangular[2].0,
            Token::Complex(Complex::new(
                BigDecimal::from_str("2.5").unwrap(),
                BigDecimal::from_str("0.5").unwrap()
            ))
        );
        assert_eq!(
            inexact_radix_rectangular[4].0,
            Token::Complex(Complex::new(BigDecimal::from(0), BigDecimal::from(1)))
        );
        assert_eq!(
            inexact_radix_rectangular[6].0,
            Token::Complex(Complex::new(BigDecimal::from(257), BigDecimal::from(-1)))
        );
        assert_eq!(
            inexact_radix_rectangular[8].0,
            Token::Complex(Complex::new(BigDecimal::from(0), BigDecimal::from(8)))
        );

        let inexact_decimal_radix_rectangular =
            tokenize("#d1.5+2.25i #i#d1/2+3/4i #d#i+i #d#i1-i #d2.5i");
        assert_eq!(
            inexact_decimal_radix_rectangular[0].0,
            Token::Complex(Complex::new(
                BigDecimal::from_str("1.5").unwrap(),
                BigDecimal::from_str("2.25").unwrap()
            ))
        );
        assert_eq!(
            inexact_decimal_radix_rectangular[2].0,
            Token::Complex(Complex::new(
                BigDecimal::from_str("0.5").unwrap(),
                BigDecimal::from_str("0.75").unwrap()
            ))
        );
        assert_eq!(
            inexact_decimal_radix_rectangular[4].0,
            Token::Complex(Complex::new(BigDecimal::from(0), BigDecimal::from(1)))
        );
        assert_eq!(
            inexact_decimal_radix_rectangular[6].0,
            Token::Complex(Complex::new(BigDecimal::from(1), BigDecimal::from(-1)))
        );
        assert_eq!(
            inexact_decimal_radix_rectangular[8].0,
            Token::Complex(Complex::new(
                BigDecimal::from(0),
                BigDecimal::from_str("2.5").unwrap()
            ))
        );

        let rectangular = tokenize(".5+.5i #i-1.5+2.i");
        assert_eq!(
            rectangular[0].0,
            Token::Complex(Complex::new(
                BigDecimal::from_str("0.5").unwrap(),
                BigDecimal::from_str("0.5").unwrap()
            ))
        );
        assert_eq!(
            rectangular[2].0,
            Token::Complex(Complex::new(
                BigDecimal::from_str("-1.5").unwrap(),
                BigDecimal::from(2)
            ))
        );

        let polar = tokenize("1@0 #i2@0 #d3@0 #i#d1/2@0 #d#i1.5@0");
        assert_eq!(
            polar[0].0,
            Token::Complex(Complex::new(BigDecimal::from(1), BigDecimal::from(0)))
        );
        assert_eq!(
            polar[2].0,
            Token::Complex(Complex::new(BigDecimal::from(2), BigDecimal::from(0)))
        );
        assert_eq!(
            polar[4].0,
            Token::Complex(Complex::new(BigDecimal::from(3), BigDecimal::from(0)))
        );
        assert_eq!(
            polar[6].0,
            Token::Complex(Complex::new(
                BigDecimal::from_str("0.5").unwrap(),
                BigDecimal::from(0)
            ))
        );
        assert_eq!(
            polar[8].0,
            Token::Complex(Complex::new(
                BigDecimal::from_str("1.5").unwrap(),
                BigDecimal::from(0)
            ))
        );

        assert!(matches!(
            tokenize_checked("1/0"),
            Err(SpannedLexerError {
                error: LexerError::ZeroDenominator,
                ..
            })
        ));
        assert!(matches!(
            tokenize_checked("#x10/0"),
            Err(SpannedLexerError {
                error: LexerError::ZeroDenominator,
                ..
            })
        ));
    }

    #[test]
    fn rejects_missing_implicit_delimiters() {
        for input in ["1abc", "#tfoo", "#\\a1", "foo'bar", ".x", "1i+2"] {
            assert!(
                matches!(
                    tokenize_checked(input),
                    Err(SpannedLexerError {
                        error: LexerError::MissingDelimiter,
                        ..
                    })
                ),
                "{input} should require a delimiter"
            );
        }

        assert!(tokenize_checked("1+2").is_err());

        assert!(tokenize_checked("1(abc)").is_ok());
        assert!(tokenize_checked("#t; ok\n#f").is_ok());
        assert!(tokenize_checked("foo\"bar\"").is_ok());
    }

    #[test]
    fn test_strings_and_chars() {
        let input = r#""hello world" "escaped \"quotes\"" #\a #\space"#;
        let tokens = tokenize(input);

        assert!(tokens[0].0.is_string());
        assert!(tokens[2].0.is_string());
        assert!(tokens[4].0.is_character());
        assert!(tokens[6].0.is_character());

        assert!(matches!(
            tokenize_checked(r#""not an r5rs \n escape""#),
            Err(SpannedLexerError {
                error: LexerError::DefaultError,
                ..
            })
        ));
    }

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
    fn recognizes_highlight_categories() {
        assert!(is_special_form("set!"));
        assert!(is_special_form("DEFINE"));
        assert!(is_special_form("define-syntax"));
        assert!(is_special_form("letrec-syntax"));
        assert!(is_special_form("syntax-rules"));
        assert!(is_special_form("quote"));
        assert!(is_special_form("quasiquote"));
        assert!(is_special_form("unquote"));
        assert!(is_special_form("unquote-splicing"));
        assert!(is_special_form("=>"));
        assert!(is_special_form("..."));
        assert!(is_predicate("number?"));
        assert!(is_conversion("exact->inexact"));
    }

    #[test]
    fn test_comments() {
        let input = "; line comment\n#| block #| nested |# comment |#";
        let tokens = tokenize(input);

        assert_eq!(tokens[0].0, Token::LineComment);
        assert_eq!(tokens[2].0, Token::BlockComment);
    }

    #[test]
    fn test_booleans() {
        let input = "#t #f #T #F";
        let tokens = tokenize(input);

        assert_eq!(tokens[0].0, Token::True);
        assert_eq!(tokens[2].0, Token::False);
        assert_eq!(tokens[4].0, Token::True);
        assert_eq!(tokens[6].0, Token::False);
    }

    #[test]
    fn test_complex_expression() {
        let input = "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))";
        let tokens = tokenize(input);
        // This just checks if lexing completes without error
        assert!(!tokens.is_empty());
    }
}
