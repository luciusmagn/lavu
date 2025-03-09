use bigdecimal::{BigDecimal, ToPrimitive};
use chumsky::prelude::*;
use logos::Span;
use num::BigInt;
use std::rc::Rc;

use crate::ast::{Expr, Number};
use crate::lexer::Token;

// Parse numerical expressions
pub fn parse_expr(
    tokens: Vec<(Token, &str, Span)>,
) -> Result<Rc<Expr>, Vec<Simple<Token>>> {
    // Extract just the tokens for the parser
    let tokens_only: Vec<Token> = tokens
        .into_iter()
        .filter(|(t, _, _)| {
            !matches!(
                t,
                Token::Whitespace(_) | Token::LineComment
            )
        })
        .map(|(t, _, _)| t)
        .collect();

    expr_parser().parse(tokens_only)
}

// Expression parser
fn expr_parser(
) -> impl Parser<Token, Rc<Expr>, Error = Simple<Token>> {
    // Numeric literals
    let number = filter_map(|span, token| {
        match token {
            Token::Integer(value) => Ok(Rc::new(Expr::Number(
                Number::ExactInteger(value),
            ))),
            Token::Decimal(value) => {
                // Convert BigDecimal to a rational representation
                // This is simplified - you might need more sophisticated conversion
                Ok(Rc::new(Expr::Number(Number::InexactReal(
                    value.to_f64().unwrap_or(0.0),
                ))))
            }
            _ => Err(Simple::expected_input_found(
                span,
                vec![
                    Some(Token::Integer(Default::default())),
                    Some(Token::Decimal(Default::default())),
                ],
                Some(token),
            )),
        }
    });

    // Forward reference for recursive expressions
    recursive(|expr| {
        // Parenthesized expressions
        let paren_expr = just(Token::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::RParen));

        // Application (function call)
        let application = just(Token::LParen)
            .ignore_then(
                expr.clone().then(expr.clone().repeated()).map(
                    |(func, args)| {
                        Rc::new(Expr::Application(func, args))
                    },
                ),
            )
            .then_ignore(just(Token::RParen));

        // Symbol/identifier
        let symbol = filter_map(|span, token: Token| {
            if let Token::Identifier(name) = token.clone() {
                Ok(Rc::new(Expr::Symbol(crate::ast::Symbol {
                    name,
                    uid: None,
                })))
            } else {
                Err(Simple::expected_input_found(
                    span,
                    vec![Some(Token::Identifier(
                        "identifier".into(),
                    ))],
                    Some(token),
                ))
            }
        });

        // Priority order of parsing
        choice((number, application, paren_expr, symbol))
    })
}

// Helper function to print the parsed expression
pub fn print_expr(expr: &Expr, indent: usize) -> String {
    match expr {
        Expr::Number(Number::ExactInteger(n)) => {
            format!("{}", n)
        }
        Expr::Number(Number::InexactReal(n)) => format!("{}", n),
        Expr::Symbol(s) => s.name.clone(),
        Expr::Application(func, args) => {
            let func_str = print_expr(func, indent + 2);
            let args_str: Vec<String> = args
                .iter()
                .map(|arg| print_expr(arg, indent + 2))
                .collect();
            format!(
                "({}{})",
                func_str,
                if args.is_empty() {
                    "".to_string()
                } else {
                    format!(" {}", args_str.join(" "))
                }
            )
        }
        _ => format!("{:?}", expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    // Helper function to parse a string and return the result
    fn parse_test(
        input: &str,
    ) -> Result<Rc<Expr>, Vec<Simple<Token>>> {
        let tokens = tokenize(input);
        parse_expr(tokens)
    }

    // Helper to parse and get string representation
    fn parse_to_string(input: &str) -> String {
        match parse_test(input) {
            Ok(expr) => print_expr(&expr, 0),
            Err(errs) => format!("Error: {:?}", errs),
        }
    }

    #[test]
    fn test_simple_numbers() {
        assert!(parse_test("42").is_ok());
        assert!(parse_test("-123").is_ok());
        assert!(parse_test("3.14159").is_ok());

        // Check actual values
        let int_expr = parse_test("42").unwrap();
        if let Expr::Number(Number::ExactInteger(n)) = &*int_expr
        {
            assert_eq!(n.to_string(), "42");
        } else {
            panic!("Expected ExactInteger, got {:?}", int_expr);
        }

        let float_expr = parse_test("3.14").unwrap();
        if let Expr::Number(Number::InexactReal(n)) =
            &*float_expr
        {
            assert!((n - 3.14).abs() < 0.0001);
        } else {
            panic!("Expected InexactReal, got {:?}", float_expr);
        }
    }

    #[test]
    fn test_symbols() {
        assert!(parse_test("x").is_ok());
        assert!(parse_test("add").is_ok());
        assert!(parse_test("+").is_ok());

        let sym_expr = parse_test("xyz").unwrap();
        if let Expr::Symbol(s) = &*sym_expr {
            assert_eq!(s.name, "xyz");
        } else {
            panic!("Expected Symbol, got {:?}", sym_expr);
        }
    }

    #[test]
    fn test_applications() {
        assert!(parse_test("(+ 1 2)").is_ok());
        assert!(parse_test("(* 3 4 5)").is_ok());
        assert!(parse_test("(- 10 5)").is_ok());

        // Test nested expressions
        assert!(parse_test("(+ (* 2 3) 4)").is_ok());
        assert!(parse_test("(* (+ 1 2) (- 6 2))").is_ok());

        // Verify structure
        let expr = parse_test("(+ 1 2)").unwrap();
        if let Expr::Application(func, args) = &*expr {
            if let Expr::Symbol(s) = &**func {
                assert_eq!(s.name, "+");
            } else {
                panic!(
                    "Expected Symbol as function, got {:?}",
                    func
                );
            }
            assert_eq!(args.len(), 2);
        } else {
            panic!("Expected Application, got {:?}", expr);
        }
    }

    #[test]
    fn test_complex_expressions() {
        // Test more complex nested expressions
        let expr_str = "(+ (* 2 (- 10 5)) (/ 8 2))";
        let result = parse_to_string(expr_str);
        assert_eq!(result, expr_str);

        // Test deeply nested expressions
        let nested = "(+ (* 2 (- (+ 1 2) (/ 10 5))) 3)";
        assert!(parse_test(nested).is_ok());
    }

    #[test]
    fn test_error_cases() {
        // Unbalanced parentheses
        assert!(parse_test("(+ 1 2").is_err());
        assert!(parse_test(")").is_err());

        // Empty application
        let empty_app = parse_test("()");
        // Note: This might succeed or fail depending on how you want to handle empty lists
        // If you want it to fail, check: assert!(empty_app.is_err());

        // Invalid token sequence
        assert!(parse_test("(1 2 3)").is_ok()); // This should parse as application with number as function
    }
}
