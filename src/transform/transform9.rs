use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;
use std::ops::Range;

use crate::ast::ast1::Atom;
use crate::ast::ast16::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

pub fn transform(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    ast: &Program,
) -> Result<Program> {
    let forms = ast
        .forms
        .iter()
        .map(|form| transform_top_level_form(form))
        .collect::<Result<Vec<_>>>()?;

    Ok(Program {
        forms,
        source: input.to_string(),
        tokens: tokens
            .iter()
            .map(|(t, s, l)| (t.clone(), s.to_string(), l.clone()))
            .collect(),
    })
}

fn transform_top_level_form(form: &TopLevelForm) -> Result<TopLevelForm> {
    match form {
        TopLevelForm::Definition(def) => Ok(TopLevelForm::Definition(transform_definition(def)?)),
        TopLevelForm::Expression(expr) => {
            let new_expr = transform_expression(expr)?;
            Ok(TopLevelForm::Expression(new_expr))
        }
    }
}

fn transform_definition(def: &Definition) -> Result<Definition> {
    match def {
        Definition::Variable((name, span), expr) => {
            let new_expr = transform_expression(expr)?;
            Ok(Definition::Variable(
                (name.clone(), span.clone()),
                Box::new(new_expr),
            ))
        }
    }
}

fn transform_expression(expr: &Expression) -> Result<Expression> {
    match expr {
        // Transform and
        Expression::And(exprs, span) => {
            // Handle special cases
            if exprs.is_empty() {
                // (and) -> #t
                return Ok(Expression::Atom(Atom::Boolean(true), span.clone()));
            }

            if exprs.len() == 1 {
                // (and x) -> x
                return transform_expression(&exprs[0]);
            }

            // Transform to nested ifs: (and a b c) -> (if a (if b c #f) #f)
            let mut result = transform_expression(&exprs[exprs.len() - 1])?;

            // Build the nested if from right to left
            for expr in exprs.iter().rev().skip(1) {
                let test = transform_expression(expr)?;
                result = Expression::If(
                    Box::new(test.clone()),
                    Box::new(result),
                    Some(Box::new(Expression::Atom(
                        Atom::Boolean(false),
                        span.clone(),
                    ))),
                    span.clone(),
                );
            }

            Ok(result)
        }

        // Transform or
        Expression::Or(exprs, span) => {
            // Handle special cases
            if exprs.is_empty() {
                // (or) -> #f
                return Ok(Expression::Atom(Atom::Boolean(false), span.clone()));
            }

            if exprs.len() == 1 {
                // (or x) -> x
                return transform_expression(&exprs[0]);
            }

            // To correctly implement (or a b c), we need to ensure a is evaluated only once
            // but its value is potentially used twice (for the test and the result if true).
            //
            // We'll create a nested let-if structure for each expression except the last:
            // (or a b c) ->
            // (let ((temp a))
            //   (if temp
            //       temp
            //       (let ((temp b))
            //         (if temp
            //             temp
            //             c))))
            //
            // However, since we've already transformed all let expressions to lambda applications,
            // we'll directly use the lambda form:

            // Start with the last expression
            let mut result = transform_expression(&exprs[exprs.len() - 1])?;

            // Build the nested structure from right to left
            for expr in exprs.iter().rev().skip(1) {
                let test_expr = transform_expression(expr)?;

                // Create a unique temp variable name
                let temp_var = ("temp".to_string(), span.clone());

                // Create the lambda-application (let equivalent)
                let lambda = Expression::Lambda(
                    vec![temp_var.clone()],
                    vec![Expression::If(
                        Box::new(Expression::Atom(
                            crate::ast::ast1::Atom::Identifier(temp_var.0.clone()),
                            temp_var.1.clone(),
                        )),
                        Box::new(Expression::Atom(
                            crate::ast::ast1::Atom::Identifier(temp_var.0.clone()),
                            temp_var.1.clone(),
                        )),
                        Some(Box::new(result)),
                        span.clone(),
                    )],
                    span.clone(),
                );

                result = Expression::List(vec![lambda, test_expr], span.clone());
            }

            Ok(result)
        }

        // We should not have any previously transformed expressions
        Expression::Case(_, _, _, _) => bail!("Unexpected Case expression after transform1"),
        Expression::Cond(_, _, _) => bail!("Unexpected Cond expression after transform2"),
        Expression::Do(_, _, _, _) => bail!("Unexpected Do expression after transform3"),
        Expression::NamedLet(_, _, _, _) => {
            bail!("Unexpected NamedLet expression after transform4")
        }
        Expression::LetRec(_, _, _) => bail!("Unexpected LetRec expression after transform5"),
        Expression::LetStar(_, _, _) => bail!("Unexpected LetStar expression after transform6"),
        Expression::Let(_, _, _) => bail!("Unexpected Let expression after transform7"),
        Expression::Delay(_, _) => bail!("Unexpected Delay expression after transform8"),

        // Recursively process other expression types
        Expression::Atom(_, _) => Ok(expr.clone()),
        Expression::SymbolLiteral(_, _) => Ok(expr.clone()),

        Expression::List(elements, span) => {
            let new_elements = elements
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;
            Ok(Expression::List(new_elements, span.clone()))
        }

        Expression::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;
            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression::Quote(inner, span) => {
            let new_inner = transform_expression(inner)?;
            Ok(Expression::Quote(Box::new(new_inner), span.clone()))
        }

        Expression::Quasiquote(inner, span) => {
            let new_inner = transform_expression(inner)?;
            Ok(Expression::Quasiquote(Box::new(new_inner), span.clone()))
        }

        Expression::Unquote(inner, span) => {
            let new_inner = transform_expression(inner)?;
            Ok(Expression::Unquote(Box::new(new_inner), span.clone()))
        }

        Expression::UnquoteSplicing(inner, span) => {
            let new_inner = transform_expression(inner)?;
            Ok(Expression::UnquoteSplicing(
                Box::new(new_inner),
                span.clone(),
            ))
        }

        Expression::Lambda(params, body, span) => {
            let new_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;
            Ok(Expression::Lambda(params.clone(), new_body, span.clone()))
        }

        Expression::SetBang(var, val, span) => {
            let new_val = transform_expression(val)?;
            Ok(Expression::SetBang(
                var.clone(),
                Box::new(new_val),
                span.clone(),
            ))
        }

        Expression::Begin(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;
            Ok(Expression::Begin(new_exprs, span.clone()))
        }

        Expression::If(cond, then_expr, else_expr, span) => {
            let new_cond = transform_expression(cond)?;
            let new_then = transform_expression(then_expr)?;
            let new_else = match else_expr {
                Some(e) => Some(Box::new(transform_expression(e)?)),
                None => None,
            };
            Ok(Expression::If(
                Box::new(new_cond),
                Box::new(new_then),
                new_else,
                span.clone(),
            ))
        }
    }
}
