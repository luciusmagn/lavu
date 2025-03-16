use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;
use std::ops::Range;

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
        // Transform case expressions
        Expression::Case(key, clauses, else_clause, span) => {
            // Create cond clauses
            let mut cond_clauses = Vec::new();

            for (datums, results) in clauses {
                // For each case clause, create a condition testing all datums
                if datums.is_empty() {
                    continue; // Skip empty datum lists (shouldn't happen in valid code)
                }

                // Transform the results
                let transformed_results: Vec<Expression> = results
                    .iter()
                    .map(transform_expression)
                    .collect::<Result<Vec<_>>>()?;

                // Generate equality tests for each datum
                if datums.len() == 1 {
                    // Simple case: just one datum
                    let datum = transform_expression(&datums[0])?;

                    // Create an eqv? test: (eqv? key datum)
                    let test = create_eqv_test(key, &datum, span.clone());
                    cond_clauses.push((test, transformed_results));
                } else {
                    // Multiple datums - create an OR expression
                    let mut or_tests = Vec::new();

                    for datum in datums {
                        let transformed_datum = transform_expression(datum)?;
                        let test = create_eqv_test(key, &transformed_datum, span.clone());
                        or_tests.push(test);
                    }

                    // Create (or (eqv? key datum1) (eqv? key datum2) ...)
                    let or_expr = Expression::Or(or_tests, span.clone());
                    cond_clauses.push((or_expr, transformed_results));
                }
            }

            // Transform the else clause
            let transformed_else = else_clause
                .as_ref()
                .map(|exprs| {
                    exprs
                        .iter()
                        .map(transform_expression)
                        .collect::<Result<Vec<_>>>()
                })
                .transpose()?;

            // Create the final cond expression
            Ok(Expression::Cond(
                cond_clauses,
                transformed_else,
                span.clone(),
            ))
        }

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

        Expression::Let(bindings, body, span) => {
            let new_bindings = bindings
                .iter()
                .map(|((name, name_span), expr)| {
                    let new_expr = transform_expression(expr)?;
                    Ok(((name.clone(), name_span.clone()), new_expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Let(new_bindings, new_body, span.clone()))
        }

        Expression::LetStar(bindings, body, span) => {
            let new_bindings = bindings
                .iter()
                .map(|((name, name_span), expr)| {
                    let new_expr = transform_expression(expr)?;
                    Ok(((name.clone(), name_span.clone()), new_expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::LetStar(new_bindings, new_body, span.clone()))
        }

        Expression::LetRec(bindings, body, span) => {
            let new_bindings = bindings
                .iter()
                .map(|((name, name_span), expr)| {
                    let new_expr = transform_expression(expr)?;
                    Ok(((name.clone(), name_span.clone()), new_expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::LetRec(new_bindings, new_body, span.clone()))
        }

        Expression::Cond(clauses, else_clause, span) => {
            let new_clauses = clauses
                .iter()
                .map(|(test, results)| {
                    let new_test = transform_expression(test)?;
                    let new_results = results
                        .iter()
                        .map(transform_expression)
                        .collect::<Result<Vec<_>>>()?;
                    Ok((new_test, new_results))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_else = else_clause
                .as_ref()
                .map(|exprs| {
                    exprs
                        .iter()
                        .map(transform_expression)
                        .collect::<Result<Vec<_>>>()
                })
                .transpose()?;

            Ok(Expression::Cond(new_clauses, new_else, span.clone()))
        }

        Expression::NamedLet(name, bindings, body, span) => {
            let new_bindings = bindings
                .iter()
                .map(|((var_name, var_span), expr)| {
                    let new_expr = transform_expression(expr)?;
                    Ok(((var_name.clone(), var_span.clone()), new_expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::NamedLet(
                name.clone(),
                new_bindings,
                new_body,
                span.clone(),
            ))
        }

        Expression::And(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::And(new_exprs, span.clone()))
        }

        Expression::Or(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Or(new_exprs, span.clone()))
        }

        Expression::Do(bindings, test_result, body, span) => {
            let new_bindings = bindings
                .iter()
                .map(|((var_name, var_span), init, step)| {
                    let new_init = transform_expression(init)?;
                    let new_step = match step {
                        Some(s) => Some(transform_expression(s)?),
                        None => None,
                    };

                    Ok(((var_name.clone(), var_span.clone()), new_init, new_step))
                })
                .collect::<Result<Vec<_>>>()?;

            let (test, results) = test_result;
            let new_test = transform_expression(test)?;
            let new_results = results
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            let new_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Do(
                new_bindings,
                (Box::new(new_test), new_results),
                new_body,
                span.clone(),
            ))
        }

        Expression::Delay(expr, span) => {
            let new_expr = transform_expression(expr)?;
            Ok(Expression::Delay(Box::new(new_expr), span.clone()))
        }
    }
}

// Helper function to create an eqv? test expression
fn create_eqv_test(key: &Expression, datum: &Expression, span: Range<usize>) -> Expression {
    // Create a list (eqv? key datum)
    Expression::List(
        vec![
            // eqv? identifier
            Expression::SymbolLiteral("eqv?".to_string(), span.clone()),
            // key expression (already transformed)
            key.clone(),
            // datum expression (already transformed)
            datum.clone(),
        ],
        span,
    )
}
