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
        // Transform cond expressions
        Expression::Cond(clauses, else_clause, span) => {
            // Build the nested ifs from the inside out (so start with the last clause)
            let mut result = match else_clause {
                Some(else_exprs) => {
                    // Wrap multiple expressions in begin
                    if else_exprs.len() == 1 {
                        transform_expression(&else_exprs[0])?
                    } else {
                        let transformed_exprs = else_exprs
                            .iter()
                            .map(transform_expression)
                            .collect::<Result<Vec<_>>>()?;
                        Expression::Begin(transformed_exprs, span.clone())
                    }
                }
                None => {
                    // (cond) with no else gives unspecified value, often #<void>
                    // For simplicity, we'll use #f as the result
                    Expression::Atom(crate::ast::ast1::Atom::Boolean(false), span.clone())
                }
            };

            // Process clauses in reverse order
            for (test, body) in clauses.iter().rev() {
                let transformed_test = transform_expression(test)?;

                // Transform the body
                let transformed_body = if body.len() == 1 {
                    transform_expression(&body[0])?
                } else {
                    let transformed_exprs = body
                        .iter()
                        .map(transform_expression)
                        .collect::<Result<Vec<_>>>()?;
                    Expression::Begin(transformed_exprs, span.clone())
                };

                // Create the if expression
                result = Expression::If(
                    Box::new(transformed_test),
                    Box::new(transformed_body),
                    Some(Box::new(result)),
                    span.clone(),
                );
            }

            Ok(result)
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

        // We should not have any Case expressions left after transform1
        Expression::Case(_, _, _, _) => bail!("Unexpected Case expression after transform1"),

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
