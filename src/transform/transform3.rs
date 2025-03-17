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
        // Transform do loops
        Expression::Do(bindings, test_result, body, span) => {
            // Create unique loop name
            let loop_name = "do-loop".to_string();
            let loop_name_span = span.clone(); // Using original span for now

            // Extract variables, initializations, and steps
            let mut vars = Vec::new();
            let mut inits = Vec::new();
            let mut steps = Vec::new();

            for ((var_name, var_span), init, step) in bindings {
                vars.push((var_name.clone(), var_span.clone()));
                inits.push(transform_expression(init)?);

                if let Some(step_expr) = step {
                    steps.push(transform_expression(step_expr)?);
                } else {
                    // If no step, use the variable itself
                    steps.push(Expression::SymbolLiteral(
                        var_name.clone(),
                        var_span.clone(),
                    ));
                }
            }

            // Get test and result expressions
            let (test_expr, result_exprs) = test_result;
            let transformed_test = transform_expression(test_expr)?;

            let transformed_results = result_exprs
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            // Create result expression (if no results, use void/#f)
            let result_expr = if transformed_results.is_empty() {
                Expression::Atom(Atom::Boolean(false), span.clone())
            } else if transformed_results.len() == 1 {
                transformed_results[0].clone()
            } else {
                Expression::Begin(transformed_results, span.clone())
            };

            // Transform body expressions
            let transformed_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            // Create recursive call with updated variables
            let recursive_call_args = steps.clone();
            let recursive_call = Expression::List(
                vec![Expression::SymbolLiteral(
                    loop_name.clone(),
                    loop_name_span.clone(),
                )]
                .into_iter()
                .chain(recursive_call_args)
                .collect(),
                span.clone(),
            );

            // Create body with loop recursion
            let loop_body = if transformed_body.is_empty() {
                recursive_call
            } else {
                // Add recursive call after body expressions
                Expression::Begin(
                    transformed_body
                        .into_iter()
                        .chain(vec![recursive_call])
                        .collect(),
                    span.clone(),
                )
            };

            // Create the if expression for the loop lambda
            let if_expr = Expression::If(
                Box::new(transformed_test),
                Box::new(result_expr),
                Some(Box::new(loop_body)),
                span.clone(),
            );

            // Create the loop lambda
            let loop_lambda = Expression::Lambda(vars.clone(), vec![if_expr], span.clone());

            // Create letrec with loop binding
            let letrec_bindings = vec![((loop_name.clone(), loop_name_span.clone()), loop_lambda)];

            // Create initial loop call with initializers
            let initial_call = Expression::List(
                vec![Expression::SymbolLiteral(
                    loop_name.clone(),
                    loop_name_span.clone(),
                )]
                .into_iter()
                .chain(inits)
                .collect(),
                span.clone(),
            );

            // Create the final letrec expression
            Ok(Expression::LetRec(
                letrec_bindings,
                vec![initial_call],
                span.clone(),
            ))
        }

        // We should not have any Case or Cond expressions left after transform1 and transform2
        Expression::Case(_, _, _, _) => bail!("Unexpected Case expression after transform1"),
        Expression::Cond(_, _, _) => bail!("Unexpected Cond expression after transform2"),

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

        Expression::Delay(expr, span) => {
            let new_expr = transform_expression(expr)?;
            Ok(Expression::Delay(Box::new(new_expr), span.clone()))
        }
    }
}
