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
        // Transform named let
        Expression::NamedLet((name, name_span), bindings, body, span) => {
            // Extract variable names and their initializations
            let mut vars = Vec::new();
            let mut values = Vec::new();

            for ((var_name, var_span), init_expr) in bindings {
                vars.push((var_name.clone(), var_span.clone()));
                values.push(transform_expression(init_expr)?);
            }

            // Transform the body
            let transformed_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            // Create the function lambda
            let func_lambda = Expression::Lambda(vars.clone(), transformed_body, span.clone());

            // Create letrec binding
            let letrec_bindings = vec![((name.clone(), name_span.clone()), func_lambda)];

            // Create initial function call with values
            let initial_call = Expression::List(
                vec![Expression::Atom(
                    crate::ast::ast1::Atom::Identifier(name.clone()),
                    name_span.clone(),
                )]
                .into_iter()
                .chain(values)
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

        // We should not have any Case, Cond, or Do expressions left
        Expression::Case(_, _, _, _) => bail!("Unexpected Case expression after transform1"),
        Expression::Cond(_, _, _) => bail!("Unexpected Cond expression after transform2"),
        Expression::Do(_, _, _, _) => bail!("Unexpected Do expression after transform3"),

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
