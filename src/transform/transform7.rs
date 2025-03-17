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
        // Transform let
        Expression::Let(bindings, body, span) => {
            // If there are no bindings, it's just a begin
            if bindings.is_empty() {
                let transformed_body = body
                    .iter()
                    .map(transform_expression)
                    .collect::<Result<Vec<_>>>()?;

                return Ok(Expression::Begin(transformed_body, span.clone()));
            }

            // Extract variable names and initialization expressions
            let mut var_names = Vec::new();
            let mut init_exprs = Vec::new();

            for ((name, name_span), init_expr) in bindings {
                var_names.push((name.clone(), name_span.clone()));
                init_exprs.push(transform_expression(init_expr)?);
            }

            // Transform the body expressions
            let transformed_body = body
                .iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;

            // Create the lambda expression
            let lambda = Expression::Lambda(var_names, transformed_body, span.clone());

            // Create the application by wrapping the lambda and arguments in a list
            let mut app_elements = Vec::with_capacity(1 + init_exprs.len());
            app_elements.push(lambda);
            app_elements.extend(init_exprs);

            Ok(Expression::List(app_elements, span.clone()))
        }

        // We should not have any Case, Cond, Do, NamedLet, LetRec, or LetStar expressions left
        Expression::Case(_, _, _, _) => bail!("Unexpected Case expression after transform1"),
        Expression::Cond(_, _, _) => bail!("Unexpected Cond expression after transform2"),
        Expression::Do(_, _, _, _) => bail!("Unexpected Do expression after transform3"),
        Expression::NamedLet(_, _, _, _) => {
            bail!("Unexpected NamedLet expression after transform4")
        }
        Expression::LetRec(_, _, _) => bail!("Unexpected LetRec expression after transform5"),
        Expression::LetStar(_, _, _) => bail!("Unexpected LetStar expression after transform6"),

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
