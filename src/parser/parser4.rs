use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;

use crate::ast::ast1::Atom;
use crate::ast::ast3::{
    Definition as Definition3, Expression as Expression3,
    TopLevelForm as TopLevelForm3,
};
use crate::ast::ast4::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program3: &crate::ast::ast3::Program,
) -> Result<Program> {
    let forms = program3
        .forms
        .iter()
        .map(|form| convert_top_level_form(form))
        .collect::<Result<Vec<_>>>()?;

    Ok(Program {
        forms,
        source: input.to_string(),
        tokens: tokens
            .iter()
            .map(|(t, s, sp)| (t.clone(), s.to_string(), sp.clone()))
            .collect(),
    })
}

fn convert_top_level_form(form: &TopLevelForm3) -> Result<TopLevelForm> {
    match form {
        TopLevelForm3::Definition(def) => {
            Ok(TopLevelForm::Definition(standardize_definition(def)?))
        }
        TopLevelForm3::Expression(expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(TopLevelForm::Expression(new_expr))
        }
    }
}

fn standardize_definition(def: &Definition3) -> Result<Definition> {
    match def {
        Definition3::Variable((name, span), expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(Definition::Variable(
                (name.clone(), span.clone()),
                Box::new(new_expr),
            ))
        }
        Definition3::Procedure((name, name_span), args, body) => {
            // Convert procedure form to variable form with lambda
            let lambda_expr = Expression::Lambda(
                args.clone(),
                body.iter()
                    .map(|e| convert_expression(e))
                    .collect::<Result<Vec<_>>>()?,
                args.first()
                    .map(|(_, span)| span.clone())
                    .unwrap_or(name_span.clone()),
            );

            Ok(Definition::Variable(
                (name.clone(), name_span.clone()),
                Box::new(lambda_expr),
            ))
        }
    }
}

fn convert_expression(expr: &Expression3) -> Result<Expression> {
    match expr {
        Expression3::Atom(atom, span) => {
            Ok(Expression::Atom(atom.clone(), span.clone()))
        }

        Expression3::List(elements, span) => {
            // Check if this is a lambda form
            if elements.len() >= 3 {
                if let Expression3::Atom(Atom::Identifier(keyword), _) =
                    &elements[0]
                {
                    if keyword == "lambda" {
                        return parse_lambda(&elements[1..], span);
                    }
                }
            }

            // Regular list
            let new_elements = elements
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::List(new_elements, span.clone()))
        }

        Expression3::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression3::Quote(inner, span) => Ok(Expression::Quote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression3::Quasiquote(inner, span) => Ok(Expression::Quasiquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression3::Unquote(inner, span) => Ok(Expression::Unquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression3::UnquoteSplicing(inner, span) => {
            Ok(Expression::UnquoteSplicing(
                Box::new(convert_expression(inner)?),
                span.clone(),
            ))
        }

        Expression3::SymbolLiteral(name, span) => {
            Ok(Expression::SymbolLiteral(name.clone(), span.clone()))
        }
    }
}

fn parse_lambda(
    elements: &[Expression3],
    full_span: &std::ops::Range<usize>,
) -> Result<Expression> {
    if elements.len() < 2 {
        bail!("Lambda requires parameters and body");
    }

    // Parse parameter list
    let params = match &elements[0] {
        Expression3::List(param_exprs, param_span) => param_exprs
            .iter()
            .map(|p| {
                if let Expression3::Atom(Atom::Identifier(name), span) = p {
                    Ok((name.clone(), span.clone()))
                } else {
                    bail!("Lambda parameters must be identifiers")
                }
            })
            .collect::<Result<Vec<_>>>()?,
        _ => bail!("Lambda requires a parameter list"),
    };

    // Convert body expressions
    let body = elements[1..]
        .iter()
        .map(|e| convert_expression(e))
        .collect::<Result<Vec<_>>>()?;

    Ok(Expression::Lambda(params, body, full_span.clone()))
}
