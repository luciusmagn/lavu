use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;

use crate::ast::ast1::Atom;
use crate::ast::ast4::{
    Definition as Definition4, Expression as Expression4,
    TopLevelForm as TopLevelForm4,
};
use crate::ast::ast5::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

fn convert_top_level_form(form: &TopLevelForm4) -> Result<TopLevelForm> {
    match form {
        TopLevelForm4::Definition(def) => {
            Ok(TopLevelForm::Definition(convert_definition(def)?))
        }
        TopLevelForm4::Expression(expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(TopLevelForm::Expression(new_expr))
        }
    }
}

fn convert_definition(def: &Definition4) -> Result<Definition> {
    match def {
        Definition4::Variable((name, span), expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(Definition::Variable(
                (name.clone(), span.clone()),
                Box::new(new_expr),
            ))
        }
    }
}

fn convert_expression(expr: &Expression4) -> Result<Expression> {
    match expr {
        Expression4::Atom(atom, span) => {
            Ok(Expression::Atom(atom.clone(), span.clone()))
        }

        Expression4::List(elements, span) => {
            // Check if this is a set! form
            if elements.len() == 3 {
                if let Expression4::Atom(Atom::Identifier(keyword), _) =
                    &elements[0]
                {
                    if keyword == "set!" {
                        if let Expression4::Atom(
                            Atom::Identifier(var_name),
                            var_span,
                        ) = &elements[1]
                        {
                            let value_expr = convert_expression(&elements[2])?;
                            return Ok(Expression::SetBang(
                                (var_name.clone(), var_span.clone()),
                                Box::new(value_expr),
                                span.clone(),
                            ));
                        } else {
                            bail!(
                                "set! requires variable name as first argument"
                            );
                        }
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

        Expression4::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression4::Quote(inner, span) => Ok(Expression::Quote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression4::Quasiquote(inner, span) => Ok(Expression::Quasiquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression4::Unquote(inner, span) => Ok(Expression::Unquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression4::UnquoteSplicing(inner, span) => {
            Ok(Expression::UnquoteSplicing(
                Box::new(convert_expression(inner)?),
                span.clone(),
            ))
        }

        Expression4::SymbolLiteral(name, span) => {
            Ok(Expression::SymbolLiteral(name.clone(), span.clone()))
        }

        Expression4::Lambda(params, body, span) => {
            let new_body = body
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Lambda(params.clone(), new_body, span.clone()))
        }
    }
}

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program4: &crate::ast::ast4::Program,
) -> Result<Program> {
    let forms = program4
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
