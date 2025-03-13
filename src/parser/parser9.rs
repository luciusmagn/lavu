use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;

use std::ops::Range;

use crate::ast::ast1::Atom;
use crate::ast::ast8::{
    Definition as Definition8, Expression as Expression8,
    TopLevelForm as TopLevelForm8,
};
use crate::ast::ast9::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

fn convert_top_level_form(form: &TopLevelForm8) -> Result<TopLevelForm> {
    match form {
        TopLevelForm8::Definition(def) => {
            Ok(TopLevelForm::Definition(convert_definition(def)?))
        }
        TopLevelForm8::Expression(expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(TopLevelForm::Expression(new_expr))
        }
    }
}

fn convert_definition(def: &Definition8) -> Result<Definition> {
    match def {
        Definition8::Variable((name, span), expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(Definition::Variable(
                (name.clone(), span.clone()),
                Box::new(new_expr),
            ))
        }
    }
}

fn parse_bindings(
    bindings_exprs: &[Expression8],
) -> Result<Vec<((String, Range<usize>), Expression)>> {
    let mut bindings = Vec::new();
    for binding_expr in bindings_exprs {
        match binding_expr {
            Expression8::List(pair, _) if pair.len() == 2 => {
                if let Expression8::Atom(Atom::Identifier(var_name), var_span) =
                    &pair[0]
                {
                    let init_expr = convert_expression(&pair[1])?;
                    bindings.push((
                        (var_name.clone(), var_span.clone()),
                        init_expr,
                    ));
                } else {
                    bail!("First element of binding must be a variable name");
                }
            }
            _ => bail!("Each binding must be a list of form (var init-expr)"),
        }
    }
    Ok(bindings)
}

fn convert_expression(expr: &Expression8) -> Result<Expression> {
    match expr {
        Expression8::Atom(atom, span) => {
            Ok(Expression::Atom(atom.clone(), span.clone()))
        }

        Expression8::List(elements, span) => {
            // Check for special forms
            if !elements.is_empty() {
                if let Expression8::Atom(Atom::Identifier(keyword), _) =
                    &elements[0]
                {
                    match keyword.as_str() {
                        "let*" if elements.len() >= 3 => {
                            // Parse let* form
                            if let Expression8::List(bindings_exprs, _) =
                                &elements[1]
                            {
                                let bindings = parse_bindings(bindings_exprs)?;

                                let body = elements[2..]
                                    .iter()
                                    .map(|e| convert_expression(e))
                                    .collect::<Result<Vec<_>>>()?;

                                return Ok(Expression::LetStar(
                                    bindings,
                                    body,
                                    span.clone(),
                                ));
                            } else {
                                bail!("Second element of let* must be a list of bindings");
                            }
                        }
                        "let" if elements.len() >= 3 => {
                            // Parse let form
                            if let Expression8::List(bindings_exprs, _) =
                                &elements[1]
                            {
                                let bindings = parse_bindings(bindings_exprs)?;

                                let body = elements[2..]
                                    .iter()
                                    .map(|e| convert_expression(e))
                                    .collect::<Result<Vec<_>>>()?;

                                return Ok(Expression::Let(
                                    bindings,
                                    body,
                                    span.clone(),
                                ));
                            } else {
                                bail!("Second element of let must be a list of bindings");
                            }
                        }
                        "if" => {
                            if elements.len() < 3 || elements.len() > 4 {
                                bail!("if expression must have 2 or 3 parts");
                            }

                            let condition = convert_expression(&elements[1])?;
                            let then_expr = convert_expression(&elements[2])?;
                            let else_expr = if elements.len() == 4 {
                                Some(Box::new(convert_expression(
                                    &elements[3],
                                )?))
                            } else {
                                None
                            };

                            return Ok(Expression::If(
                                Box::new(condition),
                                Box::new(then_expr),
                                else_expr,
                                span.clone(),
                            ));
                        }
                        "begin" => {
                            let exprs = elements[1..]
                                .iter()
                                .map(|e| convert_expression(e))
                                .collect::<Result<Vec<_>>>()?;

                            return Ok(Expression::Begin(exprs, span.clone()));
                        }
                        "set!" if elements.len() == 3 => {
                            if let Expression8::Atom(
                                Atom::Identifier(var_name),
                                var_span,
                            ) = &elements[1]
                            {
                                let value_expr =
                                    convert_expression(&elements[2])?;
                                return Ok(Expression::SetBang(
                                    (var_name.clone(), var_span.clone()),
                                    Box::new(value_expr),
                                    span.clone(),
                                ));
                            } else {
                                bail!("set! requires variable name as first argument");
                            }
                        }
                        _ => {} // Not a special form we handle
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

        Expression8::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression8::Quote(inner, span) => Ok(Expression::Quote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression8::Quasiquote(inner, span) => Ok(Expression::Quasiquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression8::Unquote(inner, span) => Ok(Expression::Unquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression8::UnquoteSplicing(inner, span) => {
            Ok(Expression::UnquoteSplicing(
                Box::new(convert_expression(inner)?),
                span.clone(),
            ))
        }

        Expression8::SymbolLiteral(name, span) => {
            Ok(Expression::SymbolLiteral(name.clone(), span.clone()))
        }

        Expression8::Lambda(params, body, span) => {
            let new_body = body
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Lambda(params.clone(), new_body, span.clone()))
        }

        Expression8::SetBang(var, val, span) => {
            let new_val = convert_expression(val)?;
            Ok(Expression::SetBang(
                var.clone(),
                Box::new(new_val),
                span.clone(),
            ))
        }

        Expression8::Begin(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Begin(new_exprs, span.clone()))
        }

        Expression8::If(cond, then_expr, else_expr, span) => {
            let new_cond = convert_expression(cond)?;
            let new_then = convert_expression(then_expr)?;
            let new_else = match else_expr {
                Some(e) => Some(Box::new(convert_expression(e)?)),
                None => None,
            };

            Ok(Expression::If(
                Box::new(new_cond),
                Box::new(new_then),
                new_else,
                span.clone(),
            ))
        }

        Expression8::Let(bindings, body, span) => {
            let new_bindings = bindings
                .iter()
                .map(|((name, name_span), expr)| {
                    let new_expr = convert_expression(expr)?;
                    Ok(((name.clone(), name_span.clone()), new_expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_body = body
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Let(new_bindings, new_body, span.clone()))
        }
    }
}

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program8: &crate::ast::ast8::Program,
) -> Result<Program> {
    let forms = program8
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
