use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;

use crate::ast::ast1::Atom;
use crate::ast::ast7::{
    Definition as Definition7, Expression as Expression7,
    TopLevelForm as TopLevelForm7,
};
use crate::ast::ast8::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

fn convert_top_level_form(form: &TopLevelForm7) -> Result<TopLevelForm> {
    match form {
        TopLevelForm7::Definition(def) => {
            Ok(TopLevelForm::Definition(convert_definition(def)?))
        }
        TopLevelForm7::Expression(expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(TopLevelForm::Expression(new_expr))
        }
    }
}

fn convert_definition(def: &Definition7) -> Result<Definition> {
    match def {
        Definition7::Variable((name, span), expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(Definition::Variable(
                (name.clone(), span.clone()),
                Box::new(new_expr),
            ))
        }
    }
}

fn convert_expression(expr: &Expression7) -> Result<Expression> {
    match expr {
        Expression7::Atom(atom, span) => {
            Ok(Expression::Atom(atom.clone(), span.clone()))
        }

        Expression7::List(elements, span) => {
            // Check if this is a let form
            if !elements.is_empty() {
                if let Expression7::Atom(Atom::Identifier(keyword), _) =
                    &elements[0]
                {
                    if keyword == "let" && elements.len() >= 3 {
                        // We expect the second element to be a list of bindings
                        if let Expression7::List(bindings_exprs, _) =
                            &elements[1]
                        {
                            // Parse bindings
                            let mut bindings = Vec::new();
                            for binding_expr in bindings_exprs {
                                match binding_expr {
                                    Expression7::List(pair, _) if pair.len() == 2 => {
                                        if let Expression7::Atom(Atom::Identifier(var_name), var_span) = &pair[0] {
                                            let init_expr = convert_expression(&pair[1])?;
                                            bindings.push(((var_name.clone(), var_span.clone()), init_expr));
                                        } else {
                                            bail!("First element of binding must be a variable name");
                                        }
                                    },
                                    _ => bail!("Each binding must be a list of form (var init-expr)"),
                                }
                            }

                            // Parse body
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
                            // TODO: this would be handled by the named let pass
                            let new_elements = elements
                                .iter()
                                .map(|e| convert_expression(e))
                                .collect::<Result<Vec<_>>>()?;

                            return Ok(Expression::List(
                                new_elements,
                                span.clone(),
                            ));
                        }
                    }
                    // Handle other special forms
                    else if keyword == "if" {
                        if elements.len() < 3 || elements.len() > 4 {
                            bail!("if expression must have 2 or 3 parts");
                        }

                        let condition = convert_expression(&elements[1])?;
                        let then_expr = convert_expression(&elements[2])?;
                        let else_expr = if elements.len() == 4 {
                            Some(Box::new(convert_expression(&elements[3])?))
                        } else {
                            None
                        };

                        return Ok(Expression::If(
                            Box::new(condition),
                            Box::new(then_expr),
                            else_expr,
                            span.clone(),
                        ));
                    } else if keyword == "begin" {
                        let exprs = elements[1..]
                            .iter()
                            .map(|e| convert_expression(e))
                            .collect::<Result<Vec<_>>>()?;

                        return Ok(Expression::Begin(exprs, span.clone()));
                    } else if keyword == "set!" && elements.len() == 3 {
                        if let Expression7::Atom(
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

        Expression7::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression7::Quote(inner, span) => Ok(Expression::Quote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression7::Quasiquote(inner, span) => Ok(Expression::Quasiquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression7::Unquote(inner, span) => Ok(Expression::Unquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression7::UnquoteSplicing(inner, span) => {
            Ok(Expression::UnquoteSplicing(
                Box::new(convert_expression(inner)?),
                span.clone(),
            ))
        }

        Expression7::SymbolLiteral(name, span) => {
            Ok(Expression::SymbolLiteral(name.clone(), span.clone()))
        }

        Expression7::Lambda(params, body, span) => {
            let new_body = body
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Lambda(params.clone(), new_body, span.clone()))
        }

        Expression7::SetBang(var, val, span) => {
            let new_val = convert_expression(val)?;
            Ok(Expression::SetBang(
                var.clone(),
                Box::new(new_val),
                span.clone(),
            ))
        }

        Expression7::Begin(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Begin(new_exprs, span.clone()))
        }

        Expression7::If(cond, then_expr, else_expr, span) => {
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
    }
}

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program7: &crate::ast::ast7::Program,
) -> Result<Program> {
    let forms = program7
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
