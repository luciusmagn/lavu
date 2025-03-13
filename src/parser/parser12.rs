use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;

use std::ops::Range;

use crate::ast::ast1::Atom;
use crate::ast::ast11::{
    Definition as Definition11, Expression as Expression11,
    TopLevelForm as TopLevelForm11,
};
use crate::ast::ast12::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

fn convert_top_level_form(form: &TopLevelForm11) -> Result<TopLevelForm> {
    match form {
        TopLevelForm11::Definition(def) => {
            Ok(TopLevelForm::Definition(convert_definition(def)?))
        }
        TopLevelForm11::Expression(expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(TopLevelForm::Expression(new_expr))
        }
    }
}

fn convert_definition(def: &Definition11) -> Result<Definition> {
    match def {
        Definition11::Variable((name, span), expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(Definition::Variable(
                (name.clone(), span.clone()),
                Box::new(new_expr),
            ))
        }
    }
}

fn parse_bindings(
    bindings_exprs: &[Expression11],
) -> Result<Vec<((String, Range<usize>), Expression)>> {
    let mut bindings = Vec::new();
    for binding_expr in bindings_exprs {
        match binding_expr {
            Expression11::List(pair, _) if pair.len() == 2 => {
                if let Expression11::Atom(
                    Atom::Identifier(var_name),
                    var_span,
                ) = &pair[0]
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

fn convert_expression(expr: &Expression11) -> Result<Expression> {
    match expr {
        Expression11::Atom(atom, span) => {
            Ok(Expression::Atom(atom.clone(), span.clone()))
        }

        Expression11::List(elements, span) => {
            // Check for special forms
            if !elements.is_empty() {
                if let Expression11::Atom(Atom::Identifier(keyword), _) =
                    &elements[0]
                {
                    match keyword.as_str() {
                        "cond" if elements.len() >= 2 => {
                            // Parse cond form: (cond clause...)
                            let mut clauses = Vec::new();
                            let mut else_clause = None;

                            // Process each clause
                            for clause_expr in &elements[1..] {
                                if let Expression11::List(clause_elements, _) =
                                    clause_expr
                                {
                                    if clause_elements.is_empty() {
                                        bail!("Cond clause cannot be empty");
                                    }

                                    // Check for else clause
                                    if let Expression11::Atom(
                                        Atom::Identifier(clause_key),
                                        _,
                                    ) = &clause_elements[0]
                                    {
                                        if clause_key == "else" {
                                            if else_clause.is_some() {
                                                bail!("Multiple else clauses in cond expression");
                                            }

                                            if clause_elements.len() < 2 {
                                                bail!("Else clause must have at least one result expression");
                                            }

                                            else_clause = Some(
                                                clause_elements[1..]
                                                    .iter()
                                                    .map(|e| {
                                                        convert_expression(e)
                                                    })
                                                    .collect::<Result<Vec<_>>>(
                                                    )?,
                                            );

                                            continue; // Skip to next clause
                                        }
                                    }

                                    // Regular clause
                                    let test_expr = convert_expression(
                                        &clause_elements[0],
                                    )?;
                                    let result_exprs = clause_elements[1..]
                                        .iter()
                                        .map(|e| convert_expression(e))
                                        .collect::<Result<Vec<_>>>()?;

                                    clauses.push((test_expr, result_exprs));
                                } else {
                                    bail!("Each cond clause must be a list");
                                }
                            }

                            return Ok(Expression::Cond(
                                clauses,
                                else_clause,
                                span.clone(),
                            ));
                        }
                        "case" if elements.len() >= 3 => {
                            // Parse case form: (case key clause...)
                            let key_expr = convert_expression(&elements[1])?;
                            let mut clauses = Vec::new();
                            let mut else_clause = None;

                            // Process each clause
                            for clause_expr in &elements[2..] {
                                if let Expression11::List(clause_elements, _) =
                                    clause_expr
                                {
                                    if clause_elements.is_empty() {
                                        bail!("Case clause cannot be empty");
                                    }

                                    // Check for else clause
                                    if let Expression11::Atom(
                                        Atom::Identifier(clause_key),
                                        _,
                                    ) = &clause_elements[0]
                                    {
                                        if clause_key == "else" {
                                            if else_clause.is_some() {
                                                bail!("Multiple else clauses in case expression");
                                            }

                                            if clause_elements.len() < 2 {
                                                bail!("Else clause must have at least one result expression");
                                            }

                                            else_clause = Some(
                                                clause_elements[1..]
                                                    .iter()
                                                    .map(|e| {
                                                        convert_expression(e)
                                                    })
                                                    .collect::<Result<Vec<_>>>(
                                                    )?,
                                            );

                                            continue; // Skip to next clause
                                        }
                                    }

                                    // Regular clause with datums
                                    if let Expression11::List(datums, _) =
                                        &clause_elements[0]
                                    {
                                        let datum_exprs = datums
                                            .iter()
                                            .map(|e| convert_expression(e))
                                            .collect::<Result<Vec<_>>>()?;

                                        let result_exprs = clause_elements[1..]
                                            .iter()
                                            .map(|e| convert_expression(e))
                                            .collect::<Result<Vec<_>>>()?;

                                        clauses
                                            .push((datum_exprs, result_exprs));
                                    } else {
                                        bail!(
                                            "Case clause datums must be a list"
                                        );
                                    }
                                } else {
                                    bail!("Each case clause must be a list");
                                }
                            }

                            return Ok(Expression::Case(
                                Box::new(key_expr),
                                clauses,
                                else_clause,
                                span.clone(),
                            ));
                        }
                        "letrec" if elements.len() >= 3 => {
                            // Parse letrec form
                            if let Expression11::List(bindings_exprs, _) =
                                &elements[1]
                            {
                                let bindings = parse_bindings(bindings_exprs)?;

                                let body = elements[2..]
                                    .iter()
                                    .map(|e| convert_expression(e))
                                    .collect::<Result<Vec<_>>>()?;

                                return Ok(Expression::LetRec(
                                    bindings,
                                    body,
                                    span.clone(),
                                ));
                            } else {
                                bail!("Second element of letrec must be a list of bindings");
                            }
                        }
                        "let*" if elements.len() >= 3 => {
                            // Parse let* form
                            if let Expression11::List(bindings_exprs, _) =
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
                            if let Expression11::List(bindings_exprs, _) =
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
                            if let Expression11::Atom(
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

        Expression11::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression11::Quote(inner, span) => Ok(Expression::Quote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression11::Quasiquote(inner, span) => Ok(Expression::Quasiquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression11::Unquote(inner, span) => Ok(Expression::Unquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression11::UnquoteSplicing(inner, span) => {
            Ok(Expression::UnquoteSplicing(
                Box::new(convert_expression(inner)?),
                span.clone(),
            ))
        }

        Expression11::SymbolLiteral(name, span) => {
            Ok(Expression::SymbolLiteral(name.clone(), span.clone()))
        }

        Expression11::Lambda(params, body, span) => {
            let new_body = body
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Lambda(params.clone(), new_body, span.clone()))
        }

        Expression11::SetBang(var, val, span) => {
            let new_val = convert_expression(val)?;
            Ok(Expression::SetBang(
                var.clone(),
                Box::new(new_val),
                span.clone(),
            ))
        }

        Expression11::Begin(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Begin(new_exprs, span.clone()))
        }

        Expression11::If(cond, then_expr, else_expr, span) => {
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

        Expression11::Let(bindings, body, span) => {
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

        Expression11::LetStar(bindings, body, span) => {
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

            Ok(Expression::LetStar(new_bindings, new_body, span.clone()))
        }

        Expression11::LetRec(bindings, body, span) => {
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

            Ok(Expression::LetRec(new_bindings, new_body, span.clone()))
        }

        Expression11::Case(key, clauses, else_clause, span) => {
            let new_key = convert_expression(key)?;

            let new_clauses = clauses
                .iter()
                .map(|(datums, results)| {
                    let new_datums = datums
                        .iter()
                        .map(|e| convert_expression(e))
                        .collect::<Result<Vec<_>>>()?;

                    let new_results = results
                        .iter()
                        .map(|e| convert_expression(e))
                        .collect::<Result<Vec<_>>>()?;

                    Ok((new_datums, new_results))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_else = match else_clause {
                Some(e) => Some(
                    e.iter()
                        .map(|expr| convert_expression(expr))
                        .collect::<Result<Vec<_>>>()?,
                ),
                None => None,
            };

            Ok(Expression::Case(
                Box::new(new_key),
                new_clauses,
                new_else,
                span.clone(),
            ))
        }
    }
}

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program11: &crate::ast::ast11::Program,
) -> Result<Program> {
    let forms = program11
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
