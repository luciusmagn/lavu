use color_eyre::eyre::{bail, Result};
use logos::Span as LogosSpan;

use std::ops::Range;

use crate::ast::ast1::Atom;
use crate::ast::ast14::{
    Definition as Definition14, Expression as Expression14, TopLevelForm as TopLevelForm14,
};
use crate::ast::ast15::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

fn convert_top_level_form(form: &TopLevelForm14) -> Result<TopLevelForm> {
    match form {
        TopLevelForm14::Definition(def) => Ok(TopLevelForm::Definition(convert_definition(def)?)),
        TopLevelForm14::Expression(expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(TopLevelForm::Expression(new_expr))
        }
    }
}

fn convert_definition(def: &Definition14) -> Result<Definition> {
    match def {
        Definition14::Variable((name, span), expr) => {
            let new_expr = convert_expression(expr)?;
            Ok(Definition::Variable(
                (name.clone(), span.clone()),
                Box::new(new_expr),
            ))
        }
    }
}

fn parse_bindings(
    bindings_exprs: &[Expression14],
) -> Result<Vec<((String, Range<usize>), Expression)>> {
    let mut bindings = Vec::new();
    for binding_expr in bindings_exprs {
        match binding_expr {
            Expression14::List(pair, _) if pair.len() == 2 => {
                if let Expression14::Atom(Atom::Identifier(var_name), var_span) = &pair[0] {
                    let init_expr = convert_expression(&pair[1])?;
                    bindings.push(((var_name.clone(), var_span.clone()), init_expr));
                } else {
                    bail!("First element of binding must be a variable name");
                }
            }
            _ => bail!("Each binding must be a list of form (var init-expr)"),
        }
    }
    Ok(bindings)
}

fn parse_do_bindings(
    bindings_exprs: &[Expression14],
) -> Result<Vec<((String, Range<usize>), Expression, Option<Expression>)>> {
    let mut bindings = Vec::new();
    for binding_expr in bindings_exprs {
        match binding_expr {
            Expression14::List(triple, _) => {
                if triple.len() < 2 || triple.len() > 3 {
                    bail!("Do binding must have form (var init [step])");
                }

                if let Expression14::Atom(Atom::Identifier(var_name), var_span) = &triple[0] {
                    let init_expr = convert_expression(&triple[1])?;
                    let step_expr = if triple.len() == 3 {
                        Some(convert_expression(&triple[2])?)
                    } else {
                        None
                    };

                    bindings.push(((var_name.clone(), var_span.clone()), init_expr, step_expr));
                } else {
                    bail!("First element of do binding must be a variable name");
                }
            }
            _ => bail!("Each do binding must be a list of form (var init [step])"),
        }
    }
    Ok(bindings)
}

fn convert_expression(expr: &Expression14) -> Result<Expression> {
    match expr {
        Expression14::Atom(atom, span) => Ok(Expression::Atom(atom.clone(), span.clone())),

        Expression14::List(elements, span) => {
            // Check for special forms
            if !elements.is_empty() {
                if let Expression14::Atom(Atom::Identifier(keyword), _) = &elements[0] {
                    match keyword.as_str() {
                        "do" if elements.len() >= 3 => {
                            // Parse do form: (do ((var init step)...) (test result...) command...)
                            if let Expression14::List(bindings_exprs, _) = &elements[1] {
                                let bindings = parse_do_bindings(bindings_exprs)?;

                                // Parse test clause
                                if let Expression14::List(test_exprs, _) = &elements[2] {
                                    if test_exprs.is_empty() {
                                        bail!("Do test clause cannot be empty");
                                    }

                                    let test_expr = convert_expression(&test_exprs[0])?;
                                    let result_exprs = test_exprs[1..]
                                        .iter()
                                        .map(|e| convert_expression(e))
                                        .collect::<Result<Vec<_>>>()?;

                                    // Parse command body
                                    let commands = elements[3..]
                                        .iter()
                                        .map(|e| convert_expression(e))
                                        .collect::<Result<Vec<_>>>()?;

                                    return Ok(Expression::Do(
                                        bindings,
                                        (Box::new(test_expr), result_exprs),
                                        commands,
                                        span.clone(),
                                    ));
                                } else {
                                    bail!("Second element of do must be a list containing test and result expressions");
                                }
                            } else {
                                bail!("First element of do must be a list of bindings");
                            }
                        }
                        "and" => {
                            // (and expr1 expr2 ...)
                            let exprs = elements[1..]
                                .iter()
                                .map(|e| convert_expression(e))
                                .collect::<Result<Vec<_>>>()?;

                            return Ok(Expression::And(exprs, span.clone()));
                        }
                        "or" => {
                            // (or expr1 expr2 ...)
                            let exprs = elements[1..]
                                .iter()
                                .map(|e| convert_expression(e))
                                .collect::<Result<Vec<_>>>()?;

                            return Ok(Expression::Or(exprs, span.clone()));
                        }
                        "let" if elements.len() >= 3 => {
                            // Check for named let form vs regular let
                            match &elements[1] {
                                // Named let: (let name ((var val) ...) body ...)
                                Expression14::Atom(Atom::Identifier(name), name_span) => {
                                    if elements.len() < 4 {
                                        bail!("Named let requires bindings and body");
                                    }

                                    // The third element should be a list of bindings
                                    if let Expression14::List(bindings_exprs, _) = &elements[2] {
                                        let bindings = parse_bindings(bindings_exprs)?;

                                        // Parse body
                                        let body = elements[3..]
                                            .iter()
                                            .map(|e| convert_expression(e))
                                            .collect::<Result<Vec<_>>>()?;

                                        return Ok(Expression::NamedLet(
                                            (name.clone(), name_span.clone()),
                                            bindings,
                                            body,
                                            span.clone()
                                        ));
                                    } else {
                                        bail!("Third element of named let must be a list of bindings");
                                    }
                                },

                                // Regular let: (let ((var val) ...) body ...)
                                Expression14::List(bindings_exprs, _) => {
                                    let bindings = parse_bindings(bindings_exprs)?;

                                    let body = elements[2..]
                                        .iter()
                                        .map(|e| convert_expression(e))
                                        .collect::<Result<Vec<_>>>()?;

                                    return Ok(Expression::Let(bindings, body, span.clone()));
                                },

                                _ => bail!("Second element of let must be either an identifier (for named let) or a list of bindings"),
                            }
                        }
                        "cond" if elements.len() >= 2 => {
                            // Parse cond form: (cond clause...)
                            let mut clauses = Vec::new();
                            let mut else_clause = None;

                            // Process each clause
                            for clause_expr in &elements[1..] {
                                if let Expression14::List(clause_elements, _) = clause_expr {
                                    if clause_elements.is_empty() {
                                        bail!("Cond clause cannot be empty");
                                    }

                                    // Check for else clause
                                    if let Expression14::Atom(Atom::Identifier(clause_key), _) =
                                        &clause_elements[0]
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
                                                    .map(|e| convert_expression(e))
                                                    .collect::<Result<Vec<_>>>()?,
                                            );

                                            continue; // Skip to next clause
                                        }
                                    }

                                    // Regular clause
                                    let test_expr = convert_expression(&clause_elements[0])?;
                                    let result_exprs = clause_elements[1..]
                                        .iter()
                                        .map(|e| convert_expression(e))
                                        .collect::<Result<Vec<_>>>()?;

                                    clauses.push((test_expr, result_exprs));
                                } else {
                                    bail!("Each cond clause must be a list");
                                }
                            }

                            return Ok(Expression::Cond(clauses, else_clause, span.clone()));
                        }
                        "case" if elements.len() >= 3 => {
                            // Parse case form
                            let key_expr = convert_expression(&elements[1])?;
                            let mut clauses = Vec::new();
                            let mut else_clause = None;

                            // Process each clause
                            for clause_expr in &elements[2..] {
                                if let Expression14::List(clause_elements, _) = clause_expr {
                                    if clause_elements.is_empty() {
                                        bail!("Case clause cannot be empty");
                                    }

                                    // Check for else clause
                                    if let Expression14::Atom(Atom::Identifier(clause_key), _) =
                                        &clause_elements[0]
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
                                                    .map(|e| convert_expression(e))
                                                    .collect::<Result<Vec<_>>>()?,
                                            );

                                            continue; // Skip to next clause
                                        }
                                    }

                                    // Regular clause with datums
                                    if let Expression14::List(datums, _) = &clause_elements[0] {
                                        let datum_exprs = datums
                                            .iter()
                                            .map(|e| convert_expression(e))
                                            .collect::<Result<Vec<_>>>()?;

                                        let result_exprs = clause_elements[1..]
                                            .iter()
                                            .map(|e| convert_expression(e))
                                            .collect::<Result<Vec<_>>>()?;

                                        clauses.push((datum_exprs, result_exprs));
                                    } else {
                                        bail!("Case clause datums must be a list");
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
                            if let Expression14::List(bindings_exprs, _) = &elements[1] {
                                let bindings = parse_bindings(bindings_exprs)?;

                                let body = elements[2..]
                                    .iter()
                                    .map(|e| convert_expression(e))
                                    .collect::<Result<Vec<_>>>()?;

                                return Ok(Expression::LetRec(bindings, body, span.clone()));
                            } else {
                                bail!("Second element of letrec must be a list of bindings");
                            }
                        }
                        "let*" if elements.len() >= 3 => {
                            // Parse let* form
                            if let Expression14::List(bindings_exprs, _) = &elements[1] {
                                let bindings = parse_bindings(bindings_exprs)?;

                                let body = elements[2..]
                                    .iter()
                                    .map(|e| convert_expression(e))
                                    .collect::<Result<Vec<_>>>()?;

                                return Ok(Expression::LetStar(bindings, body, span.clone()));
                            } else {
                                bail!("Second element of let* must be a list of bindings");
                            }
                        }
                        "if" => {
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
                        }
                        "begin" => {
                            let exprs = elements[1..]
                                .iter()
                                .map(|e| convert_expression(e))
                                .collect::<Result<Vec<_>>>()?;

                            return Ok(Expression::Begin(exprs, span.clone()));
                        }
                        "set!" if elements.len() == 3 => {
                            if let Expression14::Atom(Atom::Identifier(var_name), var_span) =
                                &elements[1]
                            {
                                let value_expr = convert_expression(&elements[2])?;
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

        Expression14::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression14::Quote(inner, span) => Ok(Expression::Quote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression14::Quasiquote(inner, span) => Ok(Expression::Quasiquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression14::Unquote(inner, span) => Ok(Expression::Unquote(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression14::UnquoteSplicing(inner, span) => Ok(Expression::UnquoteSplicing(
            Box::new(convert_expression(inner)?),
            span.clone(),
        )),

        Expression14::SymbolLiteral(name, span) => {
            Ok(Expression::SymbolLiteral(name.clone(), span.clone()))
        }

        Expression14::Lambda(params, body, span) => {
            let new_body = body
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Lambda(params.clone(), new_body, span.clone()))
        }

        Expression14::SetBang(var, val, span) => {
            let new_val = convert_expression(val)?;
            Ok(Expression::SetBang(
                var.clone(),
                Box::new(new_val),
                span.clone(),
            ))
        }

        Expression14::Begin(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Begin(new_exprs, span.clone()))
        }

        Expression14::If(cond, then_expr, else_expr, span) => {
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

        Expression14::Let(bindings, body, span) => {
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

        Expression14::LetStar(bindings, body, span) => {
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

        Expression14::LetRec(bindings, body, span) => {
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

        Expression14::Case(key, clauses, else_clause, span) => {
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

        Expression14::Cond(clauses, else_clause, span) => {
            let new_clauses = clauses
                .iter()
                .map(|(test, results)| {
                    let new_test = convert_expression(test)?;

                    let new_results = results
                        .iter()
                        .map(|e| convert_expression(e))
                        .collect::<Result<Vec<_>>>()?;

                    Ok((new_test, new_results))
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

            Ok(Expression::Cond(new_clauses, new_else, span.clone()))
        }

        Expression14::NamedLet(name, bindings, body, span) => {
            let new_bindings = bindings
                .iter()
                .map(|((var_name, var_span), expr)| {
                    let new_expr = convert_expression(expr)?;
                    Ok(((var_name.clone(), var_span.clone()), new_expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let new_body = body
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::NamedLet(
                name.clone(),
                new_bindings,
                new_body,
                span.clone(),
            ))
        }

        Expression14::And(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::And(new_exprs, span.clone()))
        }

        Expression14::Or(exprs, span) => {
            let new_exprs = exprs
                .iter()
                .map(|e| convert_expression(e))
                .collect::<Result<Vec<_>>>()?;

            Ok(Expression::Or(new_exprs, span.clone()))
        }
    }
}

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program14: &crate::ast::ast14::Program,
) -> Result<Program> {
    let forms = program14
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
