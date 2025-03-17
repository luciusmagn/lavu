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
        // Transform quasiquote expressions
        Expression::Quasiquote(inner, span) => transform_quasiquote(inner, span, 0),

        // We should not have any previously transformed expressions
        Expression::Case(_, _, _, _) => bail!("Unexpected Case expression after transform1"),
        Expression::Cond(_, _, _) => bail!("Unexpected Cond expression after transform2"),
        Expression::Do(_, _, _, _) => bail!("Unexpected Do expression after transform3"),
        Expression::NamedLet(_, _, _, _) => {
            bail!("Unexpected NamedLet expression after transform4")
        }
        Expression::LetRec(_, _, _) => bail!("Unexpected LetRec expression after transform5"),
        Expression::LetStar(_, _, _) => bail!("Unexpected LetStar expression after transform6"),
        Expression::Let(_, _, _) => bail!("Unexpected Let expression after transform7"),
        Expression::Delay(_, _) => bail!("Unexpected Delay expression after transform8"),
        Expression::And(_, _) => bail!("Unexpected And expression after transform9"),
        Expression::Or(_, _) => bail!("Unexpected Or expression after transform9"),

        // Recursively process other expression types
        Expression::Atom(_, _) => Ok(expr.clone()),
        Expression::SymbolLiteral(_, _) => Ok(expr.clone()),

        Expression::List(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| transform_expression(e))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expression::List(new_elements, span.clone()))
        }

        Expression::Vector(elements, span) => {
            let new_elements = elements
                .iter()
                .map(|e| transform_expression(e))
                .collect::<Result<Vec<_>>>()?;
            Ok(Expression::Vector(new_elements, span.clone()))
        }

        Expression::Quote(inner, span) => {
            let new_inner = transform_expression(inner)?;
            Ok(Expression::Quote(Box::new(new_inner), span.clone()))
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
                .map(|e| transform_expression(e))
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
                .map(|e| transform_expression(e))
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
    }
}

// Transform quasiquote expressions
// level tracks the nesting level of quasiquotes
fn transform_quasiquote(
    expr: &Expression,
    span: &Range<usize>,
    level: usize,
) -> Result<Expression> {
    match expr {
        // Basic case: simple (non-nested) atoms get quoted
        Expression::Atom(atom, atom_span) => Ok(Expression::Quote(
            Box::new(Expression::Atom(atom.clone(), atom_span.clone())),
            span.clone(),
        )),

        // Handle symbol literals
        Expression::SymbolLiteral(sym, sym_span) => {
            // Symbol literals remain as symbol literals
            Ok(Expression::SymbolLiteral(sym.clone(), sym_span.clone()))
        }

        // Handle unquote
        Expression::Unquote(inner, inner_span) => {
            if level == 0 {
                // At level 0, unquote means evaluate the inner expression
                transform_expression(inner)
            } else {
                // At deeper levels, we're in a nested quasiquote, so keep the unquote
                // but transform the inner expression
                let transformed_inner = transform_quasiquote(inner, inner_span, level - 1)?;
                Ok(Expression::Unquote(
                    Box::new(transformed_inner),
                    inner_span.clone(),
                ))
            }
        }

        // Handle unquote-splicing
        Expression::UnquoteSplicing(inner, inner_span) => {
            if level == 0 {
                // Error - unquote-splicing can only appear inside a list
                bail!("unquote-splicing outside of a list")
            } else {
                // Nested level
                let transformed_inner = transform_quasiquote(inner, inner_span, level - 1)?;
                Ok(Expression::UnquoteSplicing(
                    Box::new(transformed_inner),
                    inner_span.clone(),
                ))
            }
        }

        // Handle nested quasiquote
        Expression::Quasiquote(inner, inner_span) => {
            // Increment the level for the nested quasiquote
            let transformed_inner = transform_quasiquote(inner, inner_span, level + 1)?;
            Ok(Expression::Quasiquote(
                Box::new(transformed_inner),
                inner_span.clone(),
            ))
        }

        // Handle lists - this is the most complex case
        Expression::List(elements, list_span) => {
            if elements.is_empty() {
                // Empty list just gets quoted
                return Ok(Expression::Quote(
                    Box::new(Expression::List(Vec::new(), list_span.clone())),
                    span.clone(),
                ));
            }

            // Process list elements, combining with appropriate operations
            let mut result = None;
            let mut current_elems = Vec::new();

            for (i, element) in elements.iter().enumerate() {
                match element {
                    // Handle unquote-splicing in a list
                    Expression::UnquoteSplicing(inner, _) if level == 0 => {
                        // If we have accumulated regular elements, build a list
                        if !current_elems.is_empty() {
                            let list_expr =
                                create_list_expression(&current_elems, list_span, span)?;
                            current_elems.clear();

                            // Combine with previous result if any
                            if let Some(prev) = result {
                                result = Some(create_append_expression(prev, list_expr, span)?);
                            } else {
                                result = Some(list_expr);
                            }
                        }

                        // Process the unquote-splicing content
                        let spliced_expr = transform_expression(inner)?;

                        // Append to previous result if any
                        if let Some(prev) = result {
                            result = Some(create_append_expression(prev, spliced_expr, span)?);
                        } else {
                            result = Some(spliced_expr);
                        }
                    }

                    // For other elements, add to the current batch
                    _ => {
                        let transformed = transform_quasiquote(element, span, level)?;
                        current_elems.push(transformed);
                    }
                }
            }

            // Process any remaining regular elements
            if !current_elems.is_empty() {
                let list_expr = create_list_expression(&current_elems, list_span, span)?;

                // Combine with previous result if any
                if let Some(prev) = result {
                    result = Some(create_append_expression(prev, list_expr, span)?);
                } else {
                    result = Some(list_expr);
                }
            }

            match result {
                Some(expr) => Ok(expr),
                None => Ok(Expression::Quote(
                    Box::new(Expression::List(Vec::new(), list_span.clone())),
                    span.clone(),
                )),
            }
        }

        // Handle vectors - similar to lists but use vector->list and list->vector
        Expression::Vector(elements, vec_span) => {
            // Transform as if it were a list, then convert back to vector
            let list_expr = Expression::List(elements.clone(), vec_span.clone());
            let transformed_list = transform_quasiquote(&list_expr, span, level)?;

            // Wrap in list->vector conversion
            let list_to_vec =
                Expression::Atom(Atom::Identifier("list->vector".to_string()), span.clone());

            Ok(Expression::List(
                vec![list_to_vec, transformed_list],
                span.clone(),
            ))
        }

        // Handle other expression types by recursively transforming
        _ => transform_expression(expr),
    }
}

// Helper to create a list expression: (list e1 e2 e3 ...)
fn create_list_expression(
    elements: &[Expression],
    element_span: &Range<usize>,
    span: &Range<usize>,
) -> Result<Expression> {
    let mut list_elements = Vec::with_capacity(elements.len() + 1);

    // Add 'list function
    list_elements.push(Expression::Atom(
        Atom::Identifier("list".to_string()),
        span.clone(),
    ));

    // Add all elements
    list_elements.extend(elements.iter().cloned());

    Ok(Expression::List(list_elements, span.clone()))
}

// Helper to create an append expression: (append e1 e2)
fn create_append_expression(
    expr1: Expression,
    expr2: Expression,
    span: &Range<usize>,
) -> Result<Expression> {
    Ok(Expression::List(
        vec![
            Expression::Atom(Atom::Identifier("append".to_string()), span.clone()),
            expr1,
            expr2,
        ],
        span.clone(),
    ))
}
