use color_eyre::eyre::bail;
use color_eyre::Result;
use logos::Span as LogosSpan;

use crate::ast::ast1::{Atom, SExp};
use crate::ast::ast2::{Definition, Program, TopLevelForm};
use crate::lexer::Token;

fn parse_top_level_form(sexp: &SExp) -> Result<TopLevelForm> {
    if let Some(definition) = try_parse_definition(sexp)? {
        Ok(TopLevelForm::Definition(definition))
    } else {
        Ok(TopLevelForm::Expression(sexp.clone()))
    }
}

fn try_parse_definition(sexp: &SExp) -> Result<Option<Definition>> {
    match sexp {
        SExp::List(elements, _span) => {
            // Check if this is a define form
            if elements.len() < 3 {
                return Ok(None);
            }

            if let SExp::Atom(Atom::Identifier(symbol), _) = &elements[0] {
                if symbol != "define" {
                    return Ok(None);
                }

                // It's a define form - now determine which type
                match &elements[1] {
                    // Variable definition: (define var expr)
                    SExp::Atom(Atom::Identifier(name), name_span) => {
                        if elements.len() != 3 {
                            bail!("Variable definition must have exactly one expression");
                        }

                        Ok(Some(Definition::Variable(
                            (name.clone(), name_span.clone()),
                            Box::new(elements[2].clone()),
                        )))
                    }

                    // Procedure shorthand: (define (name args...) body...)
                    SExp::List(params, _) => {
                        if params.is_empty() {
                            bail!("Procedure name list cannot be empty");
                        }

                        if let SExp::Atom(Atom::Identifier(proc_name), name_span) = &params[0] {
                            // Extract parameter names
                            let args = params[1..]
                                .iter()
                                .map(|param| {
                                    if let SExp::Atom(Atom::Identifier(arg_name), arg_span) = param {
                                        Ok((arg_name.clone(), arg_span.clone()))
                                    } else {
                                        bail!("Procedure parameters must be identifiers")
                                    }
                                })
                                .collect::<Result<Vec<_>>>()?;

                            // Get the body expressions
                            let body = elements[2..].to_vec();

                            Ok(Some(Definition::Procedure(
                                (proc_name.clone(), name_span.clone()),
                                args,
                                body,
                            )))
                        } else {
                            bail!("Procedure name must be an identifier")
                        }
                    }

                    _ => bail!("Second element of define must be an identifier or a list"),
                }
            } else {
                Ok(None)
            }
        }
        _ => Ok(None),
    }
}

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    sexps: &[SExp],
) -> Result<Program> {
    let forms = sexps
        .iter()
        .map(parse_top_level_form)
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
