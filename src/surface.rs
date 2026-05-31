use thiserror::Error;

use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub forms: Vec<Spanned<TopLevel>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Define {
        name: Spanned<String>,
        value: Spanned<Expr>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Atom),
    Variable(String),
    Quote(Box<Spanned<Datum>>),
    Lambda {
        params: Vec<Spanned<String>>,
        body: Vec<Spanned<Expr>>,
    },
    If {
        condition: Box<Spanned<Expr>>,
        consequent: Box<Spanned<Expr>>,
        alternate: Option<Box<Spanned<Expr>>>,
    },
    Begin(Vec<Spanned<Expr>>),
    Set {
        name: Spanned<String>,
        value: Box<Spanned<Expr>>,
    },
    Apply {
        operator: Box<Spanned<Expr>>,
        operands: Vec<Spanned<Expr>>,
    },
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum SurfaceError {
    #[error("{form} expects {expected}")]
    BadArity {
        form: &'static str,
        expected: &'static str,
        span: SourceSpan,
    },

    #[error("{context} expects an identifier")]
    ExpectedIdentifier {
        context: &'static str,
        span: SourceSpan,
    },

    #[error("{context} expects a list")]
    ExpectedList {
        context: &'static str,
        span: SourceSpan,
    },

    #[error("empty application")]
    EmptyApplication { span: SourceSpan },

    #[error("unsupported datum in expression position")]
    UnsupportedDatum { span: SourceSpan },
}

pub fn classify_program(datums: &[Spanned<Datum>]) -> Result<Program, SurfaceError> {
    let forms = datums
        .iter()
        .map(classify_top_level)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Program { forms })
}

pub fn classify_top_level(datum: &Spanned<Datum>) -> Result<Spanned<TopLevel>, SurfaceError> {
    if let Some((name, value)) = parse_define(datum)? {
        return Ok(Spanned {
            node: TopLevel::Define { name, value },
            span: datum.span.clone(),
            origin: datum.origin,
        });
    }

    Ok(classify_expr(datum)?.map(TopLevel::Expr))
}

pub fn classify_expr(datum: &Spanned<Datum>) -> Result<Spanned<Expr>, SurfaceError> {
    let expr = match &datum.node {
        Datum::Atom(Atom::Identifier(name)) => Expr::Variable(name.clone()),
        Datum::Atom(atom) => Expr::Literal(atom.clone()),
        Datum::Quote(inner) => Expr::Quote(inner.clone()),
        Datum::List(items) => classify_list(datum.span.clone(), datum.origin, items)?,
        Datum::Vector(_) | Datum::Quasiquote(_) | Datum::Unquote(_) | Datum::UnquoteSplicing(_) => {
            return Err(SurfaceError::UnsupportedDatum {
                span: datum.span.clone(),
            });
        }
    };

    Ok(Spanned {
        node: expr,
        span: datum.span.clone(),
        origin: datum.origin,
    })
}

fn classify_list(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    items: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    let Some((head, rest)) = items.split_first() else {
        return Err(SurfaceError::EmptyApplication { span });
    };

    let head_name = identifier_name(head);
    match head_name.as_deref() {
        Some("quote") => parse_quote(span, rest),
        Some("lambda") => parse_lambda(rest),
        Some("if") => parse_if(rest),
        Some("begin") => parse_begin(rest),
        Some("set!") => parse_set(rest),
        Some("let") => parse_let(span, origin, rest),
        Some("let*") => parse_let_star(span, origin, rest),
        Some("and") => parse_and(span, origin, rest),
        Some("or") => parse_or(span, origin, rest),
        Some("cond") => parse_cond(span, origin, rest),
        _ => parse_apply(origin, head, rest),
    }
}

fn parse_define(
    datum: &Spanned<Datum>,
) -> Result<Option<(Spanned<String>, Spanned<Expr>)>, SurfaceError> {
    let Datum::List(items) = &datum.node else {
        return Ok(None);
    };
    let Some((head, rest)) = items.split_first() else {
        return Ok(None);
    };
    if identifier_name(head).as_deref() != Some("define") {
        return Ok(None);
    }

    if rest.len() != 2 {
        return Err(SurfaceError::BadArity {
            form: "define",
            expected: "a name and a value",
            span: datum.span.clone(),
        });
    }

    let name = expect_identifier(&rest[0], "define")?;
    let value = classify_expr(&rest[1])?;
    Ok(Some((name, value)))
}

fn parse_quote(span: SourceSpan, rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    if rest.len() != 1 {
        return Err(SurfaceError::BadArity {
            form: "quote",
            expected: "one datum",
            span,
        });
    }

    Ok(Expr::Quote(Box::new(rest[0].clone())))
}

fn parse_lambda(rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        let span = rest.first().map(|item| item.span.clone()).unwrap_or(0..0);
        return Err(SurfaceError::BadArity {
            form: "lambda",
            expected: "formals and at least one body expression",
            span,
        });
    }

    let params = parse_fixed_formals(&rest[0])?;
    let body = rest[1..]
        .iter()
        .map(classify_expr)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Expr::Lambda { params, body })
}

fn parse_if(rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    if !(2..=3).contains(&rest.len()) {
        let span = rest.first().map(|item| item.span.clone()).unwrap_or(0..0);
        return Err(SurfaceError::BadArity {
            form: "if",
            expected: "a condition, consequent, and optional alternate",
            span,
        });
    }

    Ok(Expr::If {
        condition: Box::new(classify_expr(&rest[0])?),
        consequent: Box::new(classify_expr(&rest[1])?),
        alternate: rest.get(2).map(classify_expr).transpose()?.map(Box::new),
    })
}

fn parse_begin(rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    Ok(Expr::Begin(
        rest.iter()
            .map(classify_expr)
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn parse_set(rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    if rest.len() != 2 {
        let span = rest.first().map(|item| item.span.clone()).unwrap_or(0..0);
        return Err(SurfaceError::BadArity {
            form: "set!",
            expected: "a name and a value",
            span,
        });
    }

    Ok(Expr::Set {
        name: expect_identifier(&rest[0], "set!")?,
        value: Box::new(classify_expr(&rest[1])?),
    })
}

fn parse_let(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "let",
            expected: "bindings and at least one body expression",
            span,
        });
    }

    let Datum::List(binding_datums) = &rest[0].node else {
        return Err(SurfaceError::ExpectedList {
            context: "let bindings",
            span: rest[0].span.clone(),
        });
    };

    let mut params = Vec::new();
    let mut operands = Vec::new();

    for binding in binding_datums {
        let Datum::List(pair) = &binding.node else {
            return Err(SurfaceError::ExpectedList {
                context: "let binding",
                span: binding.span.clone(),
            });
        };
        if pair.len() != 2 {
            return Err(SurfaceError::BadArity {
                form: "let binding",
                expected: "a name and a value",
                span: binding.span.clone(),
            });
        }

        params.push(expect_identifier(&pair[0], "let binding")?);
        operands.push(classify_expr(&pair[1])?);
    }

    let body = rest[1..]
        .iter()
        .map(classify_expr)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Expr::Apply {
        operator: Box::new(Spanned {
            node: Expr::Lambda { params, body },
            span: span.clone(),
            origin,
        }),
        operands,
    })
}

fn parse_let_star(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "let*",
            expected: "bindings and at least one body expression",
            span,
        });
    }

    let bindings = parse_bindings(&rest[0], "let* bindings")?;
    let mut current = Spanned {
        node: body_expr(&rest[1..], span.clone(), origin)?,
        span: span.clone(),
        origin,
    };

    for (name, value) in bindings.into_iter().rev() {
        current = Spanned {
            node: Expr::Apply {
                operator: Box::new(Spanned {
                    node: Expr::Lambda {
                        params: vec![name],
                        body: vec![current],
                    },
                    span: span.clone(),
                    origin,
                }),
                operands: vec![value],
            },
            span: span.clone(),
            origin,
        };
    }

    Ok(current.node)
}

fn parse_bindings(
    bindings: &Spanned<Datum>,
    context: &'static str,
) -> Result<Vec<(Spanned<String>, Spanned<Expr>)>, SurfaceError> {
    let Datum::List(binding_datums) = &bindings.node else {
        return Err(SurfaceError::ExpectedList {
            context,
            span: bindings.span.clone(),
        });
    };

    binding_datums
        .iter()
        .map(|binding| {
            let Datum::List(pair) = &binding.node else {
                return Err(SurfaceError::ExpectedList {
                    context: "binding",
                    span: binding.span.clone(),
                });
            };
            if pair.len() != 2 {
                return Err(SurfaceError::BadArity {
                    form: "binding",
                    expected: "a name and a value",
                    span: binding.span.clone(),
                });
            }

            Ok((
                expect_identifier(&pair[0], "binding")?,
                classify_expr(&pair[1])?,
            ))
        })
        .collect()
}

fn parse_and(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    match rest {
        [] => Ok(boolean_literal(true)),
        [single] => Ok(classify_expr(single)?.node),
        [first, remaining @ ..] => {
            let condition = classify_expr(first)?;
            let consequent = Spanned {
                node: parse_and(span.clone(), origin, remaining)?,
                span: span.clone(),
                origin,
            };
            Ok(Expr::If {
                condition: Box::new(condition),
                consequent: Box::new(consequent),
                alternate: Some(Box::new(Spanned {
                    node: boolean_literal(false),
                    span,
                    origin,
                })),
            })
        }
    }
}

fn parse_or(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    match rest {
        [] => Ok(boolean_literal(false)),
        [single] => Ok(classify_expr(single)?.node),
        [first, remaining @ ..] => {
            let temp = Spanned {
                node: "__lavu_or_value".to_string(),
                span: first.span.clone(),
                origin,
            };
            let condition = Spanned {
                node: Expr::Variable(temp.node.clone()),
                span: first.span.clone(),
                origin,
            };
            let alternate = Spanned {
                node: parse_or(span.clone(), origin, remaining)?,
                span: span.clone(),
                origin,
            };

            Ok(Expr::Apply {
                operator: Box::new(Spanned {
                    node: Expr::Lambda {
                        params: vec![temp.clone()],
                        body: vec![Spanned {
                            node: Expr::If {
                                condition: Box::new(condition),
                                consequent: Box::new(Spanned {
                                    node: Expr::Variable(temp.node),
                                    span: temp.span,
                                    origin,
                                }),
                                alternate: Some(Box::new(alternate)),
                            },
                            span: span.clone(),
                            origin,
                        }],
                    },
                    span: span.clone(),
                    origin,
                }),
                operands: vec![classify_expr(first)?],
            })
        }
    }
}

fn parse_cond(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    clauses: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    let mut result = Spanned {
        node: boolean_literal(false),
        span: span.clone(),
        origin,
    };

    for clause in clauses.iter().rev() {
        let Datum::List(items) = &clause.node else {
            return Err(SurfaceError::ExpectedList {
                context: "cond clause",
                span: clause.span.clone(),
            });
        };
        let Some((test, body)) = items.split_first() else {
            return Err(SurfaceError::BadArity {
                form: "cond clause",
                expected: "a test and optional body expressions",
                span: clause.span.clone(),
            });
        };

        if identifier_name(test).as_deref() == Some("else") {
            result = Spanned {
                node: body_expr(body, clause.span.clone(), origin)?,
                span: clause.span.clone(),
                origin,
            };
            continue;
        }

        let condition = classify_expr(test)?;
        let consequent = if body.is_empty() {
            condition.clone()
        } else {
            Spanned {
                node: body_expr(body, clause.span.clone(), origin)?,
                span: clause.span.clone(),
                origin,
            }
        };

        result = Spanned {
            node: Expr::If {
                condition: Box::new(condition),
                consequent: Box::new(consequent),
                alternate: Some(Box::new(result)),
            },
            span: clause.span.clone(),
            origin,
        };
    }

    Ok(result.node)
}

fn body_expr(
    body: &[Spanned<Datum>],
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
) -> Result<Expr, SurfaceError> {
    match body {
        [] => Ok(Expr::Begin(Vec::new())),
        [single] => Ok(classify_expr(single)?.node),
        many => Ok(Expr::Begin(
            many.iter()
                .map(classify_expr)
                .collect::<Result<Vec<_>, _>>()?,
        )),
    }
    .map(|expr| match expr {
        Expr::Begin(exprs) if exprs.is_empty() => Expr::Begin(vec![Spanned {
            node: Expr::Literal(Atom::Boolean(false)),
            span,
            origin,
        }]),
        expr => expr,
    })
}

fn boolean_literal(value: bool) -> Expr {
    Expr::Literal(Atom::Boolean(value))
}

fn parse_apply(
    _origin: Option<crate::syntax::NodeId>,
    head: &Spanned<Datum>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    Ok(Expr::Apply {
        operator: Box::new(classify_expr(head)?),
        operands: rest
            .iter()
            .map(classify_expr)
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn parse_fixed_formals(formals: &Spanned<Datum>) -> Result<Vec<Spanned<String>>, SurfaceError> {
    match &formals.node {
        Datum::List(items) => items
            .iter()
            .map(|item| expect_identifier(item, "lambda formals"))
            .collect(),
        _ => Err(SurfaceError::ExpectedList {
            context: "lambda formals",
            span: formals.span.clone(),
        }),
    }
}

fn expect_identifier(
    datum: &Spanned<Datum>,
    context: &'static str,
) -> Result<Spanned<String>, SurfaceError> {
    if let Some(name) = identifier_name(datum) {
        Ok(Spanned {
            node: name,
            span: datum.span.clone(),
            origin: datum.origin,
        })
    } else {
        Err(SurfaceError::ExpectedIdentifier {
            context,
            span: datum.span.clone(),
        })
    }
}

fn identifier_name(datum: &Spanned<Datum>) -> Option<String> {
    match &datum.node {
        Datum::Atom(Atom::Identifier(name)) => Some(name.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::datum_parser::parse;
    use crate::surface::{classify_program, classify_top_level, Expr, TopLevel};

    #[test]
    fn classifies_define_and_lambda() {
        let datums = parse("(define add1 (lambda (x) (+ x 1)))").unwrap();
        let program = classify_program(&datums).unwrap();

        let TopLevel::Define { name, value } = &program.forms[0].node else {
            panic!("expected define");
        };
        assert_eq!(name.node, "add1");
        assert!(matches!(value.node, Expr::Lambda { .. }));
    }

    #[test]
    fn classifies_if_with_application_branches() {
        let datums = parse("(if (string? x) (string-length x) (+ x 1))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        let TopLevel::Expr(Expr::If {
            condition,
            consequent,
            alternate,
        }) = form.node
        else {
            panic!("expected if expression");
        };

        assert!(matches!(condition.node, Expr::Apply { .. }));
        assert!(matches!(consequent.node, Expr::Apply { .. }));
        assert!(alternate.is_some());
    }

    #[test]
    fn keeps_quote_payload_as_datum() {
        let datums = parse("'(and x y)").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        let TopLevel::Expr(Expr::Quote(payload)) = form.node else {
            panic!("expected quote");
        };

        assert_eq!(payload.span, 1..10);
    }

    #[test]
    fn desugars_regular_let_to_lambda_application() {
        let datums = parse("(let ((x 1)) (+ x 1))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::Apply { .. })));
    }

    #[test]
    fn desugars_let_star_to_nested_lambda_applications() {
        let datums = parse("(let* ((x 1) (y (+ x 1))) y)").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::Apply { .. })));
    }

    #[test]
    fn desugars_and_or_to_conditionals() {
        let and_datums = parse("(and (string? x) (string-length x))").unwrap();
        let and_form = classify_top_level(&and_datums[0]).unwrap();
        assert!(matches!(and_form.node, TopLevel::Expr(Expr::If { .. })));

        let or_datums = parse("(or #f 1)").unwrap();
        let or_form = classify_top_level(&or_datums[0]).unwrap();
        assert!(matches!(or_form.node, TopLevel::Expr(Expr::Apply { .. })));
    }

    #[test]
    fn desugars_cond_to_conditionals() {
        let datums = parse("(cond ((string? x) 1) (else 2))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::If { .. })));
    }
}
