use ariadne::{Color, Label, Report, ReportKind, Source};
use color_eyre::Result;
use logos::Span as LogosSpan;
use std::ops::Range;

use crate::ast::ast1::Atom;
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Regular expression variants (from ast9)
    Atom(Atom, Range<usize>),
    List(Vec<Expression>, Range<usize>),
    Vector(Vec<Expression>, Range<usize>),
    Quote(Box<Expression>, Range<usize>),
    Quasiquote(Box<Expression>, Range<usize>),
    Unquote(Box<Expression>, Range<usize>),
    UnquoteSplicing(Box<Expression>, Range<usize>),
    SymbolLiteral(String, Range<usize>),
    Lambda(
        Vec<(String, Range<usize>)>, // arguments
        Vec<Expression>,             // body
        Range<usize>,                // span
    ),
    SetBang(
        (String, Range<usize>), // variable name
        Box<Expression>,        // value expression
        Range<usize>,           // span
    ),
    Begin(
        Vec<Expression>, // sequence of expressions
        Range<usize>,    // span
    ),
    If(
        Box<Expression>,         // condition
        Box<Expression>,         // then-expression
        Option<Box<Expression>>, // else-expression (optional)
        Range<usize>,            // span
    ),
    Let(
        Vec<((String, Range<usize>), Expression)>, // bindings: (var expr) pairs
        Vec<Expression>,                           // body expressions
        Range<usize>,                              // span
    ),
    LetStar(
        Vec<((String, Range<usize>), Expression)>, // bindings: (var expr) pairs
        Vec<Expression>,                           // body expressions
        Range<usize>,                              // span
    ),
    // New letrec form
    LetRec(
        Vec<((String, Range<usize>), Expression)>, // bindings: (var expr) pairs
        Vec<Expression>,                           // body expressions
        Range<usize>,                              // span
    ),
}

impl Expression {
    pub fn span(&self) -> &Range<usize> {
        match self {
            Expression::Atom(_, span) => span,
            Expression::List(_, span) => span,
            Expression::Vector(_, span) => span,
            Expression::Quote(_, span) => span,
            Expression::Quasiquote(_, span) => span,
            Expression::Unquote(_, span) => span,
            Expression::UnquoteSplicing(_, span) => span,
            Expression::SymbolLiteral(_, span) => span,
            Expression::Lambda(_, _, span) => span,
            Expression::SetBang(_, _, span) => span,
            Expression::Begin(_, span) => span,
            Expression::If(_, _, _, span) => span,
            Expression::Let(_, _, span) => span,
            Expression::LetStar(_, _, span) => span,
            Expression::LetRec(_, _, span) => span,
        }
    }

    pub fn convert_span(
        &self,
        tokens: &[(Token, &str, LogosSpan)],
    ) -> Range<usize> {
        let og_span = self.span();

        // Filter out whitespace and comments
        let tokens_nws = tokens
            .iter()
            .filter(|(t, _, _)| !t.is_whitespace() && !t.is_line_comment())
            .collect::<Vec<_>>();

        tokens_nws[og_span.start].2.start..tokens_nws[og_span.end - 1].2.end
    }

    fn convert_binding_span(
        var_span: &Range<usize>,
        tokens: &[(Token, &str, LogosSpan)],
    ) -> Range<usize> {
        let tokens_nws = tokens
            .iter()
            .filter(|(t, _, _)| !t.is_whitespace() && !t.is_line_comment())
            .collect::<Vec<_>>();

        tokens_nws[var_span.start].2.start..tokens_nws[var_span.end - 1].2.end
    }

    fn get_form_name(&self) -> &'static str {
        match self {
            Expression::Let(_, _, _) => "let",
            Expression::LetStar(_, _, _) => "let*",
            Expression::LetRec(_, _, _) => "letrec",
            _ => "expression",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Variable((String, Range<usize>), Box<Expression>),
}

impl Definition {
    pub fn name(&self) -> String {
        match self {
            Definition::Variable((name, _), _) => name.clone(),
        }
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Definition::Variable((_, span), _) => span.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelForm {
    Definition(Definition),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub forms: Vec<TopLevelForm>,
    pub source: String,
    pub tokens: Vec<(Token, String, LogosSpan)>,
}

pub fn report_let_forms(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program: &Program,
) -> Result<()> {
    fn traverse_expression(
        expr: &Expression,
        input: &str,
        tokens: &[(Token, &str, LogosSpan)],
        filename: &str,
    ) -> Result<()> {
        // Check if this is a let form
        match expr {
            Expression::Let(bindings, _, _)
            | Expression::LetStar(bindings, _, _)
            | Expression::LetRec(bindings, _, _) => {
                let form_name = expr.get_form_name();
                let expr_span = expr.convert_span(tokens);

                let mut report = Report::build(
                    ReportKind::Advice,
                    (filename, 0..input.len()),
                )
                .with_message(format!("Found {} expression", form_name))
                .with_label(
                    Label::new((filename, expr_span))
                        .with_message(format!("This is a {} form", form_name))
                        .with_color(Color::Blue),
                );

                // Highlight each variable binding
                for ((var_name, var_span), init_expr) in bindings {
                    let var_source_span =
                        Expression::convert_binding_span(var_span, tokens);

                    report = report.with_label(
                        Label::new((filename, var_source_span))
                            .with_message(format!("Variable: {}", var_name))
                            .with_color(Color::Green),
                    );
                }

                report.finish().eprint((filename, Source::from(input)))?;
            }
            _ => {} // Not a let form
        }

        // Recursively process nested expressions
        match expr {
            Expression::List(elements, _)
            | Expression::Vector(elements, _)
            | Expression::Begin(elements, _) => {
                for elem in elements {
                    traverse_expression(elem, input, tokens, filename)?;
                }
            }
            Expression::Lambda(_, body, _) => {
                for expr in body {
                    traverse_expression(expr, input, tokens, filename)?;
                }
            }
            Expression::If(cond, then_expr, else_expr, _) => {
                traverse_expression(cond, input, tokens, filename)?;
                traverse_expression(then_expr, input, tokens, filename)?;
                if let Some(else_branch) = else_expr {
                    traverse_expression(else_branch, input, tokens, filename)?;
                }
            }
            Expression::Let(bindings, body, _)
            | Expression::LetStar(bindings, body, _)
            | Expression::LetRec(bindings, body, _) => {
                // Process body expressions (already handled this form for reporting)
                for expr in body {
                    traverse_expression(expr, input, tokens, filename)?;
                }
            }
            Expression::Quote(inner, _)
            | Expression::Quasiquote(inner, _)
            | Expression::Unquote(inner, _)
            | Expression::UnquoteSplicing(inner, _) => {
                traverse_expression(inner, input, tokens, filename)?;
            }
            Expression::SetBang(_, expr, _) => {
                traverse_expression(expr, input, tokens, filename)?;
            }
            _ => {} // Nothing to do for atoms and other leaf nodes
        }

        Ok(())
    }

    // Process all top-level forms
    for form in &program.forms {
        match form {
            TopLevelForm::Expression(expr) => {
                traverse_expression(expr, input, tokens, "input.ss")?;
            }
            TopLevelForm::Definition(Definition::Variable(_, expr)) => {
                traverse_expression(expr, input, tokens, "input.ss")?;
            }
        }
    }

    Ok(())
}
