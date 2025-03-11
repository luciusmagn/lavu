use color_eyre::eyre::Result;
use logos::Span as LogosSpan;

use crate::ast::ast2::TopLevelForm as TopLevelForm2;
use crate::ast::ast3::{Definition, Expression, Program, TopLevelForm};
use crate::lexer::Token;

pub fn parse(
    input: &str,
    tokens: &[(Token, &str, LogosSpan)],
    program2: &crate::ast::ast2::Program,
) -> Result<Program> {
    let forms = program2
        .forms
        .iter()
        .map(|form| convert_top_level_form(form))
        .collect::<Vec<_>>();

    Ok(Program {
        forms,
        source: input.to_string(),
        tokens: tokens
            .iter()
            .map(|(t, s, sp)| (t.clone(), s.to_string(), sp.clone()))
            .collect(),
    })
}

fn convert_top_level_form(form: &TopLevelForm2) -> TopLevelForm {
    match form {
        TopLevelForm2::Definition(def) => {
            TopLevelForm::Definition(Definition::from_ast2_definition(def))
        }
        TopLevelForm2::Expression(sexp) => {
            TopLevelForm::Expression(Expression::from_sexp(sexp))
        }
    }
}
