use logos::Span as LogosSpan;
use std::ops::Range;

use crate::ast::ast1::SExp;
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    // Variable definition: (define var expr)
    Variable((String, Range<usize>), Box<SExp>),
    // Procedure shorthand: (define (name args...) body...)
    Procedure(
        (String, Range<usize>),
        Vec<(String, Range<usize>)>,
        Vec<SExp>,
    ),
}

impl Definition {
    pub fn name(&self) -> String {
        match self {
            Definition::Variable((name, _), _) => name.clone(),
            Definition::Procedure((name, _), _, _) => name.clone(),
        }
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Definition::Variable((_, span), _) => span.clone(),
            Definition::Procedure((_, span), _, _) => span.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelForm {
    Definition(Definition),
    Expression(SExp),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub forms: Vec<TopLevelForm>,
    pub source: String,
    pub tokens: Vec<(Token, String, LogosSpan)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LambdaBodyForm {
    Definition(Definition),
    Expression(SExp),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LambdaBody {
    pub forms: Vec<LambdaBodyForm>,
    pub has_definitions: bool,
}
