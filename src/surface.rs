use std::collections::{BTreeMap, BTreeSet};

use thiserror::Error;

use crate::syntax::{Atom, Datum, SourceSpan, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub forms: Vec<Spanned<TopLevel>>,
}

pub type Binding = (Spanned<String>, Spanned<Expr>);
type DefineBinding = (Spanned<String>, Spanned<Expr>);
type Formals = (Vec<Spanned<String>>, Option<Spanned<String>>);
type DefineFormals<'a> = (
    &'a Spanned<Datum>,
    &'a [Spanned<Datum>],
    Option<Spanned<String>>,
);

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
    Quasiquote(Box<Spanned<Datum>>),
    Lambda {
        params: Vec<Spanned<String>>,
        rest: Option<Spanned<String>>,
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
    Delay(Box<Spanned<Expr>>),
    LetRec {
        bindings: Vec<Binding>,
        body: Vec<Spanned<Expr>>,
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

    #[error("{context} has duplicate identifier {name}")]
    DuplicateIdentifier {
        context: &'static str,
        name: String,
        span: SourceSpan,
    },

    #[error("{context} reserves identifier {name}")]
    ReservedIdentifier {
        context: &'static str,
        name: String,
        span: SourceSpan,
    },

    #[error("empty application")]
    EmptyApplication { span: SourceSpan },

    #[error("unsupported datum in expression position")]
    UnsupportedDatum { span: SourceSpan },

    #[error("{form} is only valid in definition context")]
    DefinitionContext {
        form: &'static str,
        span: SourceSpan,
    },

    #[error("unsupported macro pattern")]
    UnsupportedMacroPattern { span: SourceSpan },

    #[error("invalid macro template")]
    InvalidMacroTemplate { span: SourceSpan },

    #[error("no matching macro rule for {name}")]
    NoMatchingMacroRule { name: String, span: SourceSpan },

    #[error("macro expansion limit reached")]
    MacroExpansionLimit { span: SourceSpan },
}

#[derive(Debug, Clone, Default)]
pub struct SurfaceContext {
    expander: MacroExpander,
}

impl SurfaceContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn classify_program(&mut self, datums: &[Spanned<Datum>]) -> Result<Program, SurfaceError> {
        let mut forms = Vec::new();

        for datum in datums {
            classify_top_level_with_macros(&mut self.expander, datum, &mut forms)?;
        }

        Ok(Program { forms })
    }
}

pub fn classify_program(datums: &[Spanned<Datum>]) -> Result<Program, SurfaceError> {
    SurfaceContext::new().classify_program(datums)
}

fn classify_top_level_with_macros(
    expander: &mut MacroExpander,
    datum: &Spanned<Datum>,
    forms: &mut Vec<Spanned<TopLevel>>,
) -> Result<(), SurfaceError> {
    if let Some(body) = top_level_begin_body(datum) {
        for datum in body {
            classify_top_level_with_macros(expander, datum, forms)?;
        }
        return Ok(());
    }

    if let Some((name, rules)) = parse_define_syntax(datum)? {
        expander.define(name, rules);
        return Ok(());
    }

    let expanded = expander.expand(datum)?;
    if let Some((name, rules)) = parse_define_syntax(&expanded)? {
        expander.define(name, rules);
        return Ok(());
    }
    if let Some(body) = top_level_begin_body(&expanded) {
        for datum in body {
            classify_top_level_with_macros(expander, datum, forms)?;
        }
        return Ok(());
    }

    forms.push(classify_top_level(&expanded)?);
    Ok(())
}

#[derive(Debug, Clone, Default)]
struct MacroExpander {
    bindings: BTreeMap<String, SyntaxRules>,
}

#[derive(Debug, Clone)]
struct SyntaxRules {
    literals: BTreeSet<String>,
    rules: Vec<SyntaxRule>,
}

#[derive(Debug, Clone)]
struct SyntaxRule {
    pattern: Spanned<Datum>,
    template: Spanned<Datum>,
}

#[derive(Debug, Clone)]
enum Capture {
    Single(Spanned<Datum>),
    Repeated(Vec<Spanned<Datum>>),
}

struct DottedDatum<'a> {
    items: &'a [Spanned<Datum>],
    tail: Option<&'a Spanned<Datum>>,
    original: &'a Spanned<Datum>,
}

impl MacroExpander {
    fn define(&mut self, name: String, rules: SyntaxRules) {
        self.bindings.insert(name, rules);
    }

    fn without_syntax_names(&self, names: &BTreeSet<String>) -> Self {
        let mut expander = self.clone();
        for name in names {
            expander.bindings.remove(name);
        }
        expander
    }

    fn expand(&self, datum: &Spanned<Datum>) -> Result<Spanned<Datum>, SurfaceError> {
        self.expand_with_depth(datum, 0)
    }

    fn expand_with_depth(
        &self,
        datum: &Spanned<Datum>,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        if depth > 256 {
            return Err(SurfaceError::MacroExpansionLimit {
                span: datum.span.clone(),
            });
        }

        match &datum.node {
            Datum::List(items) => {
                if is_quoted_list(items) {
                    return Ok(datum.clone());
                }
                if let Some(form) = items.first().and_then(identifier_name) {
                    match form.as_str() {
                        "lambda" => {
                            return self.expand_literal_prefixed_body_form(datum, 2, depth);
                        }
                        "define" => {
                            return self.expand_define_form(datum, items, depth);
                        }
                        "let" => {
                            return self.expand_let_form(datum, items, depth);
                        }
                        "let*" | "letrec" => {
                            return self.expand_binding_body_form(datum, items, 1, 2, depth);
                        }
                        "cond" => {
                            return self.expand_cond_form(datum, items, depth);
                        }
                        "case" => {
                            return self.expand_case_form(datum, items, depth);
                        }
                        "do" => {
                            return self.expand_do_form(datum, items, depth);
                        }
                        "let-syntax" => {
                            return self.expand_local_syntax("let-syntax", datum, items, depth);
                        }
                        "letrec-syntax" => {
                            return self.expand_local_syntax("letrec-syntax", datum, items, depth);
                        }
                        _ => {}
                    }
                }

                if let Some(name) = items.first().and_then(identifier_name)
                    && let Some(rules) = self.bindings.get(&name)
                {
                    let expanded = apply_syntax_rules(&name, rules, datum)?;
                    return self.expand_with_depth(&expanded, depth + 1);
                }

                items
                    .iter()
                    .map(|item| self.expand_with_depth(item, depth))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|items| datum.with_node(Datum::List(items)))
            }
            Datum::DottedList(items, tail) => {
                if let Some(name) = items.first().and_then(identifier_name)
                    && let Some(rules) = self.bindings.get(&name)
                {
                    let expanded = apply_syntax_rules(&name, rules, datum)?;
                    return self.expand_with_depth(&expanded, depth + 1);
                }

                let items = items
                    .iter()
                    .map(|item| self.expand_with_depth(item, depth))
                    .collect::<Result<Vec<_>, _>>()?;
                let tail = self.expand_with_depth(tail, depth)?;
                Ok(datum.with_node(Datum::DottedList(items, Box::new(tail))))
            }
            Datum::Vector(items) => {
                let items = items
                    .iter()
                    .map(|item| self.expand_with_depth(item, depth))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(datum.with_node(Datum::Vector(items)))
            }
            Datum::Atom(_)
            | Datum::Quote(_)
            | Datum::Quasiquote(_)
            | Datum::Unquote(_)
            | Datum::UnquoteSplicing(_) => Ok(datum.clone()),
        }
    }

    fn expand_local_syntax(
        &self,
        form: &'static str,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        if items.len() < 3 {
            return Err(SurfaceError::BadArity {
                form,
                expected: "syntax bindings and at least one body expression",
                span: datum.span.clone(),
            });
        }

        let mut local = self.clone();
        for (name, rules) in parse_syntax_bindings(&items[1])? {
            local.define(name, rules);
        }

        let body = local.expand_body_items(&items[2..], depth + 1)?;
        Ok(datum.with_node(Datum::List(
            std::iter::once(datum.with_node(Datum::identifier("begin")))
                .chain(body)
                .collect(),
        )))
    }

    fn expand_define_form(
        &self,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        if matches!(
            items.get(1).map(|item| &item.node),
            Some(Datum::List(_) | Datum::DottedList(_, _))
        ) {
            return self.expand_literal_prefixed_body_form(datum, 2, depth);
        }

        self.expand_ordinary_list(datum, items, depth)
    }

    fn expand_let_form(
        &self,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let body_start = if items
            .get(1)
            .is_some_and(|item| identifier_name(item).is_some())
        {
            3
        } else {
            2
        };
        let binding_index = if body_start == 3 { 2 } else { 1 };
        self.expand_binding_body_form(datum, items, binding_index, body_start, depth)
    }

    fn expand_literal_prefixed_body_form(
        &self,
        datum: &Spanned<Datum>,
        body_start: usize,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(items) = &datum.node else {
            return Ok(datum.clone());
        };
        if items.len() <= body_start {
            return self.expand_ordinary_list(datum, items, depth);
        }

        let prefix = items[..body_start].to_vec();
        let body = self.expand_body_items(&items[body_start..], depth + 1)?;

        Ok(datum.with_node(Datum::List(prefix.into_iter().chain(body).collect())))
    }

    fn expand_binding_body_form(
        &self,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        binding_index: usize,
        body_start: usize,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        if items.len() <= body_start || items.len() <= binding_index {
            return self.expand_ordinary_list(datum, items, depth);
        }

        let binding_names = binding_names(&items[binding_index]);
        let body_expander = self.without_syntax_names(&binding_names);
        let mut expanded = items[..binding_index].to_vec();
        expanded.push(self.expand_binding_list(&items[binding_index], depth)?);
        expanded.extend(body_expander.expand_body_items(&items[body_start..], depth + 1)?);

        Ok(datum.with_node(Datum::List(expanded)))
    }

    fn expand_binding_list(
        &self,
        datum: &Spanned<Datum>,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(bindings) = &datum.node else {
            return self.expand_with_depth(datum, depth);
        };

        bindings
            .iter()
            .map(|binding| self.expand_binding(binding, depth))
            .collect::<Result<Vec<_>, _>>()
            .map(|bindings| datum.with_node(Datum::List(bindings)))
    }

    fn expand_binding(
        &self,
        binding: &Spanned<Datum>,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(parts) = &binding.node else {
            return self.expand_with_depth(binding, depth);
        };
        let Some((name, values)) = parts.split_first() else {
            return Ok(binding.clone());
        };

        let mut expanded = vec![name.clone()];
        expanded.extend(
            values
                .iter()
                .map(|value| self.expand_with_depth(value, depth))
                .collect::<Result<Vec<_>, _>>()?,
        );

        Ok(binding.with_node(Datum::List(expanded)))
    }

    fn expand_cond_form(
        &self,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let mut expanded = vec![items[0].clone()];
        expanded.extend(
            items[1..]
                .iter()
                .map(|clause| self.expand_cond_clause(clause, depth))
                .collect::<Result<Vec<_>, _>>()?,
        );

        Ok(datum.with_node(Datum::List(expanded)))
    }

    fn expand_cond_clause(
        &self,
        clause: &Spanned<Datum>,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(items) = &clause.node else {
            return self.expand_with_depth(clause, depth);
        };
        let Some((test, rest)) = items.split_first() else {
            return Ok(clause.clone());
        };

        let expanded = if identifier_name(test).as_deref() == Some("else") {
            std::iter::once(test.clone())
                .chain(
                    rest.iter()
                        .map(|item| self.expand_with_depth(item, depth))
                        .collect::<Result<Vec<_>, _>>()?,
                )
                .collect()
        } else if rest.first().and_then(identifier_name).as_deref() == Some("=>") {
            let mut clause = vec![self.expand_with_depth(test, depth)?, rest[0].clone()];
            clause.extend(
                rest[1..]
                    .iter()
                    .map(|item| self.expand_with_depth(item, depth))
                    .collect::<Result<Vec<_>, _>>()?,
            );
            clause
        } else {
            items
                .iter()
                .map(|item| self.expand_with_depth(item, depth))
                .collect::<Result<Vec<_>, _>>()?
        };

        Ok(clause.with_node(Datum::List(expanded)))
    }

    fn expand_case_form(
        &self,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        if items.len() < 2 {
            return self.expand_ordinary_list(datum, items, depth);
        }

        let mut expanded = vec![items[0].clone(), self.expand_with_depth(&items[1], depth)?];
        expanded.extend(
            items[2..]
                .iter()
                .map(|clause| self.expand_case_clause(clause, depth))
                .collect::<Result<Vec<_>, _>>()?,
        );

        Ok(datum.with_node(Datum::List(expanded)))
    }

    fn expand_case_clause(
        &self,
        clause: &Spanned<Datum>,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(items) = &clause.node else {
            return self.expand_with_depth(clause, depth);
        };
        let Some((head, body)) = items.split_first() else {
            return Ok(clause.clone());
        };

        let expanded = std::iter::once(head.clone())
            .chain(
                body.iter()
                    .map(|item| self.expand_with_depth(item, depth))
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .collect();

        Ok(clause.with_node(Datum::List(expanded)))
    }

    fn expand_do_form(
        &self,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        if items.len() < 3 {
            return self.expand_ordinary_list(datum, items, depth);
        }

        let binding_names = binding_names(&items[1]);
        let body_expander = self.without_syntax_names(&binding_names);
        let mut expanded = vec![
            items[0].clone(),
            self.expand_do_binding_list(&items[1], &body_expander, depth)?,
            body_expander.expand_do_test_clause(&items[2], depth)?,
        ];
        expanded.extend(body_expander.expand_body_items(&items[3..], depth + 1)?);

        Ok(datum.with_node(Datum::List(expanded)))
    }

    fn expand_do_binding_list(
        &self,
        datum: &Spanned<Datum>,
        body_expander: &MacroExpander,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(bindings) = &datum.node else {
            return self.expand_with_depth(datum, depth);
        };

        bindings
            .iter()
            .map(|binding| self.expand_do_binding(binding, body_expander, depth))
            .collect::<Result<Vec<_>, _>>()
            .map(|bindings| datum.with_node(Datum::List(bindings)))
    }

    fn expand_do_binding(
        &self,
        binding: &Spanned<Datum>,
        body_expander: &MacroExpander,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(parts) = &binding.node else {
            return self.expand_with_depth(binding, depth);
        };
        let Some((name, values)) = parts.split_first() else {
            return Ok(binding.clone());
        };

        let mut expanded = vec![name.clone()];
        if let Some(init) = values.first() {
            expanded.push(self.expand_with_depth(init, depth)?);
        }
        expanded.extend(
            values[1..]
                .iter()
                .map(|step| body_expander.expand_with_depth(step, depth))
                .collect::<Result<Vec<_>, _>>()?,
        );

        Ok(binding.with_node(Datum::List(expanded)))
    }

    fn expand_do_test_clause(
        &self,
        clause: &Spanned<Datum>,
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        let Datum::List(items) = &clause.node else {
            return self.expand_with_depth(clause, depth);
        };

        items
            .iter()
            .map(|item| self.expand_with_depth(item, depth))
            .collect::<Result<Vec<_>, _>>()
            .map(|items| clause.with_node(Datum::List(items)))
    }

    fn expand_body_items(
        &self,
        body: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Vec<Spanned<Datum>>, SurfaceError> {
        let mut local = self.clone();
        let mut expanded = Vec::new();
        let mut index = 0;

        while index < body.len() {
            if let Some((name, rules)) = parse_define_syntax(&body[index])? {
                local.define(name, rules);
                index += 1;
                continue;
            }
            if !is_definition_form(&body[index]) {
                break;
            }

            expanded.push(local.expand_with_depth(&body[index], depth)?);
            index += 1;
        }

        for item in &body[index..] {
            expanded.push(local.expand_with_depth(item, depth)?);
        }

        Ok(expanded)
    }

    fn expand_ordinary_list(
        &self,
        datum: &Spanned<Datum>,
        items: &[Spanned<Datum>],
        depth: usize,
    ) -> Result<Spanned<Datum>, SurfaceError> {
        items
            .iter()
            .map(|item| self.expand_with_depth(item, depth))
            .collect::<Result<Vec<_>, _>>()
            .map(|items| datum.with_node(Datum::List(items)))
    }
}

fn parse_define_syntax(
    datum: &Spanned<Datum>,
) -> Result<Option<(String, SyntaxRules)>, SurfaceError> {
    let Datum::List(items) = &datum.node else {
        return Ok(None);
    };
    let Some((head, rest)) = items.split_first() else {
        return Ok(None);
    };
    if identifier_name(head).as_deref() != Some("define-syntax") {
        return Ok(None);
    }
    if rest.len() != 2 {
        return Err(SurfaceError::BadArity {
            form: "define-syntax",
            expected: "a keyword and syntax-rules transformer",
            span: datum.span.clone(),
        });
    }

    let name = expect_identifier(&rest[0], "define-syntax")?.node;
    let rules = parse_syntax_rules(&rest[1])?;
    Ok(Some((name, rules)))
}

fn parse_syntax_rules(datum: &Spanned<Datum>) -> Result<SyntaxRules, SurfaceError> {
    let Datum::List(items) = &datum.node else {
        return Err(SurfaceError::ExpectedList {
            context: "syntax-rules",
            span: datum.span.clone(),
        });
    };
    let Some((head, rest)) = items.split_first() else {
        return Err(SurfaceError::BadArity {
            form: "syntax-rules",
            expected: "literal identifiers and at least one rule",
            span: datum.span.clone(),
        });
    };
    if identifier_name(head).as_deref() != Some("syntax-rules") || rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "syntax-rules",
            expected: "literal identifiers and at least one rule",
            span: datum.span.clone(),
        });
    }

    let literals = parse_literal_identifiers(&rest[0])?;
    let rules = rest[1..]
        .iter()
        .map(|datum| parse_syntax_rule(datum, &literals))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(SyntaxRules { literals, rules })
}

fn parse_syntax_bindings(
    datum: &Spanned<Datum>,
) -> Result<Vec<(String, SyntaxRules)>, SurfaceError> {
    let Datum::List(bindings) = &datum.node else {
        return Err(SurfaceError::ExpectedList {
            context: "syntax bindings",
            span: datum.span.clone(),
        });
    };

    let mut names = BTreeSet::new();
    let mut parsed = Vec::new();
    for binding in bindings {
        let Datum::List(items) = &binding.node else {
            return Err(SurfaceError::ExpectedList {
                context: "syntax binding",
                span: binding.span.clone(),
            });
        };
        if items.len() != 2 {
            return Err(SurfaceError::BadArity {
                form: "syntax binding",
                expected: "a keyword and syntax-rules transformer",
                span: binding.span.clone(),
            });
        }

        let name = expect_identifier(&items[0], "syntax binding")?;
        if !names.insert(name.node.clone()) {
            return Err(SurfaceError::DuplicateIdentifier {
                context: "syntax bindings",
                name: name.node,
                span: name.span,
            });
        }
        parsed.push((name.node, parse_syntax_rules(&items[1])?));
    }

    Ok(parsed)
}

fn parse_literal_identifiers(datum: &Spanned<Datum>) -> Result<BTreeSet<String>, SurfaceError> {
    let Datum::List(items) = &datum.node else {
        return Err(SurfaceError::ExpectedList {
            context: "syntax-rules literals",
            span: datum.span.clone(),
        });
    };

    let mut literals = BTreeSet::new();
    for item in items {
        let name = expect_identifier(item, "syntax-rules literal")?;
        if name.node == "..." {
            return Err(SurfaceError::ReservedIdentifier {
                context: "syntax-rules literals",
                name: name.node,
                span: name.span,
            });
        }
        if !literals.insert(name.node.clone()) {
            return Err(SurfaceError::DuplicateIdentifier {
                context: "syntax-rules literals",
                name: name.node,
                span: name.span,
            });
        }
    }
    Ok(literals)
}

fn parse_syntax_rule(
    datum: &Spanned<Datum>,
    literals: &BTreeSet<String>,
) -> Result<SyntaxRule, SurfaceError> {
    let Datum::List(items) = &datum.node else {
        return Err(SurfaceError::ExpectedList {
            context: "syntax-rules rule",
            span: datum.span.clone(),
        });
    };
    if items.len() != 2 {
        return Err(SurfaceError::BadArity {
            form: "syntax-rules rule",
            expected: "a pattern and a template",
            span: datum.span.clone(),
        });
    }

    ensure_distinct_pattern_variables(&items[0], literals)?;
    validate_template_ellipsis(&items[1])?;

    Ok(SyntaxRule {
        pattern: items[0].clone(),
        template: items[1].clone(),
    })
}

fn ensure_distinct_pattern_variables(
    pattern: &Spanned<Datum>,
    literals: &BTreeSet<String>,
) -> Result<(), SurfaceError> {
    let keyword = syntax_rule_keyword(pattern)?;
    validate_pattern_ellipsis(pattern)?;
    let mut seen = BTreeSet::new();

    match &pattern.node {
        Datum::List(items) => {
            for item in &items[1..] {
                collect_unique_pattern_variables(item, literals, &keyword, &mut seen)?;
            }
        }
        Datum::DottedList(items, tail) => {
            for item in &items[1..] {
                collect_unique_pattern_variables(item, literals, &keyword, &mut seen)?;
            }
            collect_unique_pattern_variables(tail, literals, &keyword, &mut seen)?;
        }
        _ => unreachable!("syntax_rule_keyword rejects non-list patterns"),
    }

    Ok(())
}

fn validate_pattern_ellipsis(pattern: &Spanned<Datum>) -> Result<(), SurfaceError> {
    match &pattern.node {
        Datum::List(items) => validate_ellipsis_sequence(&items[1..], EllipsisContext::Pattern),
        Datum::DottedList(items, tail) => {
            validate_ellipsis_sequence(&items[1..], EllipsisContext::Pattern)?;
            validate_ellipsis_item(tail, EllipsisContext::Pattern)
        }
        _ => unreachable!("syntax_rule_keyword rejects non-list patterns"),
    }
}

fn validate_template_ellipsis(template: &Spanned<Datum>) -> Result<(), SurfaceError> {
    validate_ellipsis_item(template, EllipsisContext::Template)
}

#[derive(Clone, Copy)]
enum EllipsisContext {
    Pattern,
    Template,
}

impl EllipsisContext {
    fn error(self, span: SourceSpan) -> SurfaceError {
        match self {
            Self::Pattern => SurfaceError::UnsupportedMacroPattern { span },
            Self::Template => SurfaceError::InvalidMacroTemplate { span },
        }
    }
}

fn validate_ellipsis_item(
    datum: &Spanned<Datum>,
    context: EllipsisContext,
) -> Result<(), SurfaceError> {
    match &datum.node {
        Datum::Atom(Atom::Identifier(name)) if name == "..." => {
            Err(context.error(datum.span.clone()))
        }
        Datum::List(items) | Datum::Vector(items) => validate_ellipsis_sequence(items, context),
        Datum::DottedList(items, tail) => {
            validate_ellipsis_sequence(items, context)?;
            validate_ellipsis_item(tail, context)
        }
        Datum::Quote(inner)
        | Datum::Quasiquote(inner)
        | Datum::Unquote(inner)
        | Datum::UnquoteSplicing(inner) => validate_ellipsis_item(inner, context),
        Datum::Atom(_) => Ok(()),
    }
}

fn validate_ellipsis_sequence(
    items: &[Spanned<Datum>],
    context: EllipsisContext,
) -> Result<(), SurfaceError> {
    for (index, item) in items.iter().enumerate() {
        if is_ellipsis(item) {
            if index == 0 || items.get(index - 1).is_some_and(is_ellipsis) {
                return Err(context.error(item.span.clone()));
            }
        } else {
            validate_ellipsis_item(item, context)?;
        }
    }

    Ok(())
}

fn syntax_rule_keyword(pattern: &Spanned<Datum>) -> Result<String, SurfaceError> {
    match &pattern.node {
        Datum::List(items) | Datum::DottedList(items, _) => {
            let Some(head) = items.first() else {
                return Err(SurfaceError::UnsupportedMacroPattern {
                    span: pattern.span.clone(),
                });
            };
            expect_identifier(head, "syntax-rules pattern").map(|name| name.node)
        }
        _ => Err(SurfaceError::UnsupportedMacroPattern {
            span: pattern.span.clone(),
        }),
    }
}

fn collect_unique_pattern_variables(
    pattern: &Spanned<Datum>,
    literals: &BTreeSet<String>,
    keyword: &str,
    seen: &mut BTreeSet<String>,
) -> Result<(), SurfaceError> {
    match &pattern.node {
        Datum::Atom(Atom::Identifier(name))
            if name != "..." && name != keyword && !literals.contains(name) =>
        {
            if !seen.insert(name.clone()) {
                return Err(SurfaceError::DuplicateIdentifier {
                    context: "syntax-rules pattern",
                    name: name.clone(),
                    span: pattern.span.clone(),
                });
            }
        }
        Datum::List(items) | Datum::Vector(items) => {
            for item in items {
                collect_unique_pattern_variables(item, literals, keyword, seen)?;
            }
        }
        Datum::DottedList(items, tail) => {
            for item in items {
                collect_unique_pattern_variables(item, literals, keyword, seen)?;
            }
            collect_unique_pattern_variables(tail, literals, keyword, seen)?;
        }
        Datum::Quote(inner)
        | Datum::Quasiquote(inner)
        | Datum::Unquote(inner)
        | Datum::UnquoteSplicing(inner) => {
            collect_unique_pattern_variables(inner, literals, keyword, seen)?;
        }
        Datum::Atom(_) => {}
    }

    Ok(())
}

fn apply_syntax_rules(
    name: &str,
    rules: &SyntaxRules,
    datum: &Spanned<Datum>,
) -> Result<Spanned<Datum>, SurfaceError> {
    for rule in &rules.rules {
        let mut captures = BTreeMap::new();
        if match_macro_pattern(&rule.pattern, datum, &rules.literals, name, &mut captures)? {
            let mut expanded = expand_template(&rule.template, &captures)?;
            expanded.span = datum.span.clone();
            return Ok(expanded);
        }
    }

    Err(SurfaceError::NoMatchingMacroRule {
        name: name.to_string(),
        span: datum.span.clone(),
    })
}

fn match_macro_pattern(
    pattern: &Spanned<Datum>,
    datum: &Spanned<Datum>,
    literals: &BTreeSet<String>,
    keyword: &str,
    captures: &mut BTreeMap<String, Capture>,
) -> Result<bool, SurfaceError> {
    match (&pattern.node, &datum.node) {
        (Datum::List(pattern_items), Datum::List(datum_items)) => {
            let Some(pattern_rest) = macro_pattern_rest(pattern, pattern_items, keyword)? else {
                return Ok(false);
            };
            let Some(datum_rest) = macro_datum_rest(datum_items, keyword) else {
                return Ok(false);
            };
            match_pattern_list(pattern_rest, datum_rest, literals, keyword, captures)
        }
        (Datum::DottedList(pattern_items, pattern_tail), Datum::List(datum_items)) => {
            let Some(pattern_rest) = macro_pattern_rest(pattern, pattern_items, keyword)? else {
                return Ok(false);
            };
            let Some(datum_rest) = macro_datum_rest(datum_items, keyword) else {
                return Ok(false);
            };
            match_pattern_list_with_tail(
                pattern_rest,
                pattern_tail,
                DottedDatum {
                    items: datum_rest,
                    tail: None,
                    original: datum,
                },
                literals,
                keyword,
                captures,
            )
        }
        (
            Datum::DottedList(pattern_items, pattern_tail),
            Datum::DottedList(datum_items, datum_tail),
        ) => {
            let Some(pattern_rest) = macro_pattern_rest(pattern, pattern_items, keyword)? else {
                return Ok(false);
            };
            let Some(datum_rest) = macro_datum_rest(datum_items, keyword) else {
                return Ok(false);
            };
            match_pattern_list_with_tail(
                pattern_rest,
                pattern_tail,
                DottedDatum {
                    items: datum_rest,
                    tail: Some(datum_tail.as_ref()),
                    original: datum,
                },
                literals,
                keyword,
                captures,
            )
        }
        _ => Ok(false),
    }
}

fn macro_pattern_rest<'a>(
    pattern: &Spanned<Datum>,
    items: &'a [Spanned<Datum>],
    keyword: &str,
) -> Result<Option<&'a [Spanned<Datum>]>, SurfaceError> {
    let Some((head, rest)) = items.split_first() else {
        return Err(SurfaceError::UnsupportedMacroPattern {
            span: pattern.span.clone(),
        });
    };

    Ok(is_keyword_head(head, keyword).then_some(rest))
}

fn macro_datum_rest<'a>(
    items: &'a [Spanned<Datum>],
    keyword: &str,
) -> Option<&'a [Spanned<Datum>]> {
    let (head, rest) = items.split_first()?;
    is_keyword_head(head, keyword).then_some(rest)
}

fn is_keyword_head(datum: &Spanned<Datum>, keyword: &str) -> bool {
    identifier_name(datum).as_deref() == Some(keyword)
}

fn match_pattern(
    pattern: &Spanned<Datum>,
    datum: &Spanned<Datum>,
    literals: &BTreeSet<String>,
    keyword: &str,
    captures: &mut BTreeMap<String, Capture>,
) -> Result<bool, SurfaceError> {
    match &pattern.node {
        Datum::Atom(Atom::Identifier(name)) if name == "..." => {
            Err(SurfaceError::UnsupportedMacroPattern {
                span: pattern.span.clone(),
            })
        }
        Datum::Atom(Atom::Identifier(name)) if name == keyword || literals.contains(name) => {
            Ok(identifier_name(datum).as_deref() == Some(name.as_str()))
        }
        Datum::Atom(Atom::Identifier(name)) => bind_capture(name, datum.clone(), captures),
        Datum::Atom(atom) => Ok(matches!(&datum.node, Datum::Atom(actual) if actual == atom)),
        Datum::List(pattern_items) => {
            let Datum::List(datum_items) = &datum.node else {
                return Ok(false);
            };
            match_pattern_list(pattern_items, datum_items, literals, keyword, captures)
        }
        Datum::Vector(pattern_items) => {
            let Datum::Vector(datum_items) = &datum.node else {
                return Ok(false);
            };
            match_pattern_list(pattern_items, datum_items, literals, keyword, captures)
        }
        Datum::Quote(pattern_inner) => {
            let Datum::Quote(datum_inner) = &datum.node else {
                return Ok(false);
            };
            match_pattern(pattern_inner, datum_inner, literals, keyword, captures)
        }
        Datum::Quasiquote(pattern_inner) => {
            let Datum::Quasiquote(datum_inner) = &datum.node else {
                return Ok(false);
            };
            match_pattern(pattern_inner, datum_inner, literals, keyword, captures)
        }
        Datum::Unquote(pattern_inner) => {
            let Datum::Unquote(datum_inner) = &datum.node else {
                return Ok(false);
            };
            match_pattern(pattern_inner, datum_inner, literals, keyword, captures)
        }
        Datum::UnquoteSplicing(pattern_inner) => {
            let Datum::UnquoteSplicing(datum_inner) = &datum.node else {
                return Ok(false);
            };
            match_pattern(pattern_inner, datum_inner, literals, keyword, captures)
        }
        Datum::DottedList(pattern_items, pattern_tail) => match_dotted_pattern_list(
            pattern_items,
            pattern_tail,
            datum,
            literals,
            keyword,
            captures,
        ),
    }
}

fn match_pattern_list(
    pattern_items: &[Spanned<Datum>],
    datum_items: &[Spanned<Datum>],
    literals: &BTreeSet<String>,
    keyword: &str,
    captures: &mut BTreeMap<String, Capture>,
) -> Result<bool, SurfaceError> {
    let mut pattern_index = 0;
    let mut datum_index = 0;

    while pattern_index < pattern_items.len() {
        let pattern = &pattern_items[pattern_index];
        let repeated = pattern_items
            .get(pattern_index + 1)
            .is_some_and(is_ellipsis);

        if repeated {
            let rest = &pattern_items[pattern_index + 2..];
            let minimum_rest = minimum_pattern_items(rest);
            if datum_items.len() < datum_index + minimum_rest {
                return Ok(false);
            }

            let repeat_count = datum_items.len() - datum_index - minimum_rest;
            seed_repeated_captures(pattern, literals, keyword, captures)?;
            for datum in &datum_items[datum_index..datum_index + repeat_count] {
                let mut local = BTreeMap::new();
                if !match_pattern(pattern, datum, literals, keyword, &mut local)? {
                    return Ok(false);
                }
                merge_repeated_captures(captures, local, pattern.span.clone())?;
            }
            datum_index += repeat_count;
            pattern_index += 2;
            continue;
        }

        let Some(datum) = datum_items.get(datum_index) else {
            return Ok(false);
        };
        if !match_pattern(pattern, datum, literals, keyword, captures)? {
            return Ok(false);
        }
        datum_index += 1;
        pattern_index += 1;
    }

    Ok(datum_index == datum_items.len())
}

fn match_dotted_pattern_list(
    pattern_items: &[Spanned<Datum>],
    pattern_tail: &Spanned<Datum>,
    datum: &Spanned<Datum>,
    literals: &BTreeSet<String>,
    keyword: &str,
    captures: &mut BTreeMap<String, Capture>,
) -> Result<bool, SurfaceError> {
    match &datum.node {
        Datum::List(datum_items) => match_pattern_list_with_tail(
            pattern_items,
            pattern_tail,
            DottedDatum {
                items: datum_items,
                tail: None,
                original: datum,
            },
            literals,
            keyword,
            captures,
        ),
        Datum::DottedList(datum_items, datum_tail) => match_pattern_list_with_tail(
            pattern_items,
            pattern_tail,
            DottedDatum {
                items: datum_items,
                tail: Some(datum_tail.as_ref()),
                original: datum,
            },
            literals,
            keyword,
            captures,
        ),
        _ => Ok(false),
    }
}

fn match_pattern_list_with_tail(
    pattern_items: &[Spanned<Datum>],
    pattern_tail: &Spanned<Datum>,
    datum: DottedDatum<'_>,
    literals: &BTreeSet<String>,
    keyword: &str,
    captures: &mut BTreeMap<String, Capture>,
) -> Result<bool, SurfaceError> {
    let mut pattern_index = 0;
    let mut datum_index = 0;

    while pattern_index < pattern_items.len() {
        let pattern = &pattern_items[pattern_index];
        let repeated = pattern_items
            .get(pattern_index + 1)
            .is_some_and(is_ellipsis);

        if repeated {
            let rest = &pattern_items[pattern_index + 2..];
            let minimum_rest = minimum_pattern_items(rest);
            if datum.items.len() < datum_index + minimum_rest {
                return Ok(false);
            }

            let repeat_count = datum.items.len() - datum_index - minimum_rest;
            seed_repeated_captures(pattern, literals, keyword, captures)?;
            for datum in &datum.items[datum_index..datum_index + repeat_count] {
                let mut local = BTreeMap::new();
                if !match_pattern(pattern, datum, literals, keyword, &mut local)? {
                    return Ok(false);
                }
                merge_repeated_captures(captures, local, pattern.span.clone())?;
            }
            datum_index += repeat_count;
            pattern_index += 2;
            continue;
        }

        let Some(datum) = datum.items.get(datum_index) else {
            return Ok(false);
        };
        if !match_pattern(pattern, datum, literals, keyword, captures)? {
            return Ok(false);
        }
        datum_index += 1;
        pattern_index += 1;
    }

    let tail = datum_tail_after(&datum, datum_index);
    match_pattern(pattern_tail, &tail, literals, keyword, captures)
}

fn datum_tail_after(datum: &DottedDatum<'_>, index: usize) -> Spanned<Datum> {
    if index < datum.items.len() {
        return Spanned {
            node: datum.tail.map_or_else(
                || Datum::List(datum.items[index..].to_vec()),
                |tail| Datum::DottedList(datum.items[index..].to_vec(), Box::new(tail.clone())),
            ),
            span: tail_span(
                &datum.items[index],
                datum.tail.unwrap_or(&datum.items[datum.items.len() - 1]),
            ),
            origin: datum.original.origin,
        };
    }

    datum.tail.cloned().unwrap_or_else(|| Spanned {
        node: Datum::List(Vec::new()),
        span: datum.original.span.clone(),
        origin: datum.original.origin,
    })
}

fn tail_span(first: &Spanned<Datum>, last: &Spanned<Datum>) -> SourceSpan {
    first.span.start..last.span.end
}

fn minimum_pattern_items(patterns: &[Spanned<Datum>]) -> usize {
    let mut count = 0;
    let mut index = 0;
    while index < patterns.len() {
        if patterns.get(index + 1).is_some_and(is_ellipsis) {
            index += 2;
        } else {
            count += 1;
            index += 1;
        }
    }
    count
}

fn seed_repeated_captures(
    pattern: &Spanned<Datum>,
    literals: &BTreeSet<String>,
    keyword: &str,
    captures: &mut BTreeMap<String, Capture>,
) -> Result<(), SurfaceError> {
    for name in pattern_variables(pattern, literals, keyword) {
        match captures.entry(name) {
            std::collections::btree_map::Entry::Vacant(entry) => {
                entry.insert(Capture::Repeated(Vec::new()));
            }
            std::collections::btree_map::Entry::Occupied(entry) => {
                if !matches!(entry.get(), Capture::Repeated(_)) {
                    return Err(SurfaceError::UnsupportedMacroPattern {
                        span: pattern.span.clone(),
                    });
                }
            }
        }
    }
    Ok(())
}

fn pattern_variables(
    pattern: &Spanned<Datum>,
    literals: &BTreeSet<String>,
    keyword: &str,
) -> BTreeSet<String> {
    let mut variables = BTreeSet::new();
    collect_pattern_variables(pattern, literals, keyword, &mut variables);
    variables
}

fn collect_pattern_variables(
    pattern: &Spanned<Datum>,
    literals: &BTreeSet<String>,
    keyword: &str,
    variables: &mut BTreeSet<String>,
) {
    match &pattern.node {
        Datum::Atom(Atom::Identifier(name))
            if name != "..." && name != keyword && !literals.contains(name) =>
        {
            variables.insert(name.clone());
        }
        Datum::List(items) | Datum::Vector(items) => {
            for item in items {
                collect_pattern_variables(item, literals, keyword, variables);
            }
        }
        Datum::DottedList(items, tail) => {
            for item in items {
                collect_pattern_variables(item, literals, keyword, variables);
            }
            collect_pattern_variables(tail, literals, keyword, variables);
        }
        Datum::Quote(inner)
        | Datum::Quasiquote(inner)
        | Datum::Unquote(inner)
        | Datum::UnquoteSplicing(inner) => {
            collect_pattern_variables(inner, literals, keyword, variables);
        }
        Datum::Atom(_) => {}
    }
}

fn bind_capture(
    name: &str,
    datum: Spanned<Datum>,
    captures: &mut BTreeMap<String, Capture>,
) -> Result<bool, SurfaceError> {
    match captures.get(name) {
        Some(Capture::Single(existing)) => Ok(existing.node == datum.node),
        Some(Capture::Repeated(_)) => Err(SurfaceError::UnsupportedMacroPattern {
            span: datum.span.clone(),
        }),
        None => {
            captures.insert(name.to_string(), Capture::Single(datum));
            Ok(true)
        }
    }
}

fn merge_repeated_captures(
    captures: &mut BTreeMap<String, Capture>,
    local: BTreeMap<String, Capture>,
    span: SourceSpan,
) -> Result<(), SurfaceError> {
    for (name, capture) in local {
        let Capture::Single(value) = capture else {
            return Err(SurfaceError::UnsupportedMacroPattern { span });
        };
        match captures.entry(name) {
            std::collections::btree_map::Entry::Vacant(entry) => {
                entry.insert(Capture::Repeated(vec![value]));
            }
            std::collections::btree_map::Entry::Occupied(mut entry) => {
                let Capture::Repeated(values) = entry.get_mut() else {
                    return Err(SurfaceError::UnsupportedMacroPattern { span });
                };
                values.push(value);
            }
        }
    }
    Ok(())
}

fn expand_template(
    template: &Spanned<Datum>,
    captures: &BTreeMap<String, Capture>,
) -> Result<Spanned<Datum>, SurfaceError> {
    expand_template_at(template, captures, None)
}

fn expand_template_at(
    template: &Spanned<Datum>,
    captures: &BTreeMap<String, Capture>,
    repetition: Option<usize>,
) -> Result<Spanned<Datum>, SurfaceError> {
    match &template.node {
        Datum::Atom(Atom::Identifier(name)) => match captures.get(name) {
            Some(Capture::Single(value)) => Ok(value.clone()),
            Some(Capture::Repeated(values)) => {
                let Some(index) = repetition else {
                    return Err(SurfaceError::InvalidMacroTemplate {
                        span: template.span.clone(),
                    });
                };
                values
                    .get(index)
                    .cloned()
                    .ok_or_else(|| SurfaceError::InvalidMacroTemplate {
                        span: template.span.clone(),
                    })
            }
            None => Ok(template.clone()),
        },
        Datum::List(items) => expand_template_list(items, captures, repetition)
            .map(|items| template.with_node(Datum::List(items))),
        Datum::Vector(items) => expand_template_list(items, captures, repetition)
            .map(|items| template.with_node(Datum::Vector(items))),
        Datum::DottedList(items, tail) => {
            let items = expand_template_list(items, captures, repetition)?;
            let tail = expand_template_at(tail, captures, repetition)?;
            Ok(template.with_node(Datum::DottedList(items, Box::new(tail))))
        }
        Datum::Quote(inner) => {
            let inner = expand_template_at(inner, captures, repetition)?;
            Ok(template.with_node(Datum::Quote(Box::new(inner))))
        }
        Datum::Quasiquote(inner) => {
            let inner = expand_template_at(inner, captures, repetition)?;
            Ok(template.with_node(Datum::Quasiquote(Box::new(inner))))
        }
        Datum::Unquote(inner) => {
            let inner = expand_template_at(inner, captures, repetition)?;
            Ok(template.with_node(Datum::Unquote(Box::new(inner))))
        }
        Datum::UnquoteSplicing(inner) => {
            let inner = expand_template_at(inner, captures, repetition)?;
            Ok(template.with_node(Datum::UnquoteSplicing(Box::new(inner))))
        }
        Datum::Atom(_) => Ok(template.clone()),
    }
}

fn expand_template_list(
    items: &[Spanned<Datum>],
    captures: &BTreeMap<String, Capture>,
    repetition: Option<usize>,
) -> Result<Vec<Spanned<Datum>>, SurfaceError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        let item = &items[index];
        if items.get(index + 1).is_some_and(is_ellipsis) {
            let count = repeated_template_count(item, captures)?;
            for repetition in 0..count {
                expanded.push(expand_template_at(item, captures, Some(repetition))?);
            }
            index += 2;
        } else {
            expanded.push(expand_template_at(item, captures, repetition)?);
            index += 1;
        }
    }

    Ok(expanded)
}

fn repeated_template_count(
    template: &Spanned<Datum>,
    captures: &BTreeMap<String, Capture>,
) -> Result<usize, SurfaceError> {
    let mut counts = BTreeSet::new();
    collect_repeated_template_counts(template, captures, &mut counts);

    match counts.len() {
        1 => Ok(*counts.iter().next().expect("one count is present")),
        _ => Err(SurfaceError::InvalidMacroTemplate {
            span: template.span.clone(),
        }),
    }
}

fn collect_repeated_template_counts(
    template: &Spanned<Datum>,
    captures: &BTreeMap<String, Capture>,
    counts: &mut BTreeSet<usize>,
) {
    match &template.node {
        Datum::Atom(Atom::Identifier(name)) => {
            if let Some(Capture::Repeated(values)) = captures.get(name) {
                counts.insert(values.len());
            }
        }
        Datum::List(items) | Datum::Vector(items) => {
            for item in items {
                collect_repeated_template_counts(item, captures, counts);
            }
        }
        Datum::DottedList(items, tail) => {
            for item in items {
                collect_repeated_template_counts(item, captures, counts);
            }
            collect_repeated_template_counts(tail, captures, counts);
        }
        Datum::Quote(inner)
        | Datum::Quasiquote(inner)
        | Datum::Unquote(inner)
        | Datum::UnquoteSplicing(inner) => {
            collect_repeated_template_counts(inner, captures, counts);
        }
        Datum::Atom(_) => {}
    }
}

fn is_ellipsis(datum: &Spanned<Datum>) -> bool {
    identifier_name(datum).as_deref() == Some("...")
}

fn is_quoted_list(items: &[Spanned<Datum>]) -> bool {
    items
        .first()
        .and_then(identifier_name)
        .is_some_and(|name| matches!(name.as_str(), "quote" | "quasiquote"))
}

fn is_definition_form(datum: &Spanned<Datum>) -> bool {
    let Datum::List(items) = &datum.node else {
        return false;
    };

    items
        .first()
        .and_then(identifier_name)
        .is_some_and(|name| matches!(name.as_str(), "define" | "define-syntax"))
}

fn binding_names(datum: &Spanned<Datum>) -> BTreeSet<String> {
    let Datum::List(bindings) = &datum.node else {
        return BTreeSet::new();
    };

    bindings
        .iter()
        .filter_map(|binding| {
            let Datum::List(parts) = &binding.node else {
                return None;
            };
            parts.first().and_then(identifier_name)
        })
        .collect()
}

fn top_level_begin_body(datum: &Spanned<Datum>) -> Option<&[Spanned<Datum>]> {
    if let Datum::List(items) = &datum.node
        && let Some((head, rest)) = items.split_first()
        && identifier_name(head).as_deref() == Some("begin")
    {
        return Some(rest);
    }

    None
}

pub fn classify_top_level(datum: &Spanned<Datum>) -> Result<Spanned<TopLevel>, SurfaceError> {
    if let Some((name, value)) = parse_define(datum)? {
        return Ok(datum.with_node(TopLevel::Define { name, value }));
    }

    Ok(classify_expr(datum)?.map(TopLevel::Expr))
}

pub fn classify_expr(datum: &Spanned<Datum>) -> Result<Spanned<Expr>, SurfaceError> {
    let expr = match &datum.node {
        Datum::Atom(Atom::Identifier(name)) => Expr::Variable(name.clone()),
        Datum::Atom(atom) => Expr::Literal(atom.clone()),
        Datum::Quote(inner) => Expr::Quote(inner.clone()),
        Datum::Quasiquote(inner) => Expr::Quasiquote(inner.clone()),
        Datum::List(items) => classify_list(datum.span.clone(), datum.origin, items)?,
        Datum::DottedList(_, _)
        | Datum::Vector(_)
        | Datum::Unquote(_)
        | Datum::UnquoteSplicing(_) => {
            return Err(SurfaceError::UnsupportedDatum {
                span: datum.span.clone(),
            });
        }
    };

    Ok(datum.with_node(expr))
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
        Some("quasiquote") => parse_quasiquote(span, rest),
        Some("define") => Err(SurfaceError::DefinitionContext {
            form: "define",
            span,
        }),
        Some("define-syntax") => Err(SurfaceError::DefinitionContext {
            form: "define-syntax",
            span,
        }),
        Some("lambda") => parse_lambda(span, origin, rest),
        Some("if") => parse_if(rest),
        Some("begin") => parse_begin(span, rest),
        Some("set!") => parse_set(rest),
        Some("delay") => parse_delay(span, rest),
        Some("let") => parse_let(span, origin, rest),
        Some("let*") => parse_let_star(span, origin, rest),
        Some("letrec") => parse_letrec(span, origin, rest),
        Some("and") => parse_and(span, origin, rest),
        Some("or") => parse_or(span, origin, rest),
        Some("cond") => parse_cond(span, origin, rest),
        Some("case") => parse_case(span, origin, rest),
        Some("do") => parse_do(span, origin, rest),
        _ => parse_apply(origin, head, rest),
    }
}

fn parse_define(datum: &Spanned<Datum>) -> Result<Option<DefineBinding>, SurfaceError> {
    let Datum::List(items) = &datum.node else {
        return Ok(None);
    };
    let Some((head, rest)) = items.split_first() else {
        return Ok(None);
    };
    if identifier_name(head).as_deref() != Some("define") {
        return Ok(None);
    }

    if rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "define",
            expected: "a name and value, or procedure shorthand and body",
            span: datum.span.clone(),
        });
    }

    match &rest[0].node {
        Datum::Atom(Atom::Identifier(_)) => {
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
        Datum::List(formals) => {
            let (name_datum, params, rest_param) =
                define_formals_from_list(formals, None, &rest[0])?;
            let name = expect_identifier(name_datum, "define procedure")?;
            let params = parse_required_formals(params, "define procedure formals")?;
            if let Some(rest_param) = &rest_param {
                ensure_distinct_extra_name(&params, rest_param, "define procedure formals")?;
            }
            let body = parse_body(&rest[1..], datum.span.clone(), datum.origin)?;
            let value = datum.with_node(Expr::Lambda {
                params,
                rest: rest_param,
                body,
            });

            Ok(Some((name, value)))
        }
        Datum::DottedList(formals, tail) => {
            let (name_datum, params, rest_param) =
                define_formals_from_list(formals, Some(tail.as_ref()), &rest[0])?;
            let name = expect_identifier(name_datum, "define procedure")?;
            let params = parse_required_formals(params, "define procedure formals")?;
            if let Some(rest_param) = &rest_param {
                ensure_distinct_extra_name(&params, rest_param, "define procedure formals")?;
            }
            let body = parse_body(&rest[1..], datum.span.clone(), datum.origin)?;
            let value = datum.with_node(Expr::Lambda {
                params,
                rest: rest_param,
                body,
            });

            Ok(Some((name, value)))
        }
        _ => Err(SurfaceError::ExpectedIdentifier {
            context: "define",
            span: rest[0].span.clone(),
        }),
    }
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

fn parse_quasiquote(span: SourceSpan, rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    if rest.len() != 1 {
        return Err(SurfaceError::BadArity {
            form: "quasiquote",
            expected: "one datum",
            span,
        });
    }

    Ok(Expr::Quasiquote(Box::new(rest[0].clone())))
}

fn parse_lambda(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        let span = rest.first().map(|item| item.span.clone()).unwrap_or(0..0);
        return Err(SurfaceError::BadArity {
            form: "lambda",
            expected: "formals and at least one body expression",
            span,
        });
    }

    let (params, rest_param) = parse_formals(&rest[0])?;
    let body = parse_body(&rest[1..], span, origin)?;

    Ok(Expr::Lambda {
        params,
        rest: rest_param,
        body,
    })
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

fn parse_begin(span: SourceSpan, rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    if rest.is_empty() {
        return Err(SurfaceError::BadArity {
            form: "begin",
            expected: "at least one expression",
            span,
        });
    }

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

fn parse_delay(span: SourceSpan, rest: &[Spanned<Datum>]) -> Result<Expr, SurfaceError> {
    if rest.len() != 1 {
        return Err(SurfaceError::BadArity {
            form: "delay",
            expected: "one expression",
            span,
        });
    }

    Ok(Expr::Delay(Box::new(classify_expr(&rest[0])?)))
}

fn parse_let(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "let",
            expected: "bindings and at least one body expression, or a name, bindings, and body",
            span,
        });
    }

    if identifier_name(&rest[0]).is_some() {
        return parse_named_let(span, origin, rest);
    }

    let bindings = parse_bindings(&rest[0], "let bindings")?;
    ensure_distinct_bindings(&bindings, "let bindings")?;
    let (params, operands): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    let body = parse_body(&rest[1..], span.clone(), origin)?;

    Ok(Expr::Apply {
        operator: Box::new(Spanned {
            node: Expr::Lambda {
                params,
                rest: None,
                body,
            },
            span: span.clone(),
            origin,
        }),
        operands,
    })
}

fn parse_named_let(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 3 {
        return Err(SurfaceError::BadArity {
            form: "named let",
            expected: "a name, bindings, and at least one body expression",
            span,
        });
    }

    let name = expect_identifier(&rest[0], "named let")?;
    let bindings = parse_bindings(&rest[1], "named let bindings")?;
    ensure_distinct_bindings(&bindings, "named let bindings")?;
    let mut params = Vec::new();
    let mut operands = Vec::new();
    for (param, operand) in bindings {
        params.push(param);
        operands.push(operand);
    }

    let lambda_body = parse_body(&rest[2..], span.clone(), origin)?;
    let call = Spanned {
        node: Expr::Apply {
            operator: Box::new(variable_expr(&name)),
            operands,
        },
        span: span.clone(),
        origin,
    };

    Ok(Expr::LetRec {
        bindings: vec![(
            name,
            Spanned {
                node: Expr::Lambda {
                    params,
                    rest: None,
                    body: lambda_body,
                },
                span: span.clone(),
                origin,
            },
        )],
        body: vec![call],
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
        node: body_sequence_expr(&rest[1..], span.clone(), origin)?,
        span: span.clone(),
        origin,
    };

    for (name, value) in bindings.into_iter().rev() {
        current = Spanned {
            node: Expr::Apply {
                operator: Box::new(Spanned {
                    node: Expr::Lambda {
                        params: vec![name],
                        rest: None,
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
) -> Result<Vec<Binding>, SurfaceError> {
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

fn ensure_distinct_bindings(
    bindings: &[Binding],
    context: &'static str,
) -> Result<(), SurfaceError> {
    let names = bindings
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<Vec<_>>();
    ensure_distinct_names(&names, context)
}

fn parse_letrec(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "letrec",
            expected: "bindings and at least one body expression",
            span,
        });
    }

    let bindings = parse_bindings(&rest[0], "letrec bindings")?;
    ensure_distinct_bindings(&bindings, "letrec bindings")?;

    Ok(Expr::LetRec {
        bindings,
        body: parse_body(&rest[1..], span.clone(), origin)?,
    })
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
            let consequent = spanned_expr(
                parse_and(span.clone(), origin, remaining)?,
                span.clone(),
                origin,
            );
            Ok(Expr::If {
                condition: Box::new(condition),
                consequent: Box::new(consequent),
                alternate: Some(Box::new(spanned_expr(boolean_literal(false), span, origin))),
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
                node: generated_name("or_value", &span),
                span: first.span.clone(),
                origin,
            };
            let condition = variable_expr(&temp);
            let alternate = spanned_expr(
                parse_or(span.clone(), origin, remaining)?,
                span.clone(),
                origin,
            );

            Ok(Expr::Apply {
                operator: Box::new(spanned_expr(
                    Expr::Lambda {
                        params: vec![temp.clone()],
                        rest: None,
                        body: vec![spanned_expr(
                            Expr::If {
                                condition: Box::new(condition),
                                consequent: Box::new(variable_expr(&temp)),
                                alternate: Some(Box::new(alternate)),
                            },
                            span.clone(),
                            origin,
                        )],
                    },
                    span.clone(),
                    origin,
                )),
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
    if clauses.is_empty() {
        return Err(SurfaceError::BadArity {
            form: "cond",
            expected: "at least one clause",
            span,
        });
    }

    let mut result = spanned_expr(boolean_literal(false), span.clone(), origin);

    for (index, clause) in clauses.iter().enumerate().rev() {
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
            if index != clauses.len() - 1 {
                return Err(SurfaceError::BadArity {
                    form: "cond",
                    expected: "else clause last",
                    span: clause.span.clone(),
                });
            }
            if body.is_empty() {
                return Err(SurfaceError::BadArity {
                    form: "cond else clause",
                    expected: "at least one body expression",
                    span: clause.span.clone(),
                });
            }

            result = spanned_expr(
                body_expr(body, clause.span.clone(), origin)?,
                clause.span.clone(),
                origin,
            );
            continue;
        }

        let condition = classify_expr(test)?;
        let arrow_recipient = body
            .first()
            .filter(|datum| identifier_name(datum).as_deref() == Some("=>"));
        if arrow_recipient.is_some() && body.len() != 2 {
            return Err(SurfaceError::BadArity {
                form: "cond => clause",
                expected: "a test, =>, and a receiver expression",
                span: clause.span.clone(),
            });
        }
        if body.is_empty() || arrow_recipient.is_some() {
            let temp = Spanned {
                node: generated_name("cond_value", &clause.span),
                span: test.span.clone(),
                origin,
            };
            let condition_value = variable_expr(&temp);
            let consequent = match arrow_recipient {
                Some(_) => spanned_expr(
                    Expr::Apply {
                        operator: Box::new(classify_expr(&body[1])?),
                        operands: vec![condition_value.clone()],
                    },
                    clause.span.clone(),
                    origin,
                ),
                None => condition_value.clone(),
            };
            let branch = spanned_expr(
                Expr::If {
                    condition: Box::new(condition_value),
                    consequent: Box::new(consequent),
                    alternate: Some(Box::new(result)),
                },
                clause.span.clone(),
                origin,
            );

            result = spanned_expr(
                Expr::Apply {
                    operator: Box::new(spanned_expr(
                        Expr::Lambda {
                            params: vec![temp],
                            rest: None,
                            body: vec![branch],
                        },
                        clause.span.clone(),
                        origin,
                    )),
                    operands: vec![condition],
                },
                clause.span.clone(),
                origin,
            );
            continue;
        }

        let consequent = spanned_expr(
            body_expr(body, clause.span.clone(), origin)?,
            clause.span.clone(),
            origin,
        );

        result = spanned_expr(
            Expr::If {
                condition: Box::new(condition),
                consequent: Box::new(consequent),
                alternate: Some(Box::new(result)),
            },
            clause.span.clone(),
            origin,
        );
    }

    Ok(result.node)
}

fn parse_case(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "case",
            expected: "a key expression and at least one clause",
            span,
        });
    }

    let key = classify_expr(&rest[0])?;
    let temp = Spanned {
        node: generated_name("case_key", &span),
        span: rest[0].span.clone(),
        origin,
    };
    let mut result = spanned_expr(boolean_literal(false), span.clone(), origin);

    for (index, clause) in rest[1..].iter().enumerate().rev() {
        let Datum::List(items) = &clause.node else {
            return Err(SurfaceError::ExpectedList {
                context: "case clause",
                span: clause.span.clone(),
            });
        };
        let Some((head, body)) = items.split_first() else {
            return Err(SurfaceError::BadArity {
                form: "case clause",
                expected: "datums and at least one body expression",
                span: clause.span.clone(),
            });
        };
        if body.is_empty() {
            return Err(SurfaceError::BadArity {
                form: "case clause",
                expected: "at least one body expression",
                span: clause.span.clone(),
            });
        }

        if identifier_name(head).as_deref() == Some("else") {
            if index != rest.len() - 2 {
                return Err(SurfaceError::BadArity {
                    form: "case",
                    expected: "else clause last",
                    span: clause.span.clone(),
                });
            }

            result = spanned_expr(
                body_expr(body, clause.span.clone(), origin)?,
                clause.span.clone(),
                origin,
            );
            continue;
        }

        let Datum::List(datums) = &head.node else {
            return Err(SurfaceError::ExpectedList {
                context: "case clause datums",
                span: head.span.clone(),
            });
        };

        let condition = case_datum_tests(&temp, datums, clause.span.clone(), origin);
        let consequent = spanned_expr(
            body_expr(body, clause.span.clone(), origin)?,
            clause.span.clone(),
            origin,
        );
        result = spanned_expr(
            Expr::If {
                condition: Box::new(condition),
                consequent: Box::new(consequent),
                alternate: Some(Box::new(result)),
            },
            clause.span.clone(),
            origin,
        );
    }

    Ok(Expr::Apply {
        operator: Box::new(spanned_expr(
            Expr::Lambda {
                params: vec![temp],
                rest: None,
                body: vec![result],
            },
            span.clone(),
            origin,
        )),
        operands: vec![key],
    })
}

fn case_datum_tests(
    key_name: &Spanned<String>,
    datums: &[Spanned<Datum>],
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
) -> Spanned<Expr> {
    datums.iter().rev().fold(
        spanned_expr(boolean_literal(false), span.clone(), origin),
        |alternate, datum| {
            let condition = spanned_expr(
                Expr::Apply {
                    operator: Box::new(spanned_expr(
                        Expr::Variable("eqv?".to_string()),
                        datum.span.clone(),
                        origin,
                    )),
                    operands: vec![
                        variable_expr(key_name),
                        datum.with_node(Expr::Quote(Box::new(datum.clone()))),
                    ],
                },
                datum.span.clone(),
                origin,
            );

            spanned_expr(
                Expr::If {
                    condition: Box::new(condition),
                    consequent: Box::new(spanned_expr(
                        boolean_literal(true),
                        datum.span.clone(),
                        origin,
                    )),
                    alternate: Some(Box::new(alternate)),
                },
                span.clone(),
                origin,
            )
        },
    )
}

fn parse_do(
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
    rest: &[Spanned<Datum>],
) -> Result<Expr, SurfaceError> {
    if rest.len() < 2 {
        return Err(SurfaceError::BadArity {
            form: "do",
            expected: "bindings, test clause, and optional body expressions",
            span,
        });
    }

    let bindings = parse_do_bindings(&rest[0])?;
    ensure_distinct_do_bindings(&bindings, "do bindings")?;
    let Datum::List(test_clause) = &rest[1].node else {
        return Err(SurfaceError::ExpectedList {
            context: "do test clause",
            span: rest[1].span.clone(),
        });
    };
    let Some((test, result_datums)) = test_clause.split_first() else {
        return Err(SurfaceError::BadArity {
            form: "do test clause",
            expected: "a test expression and optional result expressions",
            span: rest[1].span.clone(),
        });
    };

    let loop_name = Spanned {
        node: generated_name("do_loop", &span),
        span: span.clone(),
        origin,
    };
    let params = bindings
        .iter()
        .map(|binding| binding.name.clone())
        .collect::<Vec<_>>();
    let inits = bindings
        .iter()
        .map(|binding| binding.init.clone())
        .collect::<Vec<_>>();
    let steps = bindings
        .iter()
        .map(|binding| {
            binding
                .step
                .clone()
                .unwrap_or_else(|| variable_expr(&binding.name))
        })
        .collect::<Vec<_>>();

    let recursive_call = Spanned {
        node: Expr::Apply {
            operator: Box::new(variable_expr(&loop_name)),
            operands: steps,
        },
        span: span.clone(),
        origin,
    };
    let alternate = Spanned {
        node: sequence_with_tail(&rest[2..], recursive_call)?,
        span: span.clone(),
        origin,
    };
    let loop_body = Spanned {
        node: Expr::If {
            condition: Box::new(classify_expr(test)?),
            consequent: Box::new(sequence_expr(result_datums, rest[1].span.clone(), origin)?),
            alternate: Some(Box::new(alternate)),
        },
        span: span.clone(),
        origin,
    };
    let initial_call = Spanned {
        node: Expr::Apply {
            operator: Box::new(variable_expr(&loop_name)),
            operands: inits,
        },
        span: span.clone(),
        origin,
    };

    Ok(Expr::LetRec {
        bindings: vec![(
            loop_name,
            Spanned {
                node: Expr::Lambda {
                    params,
                    rest: None,
                    body: vec![loop_body],
                },
                span: span.clone(),
                origin,
            },
        )],
        body: vec![initial_call],
    })
}

#[derive(Debug, Clone)]
struct DoBinding {
    name: Spanned<String>,
    init: Spanned<Expr>,
    step: Option<Spanned<Expr>>,
}

fn parse_do_bindings(bindings: &Spanned<Datum>) -> Result<Vec<DoBinding>, SurfaceError> {
    let Datum::List(binding_datums) = &bindings.node else {
        return Err(SurfaceError::ExpectedList {
            context: "do bindings",
            span: bindings.span.clone(),
        });
    };

    binding_datums
        .iter()
        .map(|binding| {
            let Datum::List(spec) = &binding.node else {
                return Err(SurfaceError::ExpectedList {
                    context: "do binding",
                    span: binding.span.clone(),
                });
            };
            if !(2..=3).contains(&spec.len()) {
                return Err(SurfaceError::BadArity {
                    form: "do binding",
                    expected: "a name, init expression, and optional step expression",
                    span: binding.span.clone(),
                });
            }

            Ok(DoBinding {
                name: expect_identifier(&spec[0], "do binding")?,
                init: classify_expr(&spec[1])?,
                step: spec.get(2).map(classify_expr).transpose()?,
            })
        })
        .collect()
}

fn ensure_distinct_do_bindings(
    bindings: &[DoBinding],
    context: &'static str,
) -> Result<(), SurfaceError> {
    let names = bindings
        .iter()
        .map(|binding| binding.name.clone())
        .collect::<Vec<_>>();
    ensure_distinct_names(&names, context)
}

fn parse_body(
    body: &[Spanned<Datum>],
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
) -> Result<Vec<Spanned<Expr>>, SurfaceError> {
    let mut bindings = Vec::new();
    let mut index = 0;

    while index < body.len() {
        let Some(binding) = parse_define(&body[index])? else {
            break;
        };
        bindings.push(binding);
        index += 1;
    }

    if bindings.is_empty() {
        return body.iter().map(classify_expr).collect();
    }
    ensure_distinct_bindings(&bindings, "internal definitions")?;
    if index == body.len() {
        return Err(SurfaceError::BadArity {
            form: "body",
            expected: "at least one expression after internal definitions",
            span,
        });
    }

    let body = body[index..]
        .iter()
        .map(classify_expr)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(vec![Spanned {
        node: Expr::LetRec { bindings, body },
        span,
        origin,
    }])
}

fn body_sequence_expr(
    body: &[Spanned<Datum>],
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
) -> Result<Expr, SurfaceError> {
    let exprs = parse_body(body, span.clone(), origin)?;
    Ok(match exprs.as_slice() {
        [] => Expr::Begin(Vec::new()),
        [single] => single.node.clone(),
        _ => Expr::Begin(exprs),
    })
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
        Expr::Begin(exprs) if exprs.is_empty() => Expr::Begin(vec![spanned_expr(
            Expr::Literal(Atom::Boolean(false)),
            span,
            origin,
        )]),
        expr => expr,
    })
}

fn sequence_expr(
    body: &[Spanned<Datum>],
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
) -> Result<Spanned<Expr>, SurfaceError> {
    let node = match body {
        [] => Expr::Begin(Vec::new()),
        [single] => classify_expr(single)?.node,
        many => Expr::Begin(
            many.iter()
                .map(classify_expr)
                .collect::<Result<Vec<_>, _>>()?,
        ),
    };

    Ok(spanned_expr(node, span, origin))
}

fn sequence_with_tail(body: &[Spanned<Datum>], tail: Spanned<Expr>) -> Result<Expr, SurfaceError> {
    if body.is_empty() {
        return Ok(tail.node);
    }

    let mut exprs = body
        .iter()
        .map(classify_expr)
        .collect::<Result<Vec<_>, _>>()?;
    exprs.push(tail);
    Ok(Expr::Begin(exprs))
}

fn generated_name(prefix: &str, span: &SourceSpan) -> String {
    format!("#%lavu_{prefix}_{}", span.start)
}

fn variable_expr(name: &Spanned<String>) -> Spanned<Expr> {
    name.with_node(Expr::Variable(name.node.clone()))
}

fn spanned_expr(
    node: Expr,
    span: SourceSpan,
    origin: Option<crate::syntax::NodeId>,
) -> Spanned<Expr> {
    Spanned { node, span, origin }
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

fn parse_formals(formals: &Spanned<Datum>) -> Result<Formals, SurfaceError> {
    match &formals.node {
        Datum::Atom(Atom::Identifier(_)) => Ok((
            Vec::new(),
            Some(expect_identifier(formals, "lambda formals")?),
        )),
        Datum::List(items) => Ok((parse_required_formals(items, "lambda formals")?, None)),
        Datum::DottedList(items, tail) => {
            let params = parse_required_formals(items, "lambda formals")?;
            let rest = expect_identifier(tail, "lambda rest formal")?;
            ensure_distinct_extra_name(&params, &rest, "lambda formals")?;
            Ok((params, Some(rest)))
        }
        _ => Err(SurfaceError::ExpectedList {
            context: "lambda formals",
            span: formals.span.clone(),
        }),
    }
}

fn define_formals_from_list<'a>(
    formals: &'a [Spanned<Datum>],
    rest: Option<&'a Spanned<Datum>>,
    original: &Spanned<Datum>,
) -> Result<DefineFormals<'a>, SurfaceError> {
    let Some((name_datum, params)) = formals.split_first() else {
        return Err(SurfaceError::BadArity {
            form: "define",
            expected: "a procedure name",
            span: original.span.clone(),
        });
    };
    let rest = rest
        .map(|tail| expect_identifier(tail, "define procedure rest formal"))
        .transpose()?;
    Ok((name_datum, params, rest))
}

fn parse_required_formals(
    formals: &[Spanned<Datum>],
    context: &'static str,
) -> Result<Vec<Spanned<String>>, SurfaceError> {
    let formals = formals
        .iter()
        .map(|item| expect_identifier(item, context))
        .collect::<Result<Vec<_>, _>>()?;
    ensure_distinct_names(&formals, context)?;
    Ok(formals)
}

fn ensure_distinct_names(
    names: &[Spanned<String>],
    context: &'static str,
) -> Result<(), SurfaceError> {
    let mut seen = BTreeSet::new();
    for name in names {
        if !seen.insert(name.node.clone()) {
            return Err(SurfaceError::DuplicateIdentifier {
                context,
                name: name.node.clone(),
                span: name.span.clone(),
            });
        }
    }
    Ok(())
}

fn ensure_distinct_extra_name(
    names: &[Spanned<String>],
    name: &Spanned<String>,
    context: &'static str,
) -> Result<(), SurfaceError> {
    if names.iter().any(|existing| existing.node == name.node) {
        return Err(SurfaceError::DuplicateIdentifier {
            context,
            name: name.node.clone(),
            span: name.span.clone(),
        });
    }
    Ok(())
}

fn expect_identifier(
    datum: &Spanned<Datum>,
    context: &'static str,
) -> Result<Spanned<String>, SurfaceError> {
    if let Some(name) = identifier_name(datum) {
        Ok(datum.with_node(name))
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
    use crate::surface::{
        Expr, SurfaceContext, SurfaceError, TopLevel, classify_program, classify_top_level,
    };
    use crate::syntax::NodeId;

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
    fn keeps_surface_macros_across_programs() {
        let mut context = SurfaceContext::new();
        let definition = parse(
            "(define-syntax id
               (syntax-rules ()
                 ((id x) x)))",
        )
        .unwrap();
        let use_site = parse("(id 1)").unwrap();

        assert!(
            context
                .classify_program(&definition)
                .unwrap()
                .forms
                .is_empty()
        );
        let program = context.classify_program(&use_site).unwrap();

        assert!(matches!(
            program.forms[0].node,
            TopLevel::Expr(Expr::Literal(_))
        ));
    }

    #[test]
    fn classifies_define_procedure_shorthand() {
        let datums = parse("(define (add1 x) (+ x 1))").unwrap();
        let program = classify_program(&datums).unwrap();

        let TopLevel::Define { name, value } = &program.forms[0].node else {
            panic!("expected define");
        };

        assert_eq!(name.node, "add1");
        assert!(matches!(value.node, Expr::Lambda { .. }));
    }

    #[test]
    fn rejects_duplicate_binding_names() {
        for input in [
            "(lambda (x x) x)",
            "(lambda (x . x) x)",
            "(let ((x 1) (x 2)) x)",
            "(let loop ((x 1) (x 2)) x)",
            "(letrec ((x 1) (x 2)) x)",
            "(do ((x 0) (x 1)) (#t x))",
        ] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_top_level(&datums[0]),
                Err(SurfaceError::DuplicateIdentifier { .. })
            ));
        }

        let datums = parse("(define (f x x) x)").unwrap();
        assert!(matches!(
            classify_program(&datums),
            Err(SurfaceError::DuplicateIdentifier { .. })
        ));

        let datums = parse("(define (f x . x) x)").unwrap();
        assert!(matches!(
            classify_program(&datums),
            Err(SurfaceError::DuplicateIdentifier { .. })
        ));

        let datums = parse("(let* ((x 1) (x 2)) x)").unwrap();
        assert!(classify_top_level(&datums[0]).is_ok());
    }

    #[test]
    fn rejects_duplicate_internal_definition_names() {
        let datums = parse("(lambda () (define x 1) (define x 2) x)").unwrap();
        assert!(matches!(
            classify_top_level(&datums[0]),
            Err(SurfaceError::DuplicateIdentifier { .. })
        ));
    }

    #[test]
    fn rejects_duplicate_local_syntax_names() {
        for input in [
            "(let-syntax
               ((x (syntax-rules () ((x) 1)))
                (x (syntax-rules () ((x) 2))))
               (x))",
            "(letrec-syntax
               ((x (syntax-rules () ((x) 1)))
                (x (syntax-rules () ((x) 2))))
               (x))",
        ] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_program(&datums),
                Err(SurfaceError::DuplicateIdentifier { .. })
            ));
        }
    }

    #[test]
    fn rejects_duplicate_syntax_rule_literals() {
        let datums = parse(
            "(define-syntax pick
               (syntax-rules (else else)
                 ((pick else value) value)))",
        )
        .unwrap();

        assert!(matches!(
            classify_program(&datums),
            Err(SurfaceError::DuplicateIdentifier { .. })
        ));
    }

    #[test]
    fn rejects_reserved_syntax_rule_literals() {
        let datums = parse(
            "(define-syntax m
               (syntax-rules (...)
                 ((m x) x)))",
        )
        .unwrap();

        assert!(matches!(
            classify_program(&datums),
            Err(SurfaceError::ReservedIdentifier { .. })
        ));
    }

    #[test]
    fn macro_rule_head_must_match_keyword() {
        for input in [
            "(define-syntax m
               (syntax-rules ()
                 ((wrong x) x)))
             (m 1)",
            "(define-syntax m
               (syntax-rules ()
                 ((wrong) 1)))
             (m)",
        ] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_program(&datums),
                Err(SurfaceError::NoMatchingMacroRule { name, .. }) if name == "m"
            ));
        }
    }

    #[test]
    fn rejects_duplicate_syntax_rule_pattern_variables() {
        for input in [
            "(define-syntax m
               (syntax-rules ()
                 ((m x x) x)))",
            "(define-syntax m
               (syntax-rules ()
                 ((m (x) . x) x)))",
        ] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_program(&datums),
                Err(SurfaceError::DuplicateIdentifier { .. })
            ));
        }
    }

    #[test]
    fn rejects_bad_syntax_rule_ellipsis_patterns() {
        for input in [
            "(define-syntax m
               (syntax-rules ()
                 ((m ...) 1)))",
            "(define-syntax m
               (syntax-rules ()
                 ((m x ... ...) 1)))",
        ] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_program(&datums),
                Err(SurfaceError::UnsupportedMacroPattern { .. })
            ));
        }
    }

    #[test]
    fn rejects_bad_syntax_rule_ellipsis_templates() {
        for input in [
            "(define-syntax m
               (syntax-rules ()
                 ((m) (...))))",
            "(define-syntax m
               (syntax-rules ()
                 ((m x) (x ... ...))))",
            "(define-syntax m
               (syntax-rules ()
                 ((m) (a . ...))))",
        ] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_program(&datums),
                Err(SurfaceError::InvalidMacroTemplate { .. })
            ));
        }
    }

    #[test]
    fn rejects_non_final_cond_else_clause() {
        let datums = parse("(cond (else 1) (#t 2))").unwrap();

        assert!(matches!(
            classify_top_level(&datums[0]),
            Err(SurfaceError::BadArity { form: "cond", .. })
        ));
    }

    #[test]
    fn rejects_empty_cond_forms() {
        for input in ["(cond)", "(cond (else))"] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_top_level(&datums[0]),
                Err(SurfaceError::BadArity { .. })
            ));
        }

        let datums = parse("(cond (#t))").unwrap();
        assert!(classify_top_level(&datums[0]).is_ok());
    }

    #[test]
    fn rejects_empty_begin_expression() {
        let datums = parse("(begin)").unwrap();

        assert!(matches!(
            classify_top_level(&datums[0]),
            Err(SurfaceError::BadArity { form: "begin", .. })
        ));
    }

    #[test]
    fn rejects_definitions_in_expression_position() {
        for input in [
            "(+ (define x 1) 2)",
            "(lambda () 1 (define x 2) x)",
            "(+ (define-syntax id (syntax-rules () ((id x) x))) 1)",
        ] {
            let datums = parse(input).unwrap();
            assert!(matches!(
                classify_top_level(&datums[0]),
                Err(SurfaceError::DefinitionContext { .. })
            ));
        }
    }

    #[test]
    fn splices_top_level_begin_forms() {
        let datums = parse("(begin (define x 1) x)").unwrap();
        let program = classify_program(&datums).unwrap();

        assert_eq!(program.forms.len(), 2);
        assert!(matches!(program.forms[0].node, TopLevel::Define { .. }));
        assert!(matches!(program.forms[1].node, TopLevel::Expr(_)));
    }

    #[test]
    fn classifies_rest_lambda_formals() {
        let datums = parse("(lambda (x . rest) rest)").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        let TopLevel::Expr(Expr::Lambda { params, rest, .. }) = form.node else {
            panic!("expected lambda");
        };

        assert_eq!(params[0].node, "x");
        assert_eq!(rest.unwrap().node, "rest");
    }

    #[test]
    fn classifies_rest_define_shorthand() {
        let datums = parse("(define (collect x . rest) rest)").unwrap();
        let program = classify_program(&datums).unwrap();

        let TopLevel::Define { name, value } = &program.forms[0].node else {
            panic!("expected define");
        };
        let Expr::Lambda { params, rest, .. } = &value.node else {
            panic!("expected lambda");
        };

        assert_eq!(name.node, "collect");
        assert_eq!(params[0].node, "x");
        assert_eq!(rest.as_ref().unwrap().node, "rest");
    }

    #[test]
    fn lowers_internal_definitions_to_letrec() {
        let datums = parse("(lambda () (define x 1) x)").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        let TopLevel::Expr(Expr::Lambda { body, .. }) = form.node else {
            panic!("expected lambda");
        };

        assert!(matches!(body[0].node, Expr::LetRec { .. }));
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
    fn classifies_quasiquote_payload_as_datum() {
        let datums = parse("`(a ,b)").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        let TopLevel::Expr(Expr::Quasiquote(payload)) = form.node else {
            panic!("expected quasiquote");
        };

        assert_eq!(payload.span, 1..7);
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
    fn classifies_letrec_as_recursive_binding_form() {
        let datums = parse("(letrec ((f (lambda (x) x))) (f 1))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::LetRec { .. })));
    }

    #[test]
    fn preserves_letrec_body_origin_through_internal_definition_lowering() {
        let datum = parse("(letrec ((f (lambda () x))) (define x 1) (f))")
            .unwrap()
            .remove(0)
            .with_origin(NodeId(9));
        let form = classify_top_level(&datum).unwrap();

        let TopLevel::Expr(Expr::LetRec { body, .. }) = form.node else {
            panic!("expected letrec");
        };

        assert_eq!(body[0].origin, Some(NodeId(9)));
    }

    #[test]
    fn lowers_named_let_to_recursive_binding_form() {
        let datums = parse("(let loop ((n 1)) (loop n))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::LetRec { .. })));
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

    #[test]
    fn desugars_cond_arrow_to_single_value_application() {
        let datums = parse("(cond ((number? x) => f) (else 0))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::Apply { .. })));
    }

    #[test]
    fn desugars_case_to_single_key_application() {
        let datums = parse("(case x ((a b) 1) (else 2))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::Apply { .. })));
    }

    #[test]
    fn lowers_do_to_recursive_binding_form() {
        let datums = parse("(do ((i 0 (+ i 1))) ((= i 3) i))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::LetRec { .. })));
    }

    #[test]
    fn classifies_delay_as_special_form() {
        let datums = parse("(delay (+ 1 2))").unwrap();
        let form = classify_top_level(&datums[0]).unwrap();

        assert!(matches!(form.node, TopLevel::Expr(Expr::Delay(_))));
    }
}
