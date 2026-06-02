//! Shared Scheme syntax highlighting.
//!
//! A single tokenizer pass and one [`Category`] palette feed every place
//! Lavu shows colored code: the Reedline line editor, the `?` query echo,
//! and the source rendered inside Ariadne diagnostics. Keeping them on one
//! palette means the REPL stays visually in line with itself.

use std::ops::Range;

use logos::Logos;
use nu_ansi_term::{Color, Style};

use crate::lexer::{
    Token, is_conversion, is_keywordy, is_mutator, is_operator, is_predicate, is_special_form,
};
use crate::types::{ProcedureType, Type};

static BOOLEAN_RESULT: Type = Type::Boolean;

/// One syntactic role a source token can play, independent of how it is
/// eventually colored for a given output sink.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Category {
    SpecialForm,
    Keyword,
    Predicate,
    Mutator,
    Conversion,
    Operator,
    Callable,
    Number,
    Str,
    Char,
    Boolean,
    Delimiter,
    Quote,
    Vector,
    Comment,
    Plain,
}

impl Category {
    /// Terminal style used by Reedline and the `?` query echo.
    pub fn style(self) -> Style {
        match self {
            Category::SpecialForm => Style::new().fg(Color::Green),
            Category::Keyword => Style::new().fg(Color::Purple),
            Category::Predicate => Style::new().fg(Color::LightBlue),
            Category::Mutator => Style::new().fg(Color::Red),
            Category::Conversion => Style::new().fg(Color::Yellow),
            Category::Operator => Style::new().fg(Color::LightRed),
            Category::Callable => Style::new().fg(Color::LightCyan),
            Category::Number => Style::new().fg(Color::Green),
            Category::Str => Style::new().fg(Color::LightRed),
            Category::Char => Style::new().fg(Color::LightCyan).italic(),
            Category::Boolean => Style::new().fg(Color::LightGreen).bold(),
            Category::Delimiter => Style::new().fg(Color::Purple),
            Category::Quote => Style::new().fg(Color::Magenta).bold(),
            Category::Vector => Style::new().fg(Color::Purple).bold(),
            Category::Comment => Style::new().fg(Color::DarkGray).italic(),
            Category::Plain => Style::new().fg(Color::Default),
        }
    }

    /// Ariadne (yansi) color for tinting source text, or `None` to leave
    /// the token in Ariadne's default unimportant color.
    pub fn ariadne_color(self) -> Option<ariadne::Color> {
        use ariadne::Color;

        let color = match self {
            Category::SpecialForm => Color::Green,
            Category::Keyword => Color::Magenta,
            Category::Predicate => Color::BrightBlue,
            Category::Mutator => Color::Red,
            Category::Conversion => Color::Yellow,
            Category::Operator => Color::BrightRed,
            Category::Callable => Color::BrightCyan,
            Category::Number => Color::Green,
            Category::Str => Color::BrightRed,
            Category::Char => Color::BrightCyan,
            Category::Boolean => Color::BrightGreen,
            Category::Delimiter => Color::Magenta,
            Category::Quote => Color::BrightMagenta,
            Category::Vector => Color::BrightMagenta,
            Category::Comment => Color::BrightBlack,
            Category::Plain => return None,
        };
        Some(color)
    }
}

/// A categorized token: its role, byte span, and the matching source slice.
pub struct Segment<'a> {
    pub category: Category,
    pub span: Range<usize>,
    pub text: &'a str,
}

/// Tokenize `line` and assign a [`Category`] to every token in order. The
/// segments cover the whole input contiguously, so callers can reconstruct
/// `line` exactly by concatenating their text.
pub fn segments(line: &str) -> Vec<Segment<'_>> {
    let mut lexer = Token::lexer(line);
    let mut previous: Option<Token> = None;
    let mut segments = Vec::new();

    while let Some(result) = lexer.next() {
        let span = lexer.span();
        let text = &line[span.clone()];
        let category = match &result {
            Ok(token) => categorize(token, text, previous.as_ref()),
            Err(_) => Category::Plain,
        };
        segments.push(Segment {
            category,
            span,
            text,
        });

        // Track the last meaningful token so an identifier can tell whether
        // it sits in the operator position right after an opening paren.
        if let Ok(token) = result
            && !matches!(token, Token::Whitespace(_))
        {
            previous = Some(token);
        }
    }

    segments
}

/// Render a Scheme source slice with ANSI styling for terminal echo.
pub fn paint_source(line: &str) -> String {
    segments(line)
        .iter()
        .map(|segment| segment.category.style().paint(segment.text).to_string())
        .collect()
}

fn categorize(token: &Token, text: &str, previous: Option<&Token>) -> Category {
    match token {
        Token::Identifier(_) => identifier_category(text, previous),
        Token::Integer(_)
        | Token::Decimal(_)
        | Token::Real(_)
        | Token::ExactComplex(_)
        | Token::Complex(_)
        | Token::Binary(_)
        | Token::Octal(_)
        | Token::Hex(_) => Category::Number,
        Token::String(_) => Category::Str,
        Token::Character(_) => Category::Char,
        Token::True | Token::False => Category::Boolean,
        Token::LParen | Token::RParen | Token::LBracket | Token::RBracket => Category::Delimiter,
        Token::Quote
        | Token::Backquote
        | Token::Unquote
        | Token::UnquoteSplicing
        | Token::SyntaxQuote => Category::Quote,
        Token::VectorStart => Category::Vector,
        Token::LineComment | Token::BlockComment => Category::Comment,
        _ => Category::Plain,
    }
}

fn identifier_category(text: &str, previous: Option<&Token>) -> Category {
    match text {
        x if is_special_form(x) => Category::SpecialForm,
        x if is_keywordy(x) => Category::Keyword,
        x if is_predicate(x) => Category::Predicate,
        x if is_mutator(x) => Category::Mutator,
        x if is_conversion(x) => Category::Conversion,
        x if is_operator(x) => Category::Operator,
        _ if matches!(previous, Some(Token::LParen)) => Category::Callable,
        _ => Category::Plain,
    }
}

/// Render an inferred [`Type`] with ANSI styling. Predicate-shaped atoms
/// reuse the predicate color from source highlighting; procedure types are
/// given extra structure so the arrow, parameters, result, and variadic
/// markers each read distinctly.
pub fn paint_type(ty: &Type) -> String {
    match ty {
        Type::Any => atom("any?"),
        Type::Unknown => atom("unknown?"),
        Type::Never => atom("never?"),
        Type::False => Category::Boolean.style().paint("#f").to_string(),
        Type::Boolean => atom("boolean?"),
        Type::Number => atom("number?"),
        Type::Char => atom("char?"),
        Type::String => atom("string?"),
        Type::Symbol => atom("symbol?"),
        Type::Null => atom("null?"),
        Type::List => atom("list?"),
        Type::Vector => atom("vector?"),
        Type::Port => atom("port?"),
        Type::InputPort => atom("input-port?"),
        Type::OutputPort => atom("output-port?"),
        Type::EofObject => atom("eof-object?"),
        Type::Unspecified => atom("unspecified?"),
        Type::Var(name) => var(name),
        Type::Pair(car, cdr) => form(atom("pair?"), [paint_type(car), paint_type(cdr)]),
        Type::ListOf(element) => form(ctor("listof"), [paint_type(element)]),
        Type::VectorOf(element) => form(ctor("vectorof"), [paint_type(element)]),
        Type::PromiseOf(element) => form(ctor("promiseof"), [paint_type(element)]),
        Type::Values(types) => form(former("values"), types.iter().map(paint_type)),
        Type::Union(types) => form(union("U"), types.iter().map(paint_type)),
        Type::Procedure(procedure) => paint_procedure(procedure),
    }
}

fn paint_procedure(procedure: &ProcedureType) -> String {
    match procedure {
        ProcedureType::Fixed { params, result } => {
            let parts = params.iter().map(paint_type).chain([paint_type(result)]);
            form(arrow("->"), parts)
        }
        ProcedureType::Optional {
            required,
            optional,
            result,
        } => {
            let parts = required
                .iter()
                .chain(optional)
                .map(paint_type)
                .chain([marker("?"), paint_type(result)]);
            form(arrow("->"), parts)
        }
        ProcedureType::UniformVariadic { param, result } => {
            form(arrow("->*"), [paint_type(param), paint_type(result)])
        }
        ProcedureType::Rest {
            required,
            rest,
            result,
        } => {
            let parts = required.iter().map(paint_type).chain([
                paint_type(rest),
                marker("*"),
                paint_type(result),
            ]);
            form(arrow("->"), parts)
        }
        ProcedureType::Predicate { param, positive } => form(
            arrow("->"),
            [
                paint_type(param),
                paint_type(&Type::Boolean),
                marker(":"),
                paint_type(positive),
            ],
        ),
    }
}

/// Render a `? expr` result as `value : type`. A procedure type expands into
/// a multi-line block listing each parameter with its inferred type and the
/// result on its own line, so functions and lambdas read clearly. `names`
/// supplies parameter names from a queried lambda when known; otherwise each
/// slot falls back to a positional label.
pub fn paint_query(source: &str, ty: &Type, names: &[String]) -> String {
    let value = paint_source(source);
    match ty {
        Type::Procedure(procedure) => paint_procedure_block(&value, procedure, names),
        _ => format!("{value} {} {}", sep(), paint_type(ty)),
    }
}

/// Render a `? expr` result without ANSI styling, preserving the same
/// procedure block shape used by [`paint_query`] for terminal output.
pub fn format_query(value: &str, ty: &Type, names: &[String]) -> String {
    match ty {
        Type::Procedure(procedure) => format_procedure_block(value, procedure, names),
        _ => format!("{value} : {ty}"),
    }
}

/// One argument position of a procedure type, tagged with how it is supplied.
enum Slot<'a> {
    Param(&'a Type),
    Optional(&'a Type),
    Variadic(&'a Type),
}

impl Slot<'_> {
    fn ty(&self) -> &Type {
        match self {
            Slot::Param(ty) | Slot::Optional(ty) | Slot::Variadic(ty) => ty,
        }
    }

    /// Glyph appended to the parameter name: optional and variadic positions
    /// echo the `?` and `*` markers used by the inline type display.
    fn suffix(&self) -> &'static str {
        match self {
            Slot::Param(_) => "",
            Slot::Optional(_) => "?",
            Slot::Variadic(_) => "…",
        }
    }
}

fn procedure_slots(procedure: &ProcedureType) -> (Vec<Slot<'_>>, &Type) {
    match procedure {
        ProcedureType::Fixed { params, result } => {
            (params.iter().map(Slot::Param).collect(), result)
        }
        ProcedureType::Optional {
            required,
            optional,
            result,
        } => {
            let slots = required
                .iter()
                .map(Slot::Param)
                .chain(optional.iter().map(Slot::Optional))
                .collect();
            (slots, result)
        }
        ProcedureType::UniformVariadic { param, result } => (vec![Slot::Variadic(param)], result),
        ProcedureType::Rest {
            required,
            rest,
            result,
        } => {
            let slots = required
                .iter()
                .map(Slot::Param)
                .chain([Slot::Variadic(rest)])
                .collect();
            (slots, result)
        }
        ProcedureType::Predicate { param, .. } => (vec![Slot::Param(param)], &BOOLEAN_RESULT),
    }
}

fn paint_procedure_block(value: &str, procedure: &ProcedureType, names: &[String]) -> String {
    let (slots, result) = procedure_slots(procedure);

    // Resolve each slot's label first so the `:` columns align on visible text.
    let labels = procedure_labels(&slots, names);
    let width = max_label_width(&labels);

    let mut out = format!("{value} {}\n", sep());
    for (slot, (name, suffix)) in slots.iter().zip(&labels) {
        let pad = " ".repeat(width - label_width(name, suffix));
        out.push_str(&format!(
            "    {}{pad} {} {}\n",
            paint_label(name, suffix),
            sep(),
            paint_type(slot.ty()),
        ));
    }
    out.push_str(&paint_procedure_result(procedure, result));
    out
}

fn format_procedure_block(value: &str, procedure: &ProcedureType, names: &[String]) -> String {
    let (slots, result) = procedure_slots(procedure);
    let labels = procedure_labels(&slots, names);
    let width = max_label_width(&labels);

    let mut out = format!("{value} :\n");
    for (slot, (name, suffix)) in slots.iter().zip(&labels) {
        let pad = " ".repeat(width - label_width(name, suffix));
        out.push_str(&format!(
            "    {}{pad} : {}\n",
            format_label(name, suffix),
            slot.ty(),
        ));
    }
    out.push_str(&format_procedure_result(procedure, result));
    out
}

fn paint_procedure_result(procedure: &ProcedureType, result: &Type) -> String {
    match procedure {
        ProcedureType::Predicate { positive, .. } => {
            format!(
                "{} {} {} {}",
                arrow("->"),
                paint_type(result),
                marker(":"),
                paint_type(positive)
            )
        }
        _ => format!("{} {}", arrow("->"), paint_type(result)),
    }
}

fn format_procedure_result(procedure: &ProcedureType, result: &Type) -> String {
    match procedure {
        ProcedureType::Predicate { positive, .. } => {
            format!("-> {result} : {positive}")
        }
        _ => format!("-> {result}"),
    }
}

fn procedure_labels(slots: &[Slot<'_>], names: &[String]) -> Vec<(String, &'static str)> {
    slots
        .iter()
        .enumerate()
        .map(|(index, slot)| {
            let name = names
                .get(index)
                .cloned()
                .unwrap_or_else(|| format!("arg {}", index + 1));
            (name, slot.suffix())
        })
        .collect()
}

fn max_label_width(labels: &[(String, &'static str)]) -> usize {
    labels
        .iter()
        .map(|(name, suffix)| label_width(name, suffix))
        .max()
        .unwrap_or(0)
}

/// Visible width of a parameter label, counting the ` ?`/` …` marker.
fn label_width(name: &str, suffix: &str) -> usize {
    name.chars().count() + if suffix.is_empty() { 0 } else { 2 }
}

fn paint_label(name: &str, suffix: &str) -> String {
    let name = Style::new().bold().paint(name).to_string();
    if suffix.is_empty() {
        name
    } else {
        format!("{name} {}", marker(suffix))
    }
}

fn format_label(name: &str, suffix: &str) -> String {
    if suffix.is_empty() {
        name.to_string()
    } else {
        format!("{name} {suffix}")
    }
}

fn sep() -> String {
    Color::DarkGray.paint(":").to_string()
}

/// Wrap a colored head and its operands in dimmed parentheses, matching the
/// `(head operand ...)` shape of the plain [`Type`] display.
fn form(head: String, operands: impl IntoIterator<Item = String>) -> String {
    let mut out = paren("(");
    out.push_str(&head);
    for operand in operands {
        out.push(' ');
        out.push_str(&operand);
    }
    out.push_str(&paren(")"));
    out
}

fn atom(name: &str) -> String {
    Color::LightBlue.paint(name).to_string()
}

fn var(name: &str) -> String {
    Color::Yellow.italic().paint(name).to_string()
}

fn ctor(name: &str) -> String {
    Color::Green.paint(name).to_string()
}

fn arrow(name: &str) -> String {
    Color::LightCyan.bold().paint(name).to_string()
}

fn union(name: &str) -> String {
    Color::Purple.bold().paint(name).to_string()
}

fn former(name: &str) -> String {
    Color::Purple.paint(name).to_string()
}

fn marker(name: &str) -> String {
    Color::LightRed.bold().paint(name).to_string()
}

fn paren(text: &str) -> String {
    Color::DarkGray.paint(text).to_string()
}

#[cfg(test)]
mod tests {
    use super::{Category, format_query, paint_query, paint_type, segments};
    use crate::types::{ProcedureType, Type};

    fn categories(line: &str) -> Vec<Category> {
        segments(line)
            .into_iter()
            .filter(|segment| !segment.text.trim().is_empty())
            .map(|segment| segment.category)
            .collect()
    }

    #[test]
    fn segments_reconstruct_the_whole_line() {
        let line = "(+ 1 \"x\") ; note";
        let rebuilt: String = segments(line).iter().map(|segment| segment.text).collect();
        assert_eq!(rebuilt, line);
    }

    #[test]
    fn categorizes_core_token_roles() {
        // Any identifier directly after `(` reads as the operator position,
        // matching the long-standing Reedline highlighter, so the bound `x`
        // in `(x)` is Callable while the trailing argument `x` is Plain.
        assert_eq!(
            categories("(lambda (x) (string? x))"),
            vec![
                Category::Delimiter,
                Category::SpecialForm,
                Category::Delimiter,
                Category::Callable,
                Category::Delimiter,
                Category::Delimiter,
                Category::Predicate,
                Category::Plain,
                Category::Delimiter,
                Category::Delimiter,
            ]
        );
    }

    #[test]
    fn callable_only_in_operator_position() {
        // `map` right after `(` is callable; the later `map` argument is not.
        assert_eq!(
            categories("(map map xs)"),
            vec![
                Category::Delimiter,
                Category::Callable,
                Category::Plain,
                Category::Plain,
                Category::Delimiter,
            ]
        );
    }

    #[test]
    fn paints_types_without_disturbing_their_text() {
        // Stripping the ANSI escapes must leave the plain display untouched.
        let procedure = Type::Procedure(ProcedureType::Fixed {
            params: vec![Type::Number, Type::String],
            result: Box::new(Type::Boolean),
        });
        assert_eq!(strip(&paint_type(&procedure)), procedure.to_string());

        let predicate = Type::predicate_procedure(Type::Any, Type::String);
        assert_eq!(strip(&paint_type(&predicate)), predicate.to_string());

        let polymorphic = Type::ListOf(Box::new(Type::Var("a".to_string())));
        assert_eq!(strip(&paint_type(&polymorphic)), polymorphic.to_string());

        let union = Type::union(vec![Type::Number, Type::String]);
        assert_eq!(strip(&paint_type(&union)), union.to_string());
    }

    #[test]
    fn paints_a_procedure_query_as_a_param_block() {
        // Named lambda formals label each parameter; the result sits alone.
        let procedure = Type::Procedure(ProcedureType::Fixed {
            params: vec![Type::Number, Type::String],
            result: Box::new(Type::Boolean),
        });
        let names = ["n".to_string(), "acc".to_string()];
        assert_eq!(
            strip(&paint_query("f", &procedure, &names)),
            "f :\n    n   : number?\n    acc : string?\n-> boolean?"
        );
    }

    #[test]
    fn formats_a_plain_procedure_query_as_a_param_block() {
        let procedure = Type::Procedure(ProcedureType::Fixed {
            params: vec![Type::Number],
            result: Box::new(Type::Number),
        });
        let names = ["n".to_string()];

        assert_eq!(
            format_query("#<procedure>", &procedure, &names),
            "#<procedure> :\n    n : number?\n-> number?"
        );
    }

    #[test]
    fn formats_predicate_procedure_queries_with_latent_results() {
        let procedure = Type::predicate_procedure(Type::Any, Type::String);

        assert_eq!(
            strip(&paint_query("#<procedure>", &procedure, &[])),
            "#<procedure> :\n    arg 1 : any?\n-> boolean? : string?"
        );
        assert_eq!(
            format_query("#<procedure>", &procedure, &[]),
            "#<procedure> :\n    arg 1 : any?\n-> boolean? : string?"
        );
    }

    #[test]
    fn falls_back_to_positional_labels_and_variadic_markers() {
        // Without source names, slots are positional; a rest slot is marked.
        let procedure = Type::rest_procedure(vec![Type::Number], Type::String, Type::Boolean);
        assert_eq!(
            strip(&paint_query("g", &procedure, &[])),
            "g :\n    arg 1   : number?\n    arg 2 … : string?\n-> boolean?"
        );
    }

    #[test]
    fn non_procedure_queries_stay_on_one_line() {
        assert_eq!(strip(&paint_query("1", &Type::Number, &[])), "1 : number?");
    }

    fn strip(painted: &str) -> String {
        let mut out = String::new();
        let mut chars = painted.chars();
        while let Some(c) = chars.next() {
            if c == '\u{1b}' {
                for c in chars.by_ref() {
                    if c == 'm' {
                        break;
                    }
                }
            } else {
                out.push(c);
            }
        }
        out
    }
}
