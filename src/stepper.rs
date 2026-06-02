//! Interactive `?? expr` inference stepper.
//!
//! A long inference produces dozens of steps; rendered together as Ariadne
//! labels they overlap into a wall. Instead this walks the recorded steps one
//! at a time in the alternate screen, each shown as an Ariadne report whose
//! caret points at the current subexpression: `←` / `→` move between steps and
//! `space` / `enter` / `esc` leave the mode, after which the caller prints the
//! final type the way `? expr` does.

use std::io::{self, Write};

use crossterm::cursor::{Hide, MoveTo, Show};
use crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers};
use crossterm::terminal::{
    Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use crossterm::{execute, queue};

use crate::diagnostics::render_inference_step;
use crate::highlight::paint_type;
use crate::infer::TraceStep;
use crate::syntax::SourceSpan;

/// Step through `steps` against `source` until the user leaves the mode.
pub fn run(source: &str, steps: &[TraceStep]) -> io::Result<()> {
    if steps.is_empty() {
        return Ok(());
    }

    let _screen = AltScreen::enter()?;
    let mut out = io::stdout();
    let mut index = 0usize;

    loop {
        render(&mut out, source, steps, index)?;
        match read_action()? {
            Action::Prev => index = index.saturating_sub(1),
            Action::Next => index = (index + 1).min(steps.len() - 1),
            Action::First => index = 0,
            Action::Last => index = steps.len() - 1,
            Action::Quit => break,
        }
    }

    Ok(())
}

enum Action {
    Prev,
    Next,
    First,
    Last,
    Quit,
}

fn read_action() -> io::Result<Action> {
    loop {
        let Event::Key(key) = event::read()? else {
            continue;
        };
        // Some terminals emit a release event for each press; ignore those.
        if key.kind == KeyEventKind::Release {
            continue;
        }
        if key.code == KeyCode::Char('c') && key.modifiers.contains(KeyModifiers::CONTROL) {
            return Ok(Action::Quit);
        }
        return Ok(match key.code {
            KeyCode::Left => Action::Prev,
            KeyCode::Right => Action::Next,
            KeyCode::Home => Action::First,
            KeyCode::End => Action::Last,
            KeyCode::Char(' ') | KeyCode::Enter | KeyCode::Esc | KeyCode::Char('q') => Action::Quit,
            _ => continue,
        });
    }
}

fn render(out: &mut impl Write, source: &str, steps: &[TraceStep], index: usize) -> io::Result<()> {
    let step = &steps[index];
    let last = index + 1 == steps.len();
    let snippet = snippet(source, &step.span);
    let message = narrative(step, &snippet, last);
    let report = render_inference_step(source, &step.span, index, steps.len(), &message);

    let mut frame = report;
    frame.push_str("\n  ←/→ step · space/enter/esc finish\n");

    queue!(out, MoveTo(0, 0), Clear(ClearType::All))?;
    // Raw mode needs an explicit carriage return on every line break.
    out.write_all(frame.replace('\n', "\r\n").as_bytes())?;
    out.flush()
}

/// The label message for a step: it names the pointed-at subexpression and the
/// type the engine gave it, so "the inferred shape" is always of something.
fn narrative(step: &TraceStep, snippet: &str, last: bool) -> String {
    let ty = paint_type(&step.ty);
    let detail = &step.detail;

    let body = if detail == "literal" {
        format!("the literal {snippet} has type {ty}")
    } else if detail.starts_with("variable") {
        format!("looking up the variable {snippet}, its type is {ty}")
    } else if detail.starts_with("application") {
        format!("the call {snippet} combines its operands to {ty}")
    } else if detail == "lambda" {
        format!("the lambda {snippet} has type {ty}")
    } else if detail == "conditional" {
        format!("the conditional {snippet} merges its branches to {ty}")
    } else {
        format!("{snippet} has type {ty}")
    };

    if last {
        format!("{body} — the final inferred type")
    } else {
        body
    }
}

/// A one-line, backtick-quoted excerpt of the stepped subexpression, with
/// internal whitespace collapsed and long spans truncated, used to name it.
fn snippet(source: &str, span: &SourceSpan) -> String {
    let start = span.start.min(source.len());
    let end = span.end.clamp(start, source.len());
    let text = source[start..end]
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");

    if text.chars().count() > 32 {
        let head: String = text.chars().take(31).collect();
        format!("`{head}…`")
    } else {
        format!("`{text}`")
    }
}

/// Enables raw mode and the alternate screen, restoring both on drop so the
/// terminal is always returned to normal even on an error path.
struct AltScreen;

impl AltScreen {
    fn enter() -> io::Result<Self> {
        enable_raw_mode()?;
        execute!(io::stdout(), EnterAlternateScreen, Hide)?;
        Ok(Self)
    }
}

impl Drop for AltScreen {
    fn drop(&mut self) {
        let _ = execute!(io::stdout(), Show, LeaveAlternateScreen);
        let _ = disable_raw_mode();
    }
}

#[cfg(test)]
mod tests {
    use super::{narrative, snippet};
    use crate::infer::TraceStep;
    use crate::types::Type;

    fn step(detail: &str, ty: Type) -> TraceStep {
        TraceStep {
            span: 0..1,
            detail: detail.to_string(),
            ty,
        }
    }

    #[test]
    fn narratives_name_the_subexpression_and_its_type() {
        // ANSI from the painted type is stripped here for a stable assertion.
        assert_eq!(
            strip(&narrative(&step("literal", Type::Number), "`1`", false)),
            "the literal `1` has type number?"
        );
        assert_eq!(
            strip(&narrative(
                &step("application of `+`", Type::Number),
                "`(+ x 1)`",
                false
            )),
            "the call `(+ x 1)` combines its operands to number?"
        );
        assert_eq!(
            strip(&narrative(
                &step("lambda", Type::Boolean),
                "`(lambda (x) …)`",
                true
            )),
            "the lambda `(lambda (x) …)` has type boolean? — the final inferred type"
        );
    }

    #[test]
    fn snippet_collapses_whitespace_and_truncates() {
        assert_eq!(snippet("(+  x\n  1)", &(0..10)), "`(+ x 1)`");
        let long = "(lambda (a b c d e f g) (+ a b c d e f g))";
        assert!(snippet(long, &(0..long.len())).ends_with("…`"));
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
