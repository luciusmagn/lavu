//! Interactive `?? expr` inference stepper.
//!
//! A long inference produces dozens of steps; rendered together as Ariadne
//! labels they overlap into a wall. Instead this walks the recorded steps one
//! at a time in the alternate screen: `←` / `→` move between steps and
//! `space` / `enter` / `esc` leave the mode, after which the caller prints the
//! final type the way `? expr` does.

use std::io::{self, Write};

use crossterm::cursor::{Hide, MoveTo, Show};
use crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers};
use crossterm::terminal::{
    Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use crossterm::{execute, queue};

use crate::highlight::{paint_source, paint_type};
use crate::infer::TraceStep;

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
    let start = step.span.start.min(source.len());
    let end = step.span.end.clamp(start, source.len());

    let mut frame = String::new();
    frame.push_str(&format!(
        "  inference mode    step {}/{}    ←/→ step · space/enter/esc finish\n\n",
        index + 1,
        steps.len()
    ));

    // The source, with the current subexpression emphasized in place so the
    // step is visible without a separate caret line (works across newlines).
    frame.push_str("  ");
    frame.push_str(&paint_source(&source[..start]));
    frame.push_str(&emphasize(&source[start..end]));
    frame.push_str(&paint_source(&source[end..]));
    frame.push_str("\n\n");

    frame.push_str(&format!("  {}\n", narrative(step)));
    let label = if last {
        "final inferred type"
    } else {
        "inferred shape so far"
    };
    frame.push_str(&format!("  {label}:  {}\n", paint_type(&step.ty)));

    queue!(out, MoveTo(0, 0), Clear(ClearType::All))?;
    // Raw mode needs an explicit carriage return on every line break.
    out.write_all(frame.replace('\n', "\r\n").as_bytes())?;
    out.flush()
}

fn emphasize(text: &str) -> String {
    if text.is_empty() {
        String::new()
    } else {
        format!("\x1b[1;7m{text}\x1b[0m")
    }
}

/// A short narrative for the current step, phrased as a reasoning sentence.
fn narrative(step: &TraceStep) -> String {
    let ty = paint_type(&step.ty);
    let detail = &step.detail;

    if detail == "literal" {
        format!("A literal value, so its type is {ty} directly.")
    } else if let Some(name) = detail.strip_prefix("variable ") {
        format!("Looking up the variable {name}, its type is {ty}.")
    } else if detail.starts_with("application") {
        format!(
            "{}: combining the operands here yields {ty}.",
            sentence(detail)
        )
    } else if detail == "lambda" {
        format!("Gathering the parameter and body types, the lambda is {ty}.")
    } else if detail == "conditional" {
        format!("Merging both branches, the conditional is {ty}.")
    } else {
        format!("{}, inferred as {ty}.", sentence(detail))
    }
}

fn sentence(detail: &str) -> String {
    let mut chars = detail.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
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
    use super::{narrative, sentence};
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
    fn narratives_read_as_reasoning_sentences() {
        // ANSI from the painted type is stripped here for a stable assertion.
        assert_eq!(
            strip(&narrative(&step("literal", Type::Number))),
            "A literal value, so its type is number? directly."
        );
        assert_eq!(
            strip(&narrative(&step("variable `x`", Type::String))),
            "Looking up the variable `x`, its type is string?."
        );
        assert_eq!(
            strip(&narrative(&step("application of `+`", Type::Number))),
            "Application of `+`: combining the operands here yields number?."
        );
    }

    #[test]
    fn sentence_capitalizes_the_first_letter() {
        assert_eq!(sentence("lambda"), "Lambda");
        assert_eq!(sentence(""), "");
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
