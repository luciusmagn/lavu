use chrono::{Local, Utc};
use color_eyre::eyre::Result;
use logos::Logos;
use nu_ansi_term::{Color, Style};
use reedline::{
    DefaultHinter, Highlighter, Hinter, History, Prompt, PromptEditMode,
    PromptViMode, Reedline, SqliteBackedHistory, StyledText, ValidationResult,
    Validator,
};

use std::borrow::Cow;
use std::env;

use crate::lexer::{
    Token, is_conversion, is_keywordy, is_mutator, is_operator, is_predicate,
    is_special_form,
};

pub fn history() -> Result<Box<dyn History>> {
    let history_path = dirs::cache_dir()
        .or_else(dirs::data_dir)
        .or_else(dirs::home_dir)
        .unwrap_or(env::current_dir()?)
        .join(".lavu-history.db");

    Ok(Box::new(SqliteBackedHistory::with_file(
        history_path,
        Reedline::create_history_session_id(),
        Some(Utc::now()),
    )?))
}

pub fn highlighter() -> Result<Box<dyn Highlighter>> {
    struct SchemeHighlighter;

    impl Highlighter for SchemeHighlighter {
        fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
            let mut result = StyledText::new();
            let mut lexer = Token::lexer(line);

            let mut previous = None;

            while let Some(token_result) = lexer.next() {
                let span = lexer.span();
                let text = &line[span.clone()];

                let style = match &token_result {
                    Ok(Token::Identifier(_)) => match text {
                        x if is_special_form(x) => {
                            Style::new().fg(Color::Green)
                        }
                        x if is_keywordy(x) => Style::new().fg(Color::Purple),
                        x if is_predicate(x) => {
                            Style::new().fg(Color::LightBlue)
                        }
                        x if is_mutator(x) => Style::new().fg(Color::Red),
                        x if is_conversion(x) => Style::new().fg(Color::Yellow),
                        x if is_operator(x) => Style::new().fg(Color::LightRed),
                        _ if previous == Some(Token::LParen) => {
                            Style::new().fg(Color::LightCyan)
                        }
                        _ => Style::new().fg(Color::Default),
                    },
                    Ok(Token::Integer(_))
                    | Ok(Token::Decimal(_))
                    | Ok(Token::Binary)
                    | Ok(Token::Octal)
                    | Ok(Token::Hex) => Style::new().fg(Color::Green),
                    Ok(Token::String(_)) => Style::new().fg(Color::LightRed),
                    Ok(Token::Character(_)) => {
                        Style::new().fg(Color::LightCyan).italic()
                    }
                    Ok(Token::True) | Ok(Token::False) => {
                        Style::new().fg(Color::LightGreen).bold()
                    }
                    Ok(Token::LParen) | Ok(Token::RParen)
                    | Ok(Token::LBracket) | Ok(Token::RBracket) => {
                        Style::new().fg(Color::Purple)
                    }
                    Ok(Token::Quote)
                    | Ok(Token::Backquote)
                    | Ok(Token::Unquote)
                    | Ok(Token::UnquoteSplicing)
                    | Ok(Token::SyntaxQuote) => {
                        Style::new().fg(Color::Magenta).bold()
                    }
                    Ok(Token::VectorStart) => {
                        Style::new().fg(Color::Purple).bold()
                    }
                    Ok(Token::LineComment) => {
                        Style::new().fg(Color::DarkGray).italic()
                    }
                    _ => Style::new().fg(Color::Default),
                };

                match token_result {
                    Ok(Token::Whitespace(_)) | Err(_) => (),
                    Ok(owo) => {
                        previous = Some(owo);
                    }
                }

                result.push((style, text.to_string()));
            }

            result
        }
    }

    Ok(Box::new(SchemeHighlighter))
}

pub fn hinter() -> Result<Box<dyn Hinter>> {
    let h = Box::new(
        DefaultHinter::default()
            .with_style(Style::new().italic().fg(Color::LightGray)),
    );

    Ok(h)
}

pub fn validator() -> Result<Box<dyn Validator>> {
    struct SchemeValidator;

    impl Validator for SchemeValidator {
        fn validate(&self, line: &str) -> ValidationResult {
            let lparens = line.chars().filter(|c| *c == '(').count() as i64;
            let rparens = line.chars().filter(|c| *c == ')').count() as i64;

            if lparens - rparens != 0 {
                ValidationResult::Incomplete
            } else {
                ValidationResult::Complete
            }
        }
    }

    Ok(Box::new(SchemeValidator))
}

pub fn prompt() -> Result<Box<dyn Prompt>> {
    struct SchemePrompt;

    impl Prompt for SchemePrompt {
        fn render_prompt_left(&self) -> Cow<str> {
            let now = Local::now();
            let stamp = now.format("%H:%M").to_string();

            Cow::Owned(format!("{stamp} "))
        }

        fn render_prompt_right(&self) -> std::borrow::Cow<str> {
            Cow::Borrowed("")
        }

        fn render_prompt_indicator(
            &self,
            prompt_mode: reedline::PromptEditMode,
        ) -> std::borrow::Cow<str> {
            match prompt_mode {
                PromptEditMode::Default => Cow::Borrowed("( ) λ "),
                PromptEditMode::Vi(PromptViMode::Insert) => {
                    Cow::Borrowed("[I] λ ")
                }
                PromptEditMode::Vi(PromptViMode::Normal) => {
                    Cow::Borrowed("[N] λ ")
                }
                PromptEditMode::Emacs => Cow::Borrowed("(E) λ "),
                PromptEditMode::Custom(_) => Cow::Borrowed("(-) λ "),
            }
        }

        fn render_prompt_multiline_indicator(&self) -> std::borrow::Cow<str> {
            Cow::Owned(format!("{}| ", " ".repeat(10)))
        }

        fn render_prompt_history_search_indicator(
            &self,
            _history_search: reedline::PromptHistorySearch,
        ) -> std::borrow::Cow<str> {
            Cow::Borrowed("search: ")
        }
    }

    Ok(Box::new(SchemePrompt))
}

pub fn print_logo() {
    println!(
        r#"

██╗      █████╗ ██╗   ██╗██╗   ██╗         __/)
██║     ██╔══██╗██║   ██║██║   ██║      .-(__(=:
██║     ███████║██║   ██║██║   ██║   |\ |    \)
██║     ██╔══██║╚██╗ ██╔╝██║   ██║   \ ||
███████╗██║  ██║ ╚████╔╝ ╚██████╔╝    \|| ejm97
╚══════╝╚═╝  ╚═╝  ╚═══╝   ╚═════╝      \|
-- Lavu Scheme 0.0 (me@mag.wiki)        |
"#
    )
}

pub fn line_editor() -> Result<(Reedline, Box<dyn Prompt>)> {
    let line_editor = Reedline::create()
        .with_hinter(hinter()?)
        .with_history(history()?)
        .with_validator(validator()?)
        .with_highlighter(highlighter()?);

    let prompt = prompt()?;

    Ok((line_editor, prompt))
}
