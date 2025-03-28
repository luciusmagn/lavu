#![allow(unused)]
use color_eyre::eyre::Result;
use interpreter::GerbilInterpreter;
use reedline::Signal;

mod lexer;
use lexer::tokenize;

mod interpreter;

mod parser;
use parser::{
    parser1, parser10, parser11, parser12, parser13, parser14, parser15, parser16, parser2,
    parser3, parser4, parser5, parser6, parser7, parser8, parser9,
};

mod repl;
use repl::{line_editor, print_logo};

mod ast;
use ast::{ast1::ariadne_yap, ast10::report_let_forms};

mod chars;

mod transform;
use transform::*;

mod printer;

mod real_interpreter;

fn main() -> Result<()> {
    color_eyre::install()?;

    let (mut line_editor, prompt) = line_editor()?;
    let mut interpreter = GerbilInterpreter::new()?;

    print_logo();

    loop {
        let sig = line_editor.read_line(&*prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                let lexed = tokenize(&buffer);

                // Check for unclosed expressions
                if let Some(error_idx) = parser1::find_unclosed_sexp(&lexed) {
                    if let Err(e) = parser1::create_diagnostic(&buffer, &lexed, error_idx) {
                        println!("Error creating diagnostic: {}", e);
                    }
                    continue;
                }

                // Try to parse the input
                match parser1::parse(&lexed) {
                    Ok(expressions) => {
                        /*
                        println!("lexed tokens:");
                        println!("{:#?}", lexed);
                        println!("parsed ast:");
                        println!("{:#?}", expressions);
                        println!("ariadne diagnostics:");
                        */
                        //ariadne_yap(&buffer, &lexed, &expressions)?;

                        let parsed2 = parser2::parse(&buffer, &lexed, &expressions)?;
                        let parsed3 = parser3::parse(&buffer, &lexed, &parsed2)?;
                        let parsed4 = parser4::parse(&buffer, &lexed, &parsed3)?;
                        let parsed5 = parser5::parse(&buffer, &lexed, &parsed4)?;
                        let parsed6 = parser6::parse(&buffer, &lexed, &parsed5)?;
                        let parsed7 = parser7::parse(&buffer, &lexed, &parsed6)?;
                        let parsed8 = parser8::parse(&buffer, &lexed, &parsed7)?;
                        let parsed9 = parser9::parse(&buffer, &lexed, &parsed8)?;
                        let parsed10 = parser10::parse(&buffer, &lexed, &parsed9)?;

                        //report_let_forms(&buffer, &lexed, &parsed10)?;

                        let parsed11 = parser11::parse(&buffer, &lexed, &parsed10)?;
                        let parsed12 = parser12::parse(&buffer, &lexed, &parsed11)?;
                        let parsed13 = parser13::parse(&buffer, &lexed, &parsed12)?;
                        let parsed14 = parser14::parse(&buffer, &lexed, &parsed13)?;
                        let parsed15 = parser15::parse(&buffer, &lexed, &parsed14)?;
                        let parsed16 = parser16::parse(&buffer, &lexed, &parsed15)?;

                        let transformed1 = transform1::transform(&buffer, &lexed, &parsed16)?;
                        let transformed2 = transform2::transform(&buffer, &lexed, &transformed1)?;
                        let transformed3 = transform3::transform(&buffer, &lexed, &transformed2)?;
                        let transformed4 = transform4::transform(&buffer, &lexed, &transformed3)?;
                        let transformed5 = transform5::transform(&buffer, &lexed, &transformed4)?;
                        let transformed6 = transform6::transform(&buffer, &lexed, &transformed5)?;
                        let transformed7 = transform7::transform(&buffer, &lexed, &transformed6)?;
                        let transformed8 = transform8::transform(&buffer, &lexed, &transformed7)?;
                        let transformed9 = transform9::transform(&buffer, &lexed, &transformed8)?;

                        //println!("{:#?}", transformed1.forms);
                        println!("{}", printer::pretty_print(&transformed9));

                        // Evaluate in Gerbil (for now)
                        if let Err(e) = interpreter.eval(&printer::pretty_print(&transformed9)) {
                            println!("Error communicating with Gerbil: {}", e);
                            continue;
                        }

                        // Print output
                        for line in interpreter.get_output() {
                            println!("{}", line);
                        }
                        interpreter.clear_output();
                    }
                    Err(errors) => {
                        println!("Parse errors: {:?}", errors);
                    }
                }
            }
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                println!("\nAborted!");
                interpreter.close()?;
                break;
            }
            x => {
                println!("Event: {:?}", x);
            }
        }
    }

    Ok(())
}
