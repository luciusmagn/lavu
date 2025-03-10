use ast::ariadne_yap;
use color_eyre::eyre::Result;
use interpreter::GerbilInterpreter;
use reedline::Signal;

mod lexer;
use lexer::tokenize;

mod interpreter;
mod parser;

mod repl;
use repl::{line_editor, print_logo};

mod ast;
mod chars;

fn main() -> Result<()> {
    color_eyre::install()?;

    //flungus();

    let (mut line_editor, prompt) = line_editor()?;
    let mut interpreter = GerbilInterpreter::new()?;

    print_logo();

    loop {
        let sig = line_editor.read_line(&*prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                let lexed = tokenize(&buffer);

                // Check for unclosed expressions
                if let Some(error_idx) = parser::find_unclosed_sexp(&lexed) {
                    if let Err(e) =
                        parser::create_diagnostic(&buffer, &lexed, error_idx)
                    {
                        println!("Error creating diagnostic: {}", e);
                    }
                    continue;
                }

                // Try to parse the input
                match parser::parse(&lexed) {
                    Ok(expressions) => {
                        println!("lexed tokens:");
                        println!("{:#?}", lexed);
                        println!("parsed ast:");
                        println!("{:#?}", expressions);
                        println!("ariadne diagnostics:");
                        ariadne_yap(&buffer, &lexed, &expressions);

                        // Evaluate in Gerbil (for now)
                        if let Err(e) = interpreter.eval(&buffer) {
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

fn flungus() {
    println!();
    println!();
    println!();
    println!();

    let input = r#"
(define brungus 'dung)

;; Factorial function
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Map function
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (my-map f (cdr lst))))

;; Simple list length
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))
"#;

    let lexed = tokenize(&input);

    // Check for unclosed expressions
    if let Some(error_idx) = parser::find_unclosed_sexp(&lexed) {
        if let Err(e) = parser::create_diagnostic(&input, &lexed, error_idx) {
            println!("Error creating diagnostic: {}", e);
        }
    }
}
