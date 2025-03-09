use color_eyre::Result;

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;

pub struct GerbilInterpreter {
    process: Child,
    stdin: std::process::ChildStdin,
    output_buffer: Arc<Mutex<Vec<String>>>,
}

impl GerbilInterpreter {
    pub fn new() -> Result<Self> {
        // Start gxi process with pipes for stdin/stdout
        let mut process = Command::new("gerbil")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = process.stdin.take().expect("Failed to open stdin");
        let stdout = process.stdout.take().expect("Failed to open stdout");
        let stderr = process.stderr.take().expect("Failed to open stderr");

        let output_buffer = Arc::new(Mutex::new(Vec::new()));
        let buffer_clone = Arc::clone(&output_buffer);

        // Read output in a separate thread
        thread::spawn(move || {
            let stdout_reader = BufReader::new(stdout);
            let stderr_reader = BufReader::new(stderr);

            // Read stdout
            for line in stdout_reader.lines() {
                if let Ok(line) = line {
                    // Skip the prompt lines
                    let mut final_line = line.clone();

                    while final_line.starts_with("> ") && final_line.len() > 2 {
                        final_line = final_line[2..].into();
                    }

                    buffer_clone.lock().unwrap().push(final_line)
                }
            }

            // Read stderr
            for line in stderr_reader.lines() {
                if let Ok(line) = line {
                    let mut buffer = buffer_clone.lock().unwrap();
                    buffer.push(format!("ERROR: {}", line));
                }
            }
        });

        // Skip initial output by sending a dummy command
        let mut interpreter = Self {
            process,
            stdin,
            output_buffer,
        };

        // Clear initial banner
        interpreter.eval("(void)")?;
        interpreter.clear_output();

        Ok(interpreter)
    }

    pub fn eval(&mut self, code: &str) -> Result<()> {
        writeln!(self.stdin, "{}", code)?;
        self.stdin.flush()?;

        // Give Gerbil time to process and output results
        thread::sleep(std::time::Duration::from_millis(100));

        Ok(())
    }

    pub fn get_output(&self) -> Vec<String> {
        let buffer = self.output_buffer.lock().unwrap();
        buffer.clone()
    }

    pub fn clear_output(&self) {
        let mut buffer = self.output_buffer.lock().unwrap();
        buffer.clear();
    }

    pub fn close(mut self) -> Result<()> {
        self.eval("(exit 0)")?;
        self.process.wait()?;
        Ok(())
    }
}
