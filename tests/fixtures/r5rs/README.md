# R5RS Fixtures

`r5rs_pitfall.scm` is the SISC R5RS pitfall suite. It targets edge cases
in R5RS implementations, especially `letrec`, `call/cc`, hygienic
macros, keyword shadowing, evaluation order, and `map` behavior.

Source page: <https://sisc-scheme.org/r5rs_pitfall.php>

The upstream file uses SISC-specific harness forms around the actual
R5RS cases. The Rust integration test keeps this file verbatim and
extracts the `(should-be id expected expression)` forms for evaluation
through Lavu.

Run the compliance corpus with:

```sh
cargo test sisc_r5rs_pitfall_suite
```
