# Lavu Agent Notes

## Workflow

- Work in small, focused commits.
- Use title-only commit messages.
- Push after each commit.
- Track project direction and work progress in `lavu-development.org`.
- Sweeping rewrites are acceptable when they are split into small commits.
- Existing dirty work in the tree may be user work; do not revert it unless
  explicitly asked.

## Project Direction

- Lavu aims for R5RS Scheme: nothing more, nothing less.
- Keep the door open for future SRFI-9 records.
- Gerbil has been removed; keep Lavu on its native parser, normalizer,
  type inference engine, evaluator, and Rust standard library.
- Preserve high-quality syntax highlighting, source spans, and Ariadne
  diagnostics throughout parser, rewrite, type inference, and evaluation work.
- Future REPL work should support stepping through Scheme
  rewrites/normalization and type inference.

## Rust Style

- Prefer clean, dense, readable Rust.
- Favor functional approaches where they make the code clearer.
- Use reasonably small reusable self-contained functions.
- Prefer stable ASTs with reusable traversal helpers over repeated
  boilerplate conversions between many nearly identical AST enums.
- Keep educational tiny passes, but avoid duplicating whole-tree traversal
  logic in each pass.
