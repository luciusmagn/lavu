use crate::ast::ast1::Atom;
use crate::ast::ast16::*;

pub fn pretty_print(program: &Program) -> String {
    let mut result = String::new();
    for form in &program.forms {
        result.push_str(&pretty_print_form(form, 0));
        result.push('\n');
    }
    result
}

fn pretty_print_form(form: &TopLevelForm, indent: usize) -> String {
    match form {
        TopLevelForm::Definition(def) => pretty_print_definition(def, indent),
        TopLevelForm::Expression(expr) => pretty_print_expression(expr, indent),
    }
}

fn pretty_print_definition(def: &Definition, indent: usize) -> String {
    match def {
        Definition::Variable((name, _), expr) => {
            let spaces = " ".repeat(indent);
            format!(
                "{}(define {} {})",
                spaces,
                name,
                pretty_print_expression(expr, indent + 2)
            )
        }
    }
}

fn pretty_print_expression(expr: &Expression, indent: usize) -> String {
    let spaces = " ".repeat(indent);

    match expr {
        Expression::Atom(atom, _) => pretty_print_atom(atom),

        Expression::List(elements, _) => {
            if elements.is_empty() {
                return format!("{}()", spaces);
            }

            // Try one-line format first
            let one_line = format!(
                "({})",
                elements
                    .iter()
                    .map(|e| pretty_print_expression(e, 0))
                    .collect::<Vec<_>>()
                    .join(" ")
            );

            if one_line.len() < 80 {
                return format!("{}{}", spaces, one_line);
            }

            // Multi-line format
            let mut result = format!("{}(", spaces);

            // Handle first element specially - often the function/operator
            if !elements.is_empty() {
                result.push_str(&pretty_print_expression(&elements[0], 0));

                // Add other elements with proper indentation
                for element in &elements[1..] {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}{}",
                        " ".repeat(indent + 2),
                        pretty_print_expression(element, 0)
                    ));
                }
            }

            result.push(')');
            result
        }

        Expression::Vector(elements, _) => {
            if elements.is_empty() {
                return format!("{}#()", spaces);
            }

            // Try one-line format first
            let one_line = format!(
                "#({})",
                elements
                    .iter()
                    .map(|e| pretty_print_expression(e, 0))
                    .collect::<Vec<_>>()
                    .join(" ")
            );

            if one_line.len() < 80 {
                return format!("{}{}", spaces, one_line);
            }

            // Multi-line format
            let mut result = format!("{}#(", spaces);

            // Handle first element specially
            if !elements.is_empty() {
                result.push_str(&pretty_print_expression(&elements[0], 0));

                // Add other elements with proper indentation
                for element in &elements[1..] {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}{}",
                        " ".repeat(indent + 2),
                        pretty_print_expression(element, 0)
                    ));
                }
            }

            result.push(')');
            result
        }

        Expression::Quote(inner, _) => {
            format!("{}'({})", spaces, pretty_print_expression(inner, 0))
        }

        Expression::Quasiquote(inner, _) => {
            format!("{}`({})", spaces, pretty_print_expression(inner, 0))
        }

        Expression::Unquote(inner, _) => {
            format!("{},{}", spaces, pretty_print_expression(inner, 0))
        }

        Expression::UnquoteSplicing(inner, _) => {
            format!("{},@{}", spaces, pretty_print_expression(inner, 0))
        }

        Expression::SymbolLiteral(sym, _) => format!("{}'{}", spaces, sym),

        Expression::Lambda(args, body, _) => {
            let args_str = args
                .iter()
                .map(|(arg, _)| arg.clone())
                .collect::<Vec<_>>()
                .join(" ");

            // Format the lambda with proper indentation
            let mut result = format!("{}(lambda ({})", spaces, args_str);

            if body.is_empty() {
                result.push(')');
                return result;
            }

            // Format the body
            for (i, expr) in body.iter().enumerate() {
                result.push('\n');
                result.push_str(&format!(
                    "{}{}",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }

            result.push(')');
            result
        }

        Expression::SetBang((var, _), expr, _) => {
            format!(
                "{}(set! {} {})",
                spaces,
                var,
                pretty_print_expression(expr, indent + 2)
            )
        }

        Expression::Begin(exprs, _) => {
            if exprs.is_empty() {
                return format!("{}(begin)", spaces);
            }

            let mut result = format!("{}(begin", spaces);

            // Format the body expressions
            for expr in exprs {
                result.push('\n');
                result.push_str(&format!(
                    "{}{}",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }

            result.push(')');
            result
        }

        Expression::If(cond, then_expr, else_expr, _) => {
            let mut result = format!("{}(if ", spaces);

            // Condition - format appropriately
            result.push_str(&pretty_print_expression(cond, 0));

            // Then expression - use 4 spaces from the if for better alignment
            result.push('\n');
            result.push_str(&format!(
                "{spaces}{}{}",
                " ".repeat(indent + 4), // Increased from +2 to +4
                pretty_print_expression(then_expr, indent + 4)
            ));

            // Else expression (if present)
            if let Some(else_e) = else_expr {
                result.push('\n');
                result.push_str(&format!(
                    "{spaces}{}",
                    pretty_print_expression(else_e, spaces.len() + indent + 4)
                ));
            }

            result.push(')');
            result
        }

        Expression::Let(bindings, body, _) => {
            let mut result = format!("{}(let (", spaces);

            if !bindings.is_empty() {
                // Format the bindings
                for ((var, _), expr) in bindings {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}({} {})",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push('\n');
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push(')');
            }

            // Format the body
            if !body.is_empty() {
                for expr in body {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}{}",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push(')');
            result
        }

        Expression::LetStar(bindings, body, _) => {
            let mut result = format!("{}(let* (", spaces);

            if !bindings.is_empty() {
                // Format the bindings
                for ((var, _), expr) in bindings {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}({} {})",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push('\n');
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push(')');
            }

            // Format the body
            if !body.is_empty() {
                for expr in body {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}{}",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push(')');
            result
        }

        Expression::LetRec(bindings, body, _) => {
            let mut result = format!("{}(letrec (", spaces);

            if !bindings.is_empty() {
                // Format the bindings
                for ((var, _), expr) in bindings {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}({} {})",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push('\n');
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push(')');
            }

            // Format the body
            if !body.is_empty() {
                for expr in body {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}{}",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push(')');
            result
        }

        Expression::Case(key, clauses, else_clause, _) => {
            let mut result = format!("{}(case ", spaces);

            // Format key expression
            result.push_str(&pretty_print_expression(key, 0));

            // Format clauses
            for (datums, body) in clauses {
                result.push('\n');

                // Format datums
                let datums_str = datums
                    .iter()
                    .map(|e| pretty_print_expression(e, 0))
                    .collect::<Vec<_>>()
                    .join(" ");

                result.push_str(&format!("{}(({}) ", " ".repeat(indent + 2), datums_str));

                // Format body
                if body.is_empty() {
                    result.push(')');
                    continue;
                }

                for (i, expr) in body.iter().enumerate() {
                    if i > 0 {
                        result.push('\n');
                    }
                    result.push_str(&format!(
                        "\n{}{}",
                        " ".repeat(indent + 4),
                        pretty_print_expression(expr, indent + 4)
                    ));
                }
                result.push_str(&format!("\n{})", " ".repeat(indent + 2)));
            }

            // Format else clause
            if let Some(else_body) = else_clause {
                result.push('\n');
                result.push_str(&format!("{}(else ", " ".repeat(indent + 2)));

                if else_body.is_empty() {
                    result.push(')');
                } else {
                    for (i, expr) in else_body.iter().enumerate() {
                        if i > 0 {
                            result.push('\n');
                        }
                        result.push_str(&format!(
                            "\n{}{}",
                            " ".repeat(indent + 4),
                            pretty_print_expression(expr, indent + 4)
                        ));
                    }
                    result.push_str(&format!("\n{})", " ".repeat(indent + 2)));
                }
            }

            result.push(')');
            result
        }

        Expression::Cond(clauses, else_clause, _) => {
            let mut result = format!("{}(cond", spaces);

            // Format clauses
            for (test, body) in clauses {
                result.push('\n');
                result.push_str(&format!(
                    "{}({}",
                    " ".repeat(indent + 2),
                    pretty_print_expression(test, 0)
                ));

                // Format body
                if body.is_empty() {
                    result.push(')');
                    continue;
                }

                for (i, expr) in body.iter().enumerate() {
                    if i > 0 {
                        result.push('\n');
                    }
                    result.push_str(&format!(
                        "\n{}{}",
                        " ".repeat(indent + 4),
                        pretty_print_expression(expr, indent + 4)
                    ));
                }
                result.push_str(&format!("\n{})", " ".repeat(indent + 2)));
            }

            // Format else clause
            if let Some(else_body) = else_clause {
                result.push('\n');
                result.push_str(&format!("{}(else", " ".repeat(indent + 2)));

                if else_body.is_empty() {
                    result.push(')');
                } else {
                    for (i, expr) in else_body.iter().enumerate() {
                        if i > 0 {
                            result.push('\n');
                        }
                        result.push_str(&format!(
                            "\n{}{}",
                            " ".repeat(indent + 4),
                            pretty_print_expression(expr, indent + 4)
                        ));
                    }
                    result.push_str(&format!("\n{})", " ".repeat(indent + 2)));
                }
            }

            result.push(')');
            result
        }

        Expression::NamedLet((name, _), bindings, body, _) => {
            let mut result = format!("{}(let {} (", spaces, name);

            if !bindings.is_empty() {
                // Format bindings
                for ((var, _), expr) in bindings {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}({} {})",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push('\n');
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push(')');
            }

            // Format body
            if !body.is_empty() {
                for expr in body {
                    result.push('\n');
                    result.push_str(&format!(
                        "{}{}",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push(')');
            result
        }

        Expression::And(exprs, _) => {
            if exprs.is_empty() {
                return format!("{}(and)", spaces);
            }

            // Try one-line formatting
            let one_line = format!(
                "(and {})",
                exprs
                    .iter()
                    .map(|e| pretty_print_expression(e, 0))
                    .collect::<Vec<_>>()
                    .join(" ")
            );

            if one_line.len() < 80 {
                return format!("{}{}", spaces, one_line);
            }

            // Multi-line formatting
            let mut result = format!("{}(and", spaces);

            for expr in exprs {
                result.push('\n');
                result.push_str(&format!(
                    "{}{}",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }

            result.push(')');
            result
        }

        Expression::Or(exprs, _) => {
            if exprs.is_empty() {
                return format!("{}(or)", spaces);
            }

            // Try one-line formatting
            let one_line = format!(
                "(or {})",
                exprs
                    .iter()
                    .map(|e| pretty_print_expression(e, 0))
                    .collect::<Vec<_>>()
                    .join(" ")
            );

            if one_line.len() < 80 {
                return format!("{}{}", spaces, one_line);
            }

            // Multi-line formatting
            let mut result = format!("{}(or", spaces);

            for expr in exprs {
                result.push('\n');
                result.push_str(&format!(
                    "{}{}",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }

            result.push(')');
            result
        }

        Expression::Do(bindings, (test, result), body, _) => {
            let mut result_str = format!("{}(do (", spaces);

            if !bindings.is_empty() {
                // Format bindings
                for ((var, _), init, step) in bindings {
                    result_str.push('\n');
                    let step_str = match step {
                        Some(step_expr) => format!(" {}", pretty_print_expression(step_expr, 0)),
                        None => String::new(),
                    };

                    result_str.push_str(&format!(
                        "{}({} {}{})",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(init, 0),
                        step_str
                    ));
                }
                result_str.push('\n');
                result_str.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result_str.push(')');
            }

            // Format test and result
            result_str.push_str(&format!(" ({}", pretty_print_expression(test, 0)));

            // Format result expressions
            if result.is_empty() {
                result_str.push(')');
            } else {
                for (i, expr) in result.iter().enumerate() {
                    if i == 0 {
                        result_str.push(' ');
                    } else {
                        result_str.push('\n');
                        result_str.push_str(&" ".repeat(indent + 4));
                    }
                    result_str.push_str(&pretty_print_expression(expr, 0));
                }
                result_str.push(')');
            }

            // Format body expressions
            if !body.is_empty() {
                for expr in body {
                    result_str.push('\n');
                    result_str.push_str(&format!(
                        "{}{}",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result_str.push(')');
            result_str
        }

        Expression::Delay(expr, _) => {
            format!("{}(delay {})", spaces, pretty_print_expression(expr, 0))
        }
    }
}

fn pretty_print_atom(atom: &Atom) -> String {
    match atom {
        Atom::Identifier(id) => id.clone(),
        Atom::Integer(i) => i.to_string(),
        Atom::Decimal(d) => d.to_string(),
        Atom::Real((n, d)) => format!("{}/{}", n, d),
        Atom::Complex(c) => format!("{}+{}i", c.re, c.im),
        Atom::String(s) => format!("\"{}\"", s),
        Atom::Boolean(b) => if *b { "#t" } else { "#f" }.to_string(),
        Atom::Character(c) => format!("#\\{}", c),
    }
}
