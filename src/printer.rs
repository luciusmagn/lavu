use crate::ast::ast1::Atom;
use crate::ast::ast16::*;

pub fn pretty_print(program: &Program) -> String {
    let mut result = String::new();
    for form in &program.forms {
        result.push_str(&pretty_print_form(form, 0));
        result.push_str("\n");
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

            let mut result = format!("{}(\n", spaces);
            for element in elements {
                result.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(element, indent + 2)
                ));
            }
            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::Vector(elements, _) => {
            if elements.is_empty() {
                return format!("{}#()", spaces);
            }

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

            let mut result = format!("{}#(\n", spaces);
            for element in elements {
                result.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(element, indent + 2)
                ));
            }
            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::Quote(expr, _) => format!("{}'({})", spaces, pretty_print_expression(expr, 0)),
        Expression::Quasiquote(expr, _) => {
            format!("{}`({})", spaces, pretty_print_expression(expr, 0))
        }
        Expression::Unquote(expr, _) => format!("{},{}", spaces, pretty_print_expression(expr, 0)),
        Expression::UnquoteSplicing(expr, _) => {
            format!("{},@{}", spaces, pretty_print_expression(expr, 0))
        }
        Expression::SymbolLiteral(sym, _) => format!("{}'{}", spaces, sym),
        Expression::Lambda(args, body, _) => {
            let args_str = args
                .iter()
                .map(|(arg, _)| arg.clone())
                .collect::<Vec<_>>()
                .join(" ");

            let mut result = format!("{}(lambda ({})", spaces, args_str);

            if body.is_empty() {
                return format!("{})", result);
            }

            result.push_str("\n");
            for expr in body {
                result.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }
            result.push_str(&format!("{})", spaces));
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

            let mut result = format!("{}(begin\n", spaces);
            for expr in exprs {
                result.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }
            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::If(cond, then_expr, else_expr, _) => {
            let mut result = format!(
                "{}(if {}\n",
                spaces,
                pretty_print_expression(cond, indent + 2)
            );
            result.push_str(&format!(
                "{}{}\n",
                " ".repeat(indent + 2),
                pretty_print_expression(then_expr, indent + 2)
            ));

            if let Some(else_e) = else_expr {
                result.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(else_e, indent + 2)
                ));
            }

            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::Let(bindings, body, _) => {
            let mut result = format!("{}(let (", spaces);

            if !bindings.is_empty() {
                result.push_str("\n");
                for ((var, _), expr) in bindings {
                    result.push_str(&format!(
                        "{}({} {})\n",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push_str(")");
            }

            if !body.is_empty() {
                result.push_str("\n");
                for expr in body {
                    result.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::LetStar(bindings, body, _) => {
            let mut result = format!("{}(let* (", spaces);

            if !bindings.is_empty() {
                result.push_str("\n");
                for ((var, _), expr) in bindings {
                    result.push_str(&format!(
                        "{}({} {})\n",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push_str(")");
            }

            if !body.is_empty() {
                result.push_str("\n");
                for expr in body {
                    result.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::LetRec(bindings, body, _) => {
            let mut result = format!("{}(letrec (", spaces);

            if !bindings.is_empty() {
                result.push_str("\n");
                for ((var, _), expr) in bindings {
                    result.push_str(&format!(
                        "{}({} {})\n",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push_str(")");
            }

            if !body.is_empty() {
                result.push_str("\n");
                for expr in body {
                    result.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::Case(key, clauses, else_clause, _) => {
            let mut result = format!(
                "{}(case {}\n",
                spaces,
                pretty_print_expression(key, indent + 2)
            );

            for (datums, body) in clauses {
                let datums_str = datums
                    .iter()
                    .map(|e| pretty_print_expression(e, 0))
                    .collect::<Vec<_>>()
                    .join(" ");

                result.push_str(&format!("{}(({}) ", " ".repeat(indent + 2), datums_str));

                if body.is_empty() {
                    result.push_str(")\n");
                    continue;
                }

                result.push_str("\n");
                for expr in body {
                    result.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(indent + 4),
                        pretty_print_expression(expr, indent + 4)
                    ));
                }
                result.push_str(&format!("{})\n", " ".repeat(indent + 2)));
            }

            if let Some(else_body) = else_clause {
                result.push_str(&format!("{}(else ", " ".repeat(indent + 2)));

                if else_body.is_empty() {
                    result.push_str(")\n");
                } else {
                    result.push_str("\n");
                    for expr in else_body {
                        result.push_str(&format!(
                            "{}{}\n",
                            " ".repeat(indent + 4),
                            pretty_print_expression(expr, indent + 4)
                        ));
                    }
                    result.push_str(&format!("{})\n", " ".repeat(indent + 2)));
                }
            }

            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::Cond(clauses, else_clause, _) => {
            let mut result = format!("{}(cond\n", spaces);

            for (test, body) in clauses {
                result.push_str(&format!(
                    "{}({} ",
                    " ".repeat(indent + 2),
                    pretty_print_expression(test, 0)
                ));

                if body.is_empty() {
                    result.push_str(")\n");
                    continue;
                }

                result.push_str("\n");
                for expr in body {
                    result.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(indent + 4),
                        pretty_print_expression(expr, indent + 4)
                    ));
                }
                result.push_str(&format!("{})\n", " ".repeat(indent + 2)));
            }

            if let Some(else_body) = else_clause {
                result.push_str(&format!("{}(else ", " ".repeat(indent + 2)));

                if else_body.is_empty() {
                    result.push_str(")\n");
                } else {
                    result.push_str("\n");
                    for expr in else_body {
                        result.push_str(&format!(
                            "{}{}\n",
                            " ".repeat(indent + 4),
                            pretty_print_expression(expr, indent + 4)
                        ));
                    }
                    result.push_str(&format!("{})\n", " ".repeat(indent + 2)));
                }
            }

            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::NamedLet((name, _), bindings, body, _) => {
            let mut result = format!("{}(let {} (", spaces, name);

            if !bindings.is_empty() {
                result.push_str("\n");
                for ((var, _), expr) in bindings {
                    result.push_str(&format!(
                        "{}({} {})\n",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(expr, 0)
                    ));
                }
                result.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result.push_str(")");
            }

            if !body.is_empty() {
                result.push_str("\n");
                for expr in body {
                    result.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::And(exprs, _) => {
            if exprs.is_empty() {
                return format!("{}(and)", spaces);
            }

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

            let mut result = format!("{}(and\n", spaces);
            for expr in exprs {
                result.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }
            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::Or(exprs, _) => {
            if exprs.is_empty() {
                return format!("{}(or)", spaces);
            }

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

            let mut result = format!("{}(or\n", spaces);
            for expr in exprs {
                result.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }
            result.push_str(&format!("{})", spaces));
            result
        }
        Expression::Do(bindings, (test, result), body, _) => {
            let mut result_str = format!("{}(do (", spaces);

            if !bindings.is_empty() {
                result_str.push_str("\n");
                for ((var, _), init, step) in bindings {
                    result_str.push_str(&format!(
                        "{}({} {} {})\n",
                        " ".repeat(indent + 2),
                        var,
                        pretty_print_expression(init, 0),
                        match step {
                            Some(step_expr) => pretty_print_expression(step_expr, 0),
                            None => String::new(),
                        }
                    ));
                }
                result_str.push_str(&format!("{})", " ".repeat(indent)));
            } else {
                result_str.push_str(")");
            }

            // Test and result
            result_str.push_str(&format!(
                " ({}\n",
                pretty_print_expression(test, indent + 2)
            ));

            for expr in result {
                result_str.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(indent + 2),
                    pretty_print_expression(expr, indent + 2)
                ));
            }
            result_str.push_str(&format!("{})", " ".repeat(indent)));

            // Body
            if !body.is_empty() {
                result_str.push_str("\n");
                for expr in body {
                    result_str.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(indent + 2),
                        pretty_print_expression(expr, indent + 2)
                    ));
                }
            }

            result_str.push_str(&format!("{})", spaces));
            result_str
        }
        Expression::Delay(expr, _) => {
            format!(
                "{}(delay {})",
                spaces,
                pretty_print_expression(expr, indent + 2)
            )
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
