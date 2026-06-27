use lavu::query::infer_query;

const OCCURRENCE_STRESS: &str = include_str!("fixtures/infer/occurrence_stress.scm");
const RECURSIVE_STRESS: &str = include_str!("fixtures/infer/recursive_stress.scm");

#[test]
fn occurrence_stress_fixture_infers() {
    assert_fixture_infers("occurrence_stress.scm", OCCURRENCE_STRESS);
}

#[test]
fn recursive_stress_fixture_infers() {
    assert_fixture_infers("recursive_stress.scm", RECURSIVE_STRESS);
}

fn assert_fixture_infers(name: &str, source: &str) {
    let failures = source
        .lines()
        .enumerate()
        .filter_map(|(index, line)| {
            let query = line.trim();
            if query.is_empty() || query.starts_with('#') {
                return None;
            }

            match infer_query(query) {
                Ok(types) if !types.is_empty() => None,
                Ok(_) => Some(format!("{}:{} inferred no forms", name, index + 1)),
                Err(error) => Some(format!("{}:{}: {}\n{}", name, index + 1, error, query)),
            }
        })
        .collect::<Vec<_>>();

    assert!(
        failures.is_empty(),
        "{} inference stress case(s) failed:\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
