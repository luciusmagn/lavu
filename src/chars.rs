use thiserror::Error;

#[derive(Debug, Error, PartialEq, Clone)]
pub enum ParseCharError {
    #[error("empty input string")]
    EmptyInput,

    #[error("input doesn't start with '#\\'")]
    InvalidPrefix,

    #[error("empty character after prefix")]
    EmptyCharacter,

    #[error("unknown named character: {0}")]
    UnknownNamedChar(String),
}

pub fn parse_char(s: &str) -> Result<char, ParseCharError> {
    if s.is_empty() {
        return Err(ParseCharError::EmptyInput);
    }

    if !s.starts_with("#\\") {
        return Err(ParseCharError::InvalidPrefix);
    }

    let char_part = &s[2..];

    if char_part.is_empty() {
        return Err(ParseCharError::EmptyCharacter);
    }

    // Single character case
    if char_part.chars().count() == 1 {
        return Ok(char_part.chars().next().unwrap());
    }

    // Named characters
    match char_part.to_lowercase().as_str() {
        "space" => Ok(' '),
        "newline" => Ok('\n'),
        _ => Err(ParseCharError::UnknownNamedChar(char_part.to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseCharError, parse_char};

    #[test]
    fn parses_r5rs_named_characters() {
        assert_eq!(parse_char("#\\space"), Ok(' '));
        assert_eq!(parse_char("#\\SPACE"), Ok(' '));
        assert_eq!(parse_char("#\\newline"), Ok('\n'));
    }

    #[test]
    fn rejects_non_r5rs_named_character_extensions() {
        assert_eq!(
            parse_char("#\\tab"),
            Err(ParseCharError::UnknownNamedChar("tab".to_string()))
        );
        assert_eq!(
            parse_char("#\\return"),
            Err(ParseCharError::UnknownNamedChar("return".to_string()))
        );
    }
}
