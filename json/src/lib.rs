mod tokenizer;
use tokenizer::{JsonParseErr, JsonTokenKind, JsonTokenizer};

use std::{borrow::Cow, collections::HashMap, error::Error, str::FromStr};

pub fn format(json: &str) -> (String, Vec<Box<dyn Error>>) {
    let mut result = String::new();
    let tokenizer = JsonTokenizer::new(json);
    let mut errs = Vec::new();
    let mut indent = 0;
    let mut previous = None;

    for token in tokenizer.into_iter() {
        match token {
            Err(err) => match err {
                JsonParseErr::UnexpectedCharacters(span) => {
                    result.push_str(&json[span.as_range()]);
                    errs.push(err);
                }
                err => {
                    errs.push(err);
                }
            },
            Ok(token) => {
                match token.kind {
                    JsonTokenKind::ObjectStart => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        indent += 1;
                        result.push('{');
                    }
                    JsonTokenKind::ObjectEnd => {
                        indent -= 1;
                        if let Some(JsonTokenKind::ObjectStart) = previous {
                            result.push('}');
                        } else {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                            result.push('}');
                        }
                    }
                    JsonTokenKind::ArrayStart => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        indent += 1;
                        result.push('[');
                    }
                    JsonTokenKind::ArrayEnd => {
                        indent -= 1;
                        if let Some(JsonTokenKind::ArrayStart) = previous {
                            result.push(']');
                        } else {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                            result.push(']');
                        }
                    }
                    JsonTokenKind::Colon => {
                        result.push(':');
                        result.push(' ');
                    }
                    JsonTokenKind::Comma => {
                        result.push('\n');
                        for _ in 0..indent {
                            result.push('\t');
                        }
                        result.push(',');
                        result.push(' ');
                    }
                    JsonTokenKind::String => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str(&json[token.span.as_range()])
                    }
                    JsonTokenKind::Number => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str(&json[token.span.as_range()])
                    }
                    JsonTokenKind::True => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("true")
                    }
                    JsonTokenKind::False => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("false")
                    }
                    JsonTokenKind::Null => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("null");
                    }
                }
                previous = Some(token.kind);
            }
        }
    }

    (
        result,
        if !errs.is_empty() {
            errs.into_iter()
                .map(|err| Box::new(err) as Box<dyn Error>)
                .collect()
        } else {
            Vec::with_capacity(0)
        },
    )
}

#[derive(Clone, Debug)]
pub enum JsonValue<'i> {
    Null,
    Boolean(bool),
    Number(Box<JsonNumber<'i>>),
    String(Box<JsonString<'i>>),
    Array(Vec<JsonValue<'i>>),
    Object(HashMap<&'i str, JsonValue<'i>>),
}

#[derive(Clone, Debug)]
pub struct JsonNumber<'i> {
    source: &'i str,
}

impl<'i> JsonNumber<'i> {
    pub(crate) fn new(source: &'i str) -> Self {
        Self { source }
    }

    pub fn parse<T>(&self) -> Result<T, <T as FromStr>::Err>
    where
        T: FromStr,
    {
        self.source.parse::<T>()
    }
}

#[derive(Clone, Debug)]
pub struct JsonString<'i> {
    source: &'i str,
    cow: Option<Cow<'i, str>>,
}

impl<'i> JsonString<'i> {
    pub(crate) fn new(source: &'i str) -> Self {
        Self {
            source,
            cow: Self::escape(source),
        }
    }

    pub fn raw(&self) -> &str {
        self.source
    }

    pub fn parsed(&self) -> &Option<Cow<'i, str>> {
        &self.cow
    }

    /// Handles escaping characters from the string. If the string
    /// is not a valid JSON string, returns None. If the string is
    /// parsed without issue, returns Some() with the Cow containing
    /// the escaped string.
    pub fn escape(source: &str) -> Option<Cow<'_, str>> {
        let mut cow = Cow::Borrowed(source);

        let mut chars = source.char_indices();
        loop {
            let ch = chars.next();
            match ch {
                None => return Some(cow),
                Some((i, ch)) => {
                    if ch == '\\' {
                        let mut string = match cow {
                            Cow::Borrowed(_) => source[..i].to_string(),
                            Cow::Owned(string) => string,
                        };

                        match chars.next() {
                            None => return None,
                            Some((next_i, next_ch)) => {
                                string.push(match next_ch {
                                    '"' => '"',
                                    '\\' => '\\',
                                    '/' => '/',
                                    'b' => '\u{0008}',
                                    'f' => '\u{000c}',
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    'u' => {
                                        let mut i = 0;
                                        while i < 4 {
                                            match chars.next() {
                                                Some((_, '0'..='9')) => {}
                                                _ => return None,
                                            }
                                            i += 1;
                                        }

                                        let int_str = &source[(next_i + 1)..(next_i + 5)];
                                        let code = u32::from_str_radix(int_str, 16)
                                            .expect("BUG: int_str should always be a valid u32");

                                        char::from_u32(code)
                                            .expect("BUG: char::from_u32 should not fail.")
                                    }
                                    _ => return None,
                                });
                                cow = Cow::Owned(string)
                            }
                        }
                    } else if ch.is_control() {
                        return None;
                    } else {
                        match cow {
                            Cow::Borrowed(_) => {}
                            Cow::Owned(mut string) => {
                                string.push(ch);
                                cow = Cow::Owned(string);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        borrow::Cow,
        io::{BufRead, Write},
    };

    use crate::{tokenizer::{JsonParseErr, JsonTokenizer}, JsonString};

    // #[test]
    fn test_bench() {
        loop {
            println!("Enter your JSON:");
            let mut stdin = std::io::stdin().lock();
            let mut input = String::new();
            stdin
                .read_line(&mut input)
                .expect("Failed to read from stdin.");

            if input.trim().is_empty() {
                break;
            }

            let output = super::format(&input);
            let mut stdout = std::io::stdout();
            stdout
                .write_all(output.0.as_bytes())
                .expect("Failed to write to stdout.");

            // If this doesn't write correctly, we don't care. Just ignore the length.
            let _ = stdout.write(b"\n").expect("Failed to write to stdout");
            if !output.1.is_empty() {
                let mut stdout = std::io::stdout();
                for err in output.1 {
                    writeln!(stdout, "{}", err).expect("Failed to write to stdout.");
                }
                stdout.flush().expect("Failed to flush to stdout.");
            }
        }
    }

    #[test]
    fn escape_sequences() {
        let str = JsonString::escape("\\\\\\u0020\\\"\\b\\f\\n\\t\\r").unwrap();
        let expected = "\\ \"\u{0008}\u{000c}\n\t\r";
        assert_eq!(
            Cow::Owned::<'static, str>(expected.to_string()),
            str.to_owned()
        );
    }

    #[test]
    fn test_json() {
        let str = include_str!("../../bad_test2.json");
        for result in JsonTokenizer::new(str) {
            match result {
                Ok(token) => {
                    print!("{:?} ", token.kind);
                    println!("/{}/", &str[token.span.as_range()]);
                }
                Err(err) => {
                    match err {
                        JsonParseErr::UnexpectedCharacters(span) => {
                            println!("Unexpected chars: {}", &str[span.as_range()]);
                        }
                        JsonParseErr::InvalidUnicodeEscapeSequence(span) => {
                            println!("Invalid unicode escape sequence: {}", &str[span.as_range()])
                        }
                        _ => {
                            println!("{:?}", err);
                        }
                    }
                }
            }
        }
    }
}
