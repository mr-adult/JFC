#![doc = include_str!("../README.md")]

mod tokenizer;
use parser::{JsonParser, Value};
use tokenizer::{JsonParseErr, JsonTokenKind, JsonTokenizer};
pub mod parser;

use std::error::Error;

pub struct FormatOptions<'a> {
    /// Compact mode removes all whitespace
    pub compact: bool,
    /// This string will be used as the indentation string
    pub indent_str: &'a str,
}

pub fn format(json: &str, options: Option<FormatOptions<'_>>) -> (String, Vec<Box<dyn Error>>) {
    let mut result = String::new();
    let tokenizer = JsonTokenizer::new(json);
    let mut errs = Vec::new();
    let mut indent = 0;
    let mut previous = None;

    let chosen_options = options.unwrap_or_else(|| FormatOptions {
        compact: false,
        indent_str: "\t",
    });

    let compact_mode = chosen_options.compact;
    let indent_str = chosen_options.indent_str;

    for token in tokenizer.into_iter() {
        match token {
            Err(err) => match &err {
                JsonParseErr::UnexpectedCharacters(span) => {
                    result.push_str(&json[span.as_range()]);
                    errs.push(err);
                }
                JsonParseErr::TrailingComma(position) => {
                    result.push_str(&json[position.byte_index()..(position.byte_index() + 1)])
                }
                _ => {
                    errs.push(err);
                }
            },
            Ok(token) => {
                match token.kind {
                    JsonTokenKind::ObjectStart => {
                        if !compact_mode {
                            if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                                previous
                            {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                        }
                        indent += 1;
                        result.push('{');
                    }
                    JsonTokenKind::ObjectEnd => {
                        if indent > 0 {
                            indent -= 1;
                        }
                        if let Some(JsonTokenKind::ObjectStart) = previous {
                            result.push('}');
                        } else {
                            if !compact_mode {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                            result.push('}');
                        }
                    }
                    JsonTokenKind::ArrayStart => {
                        if !compact_mode {
                            if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                                previous
                            {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                        }
                        indent += 1;
                        result.push('[');
                    }
                    JsonTokenKind::ArrayEnd => {
                        if indent > 0 {
                            indent -= 1;
                        }
                        if let Some(JsonTokenKind::ArrayStart) = previous {
                            result.push(']');
                        } else {
                            if !compact_mode {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                            result.push(']');
                        }
                    }
                    JsonTokenKind::Colon => {
                        result.push(':');
                        if !compact_mode {
                            result.push(' ');
                        }
                    }
                    JsonTokenKind::Comma => {
                        result.push(',');
                        if !compact_mode {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push_str(indent_str);
                            }
                        }
                    }
                    JsonTokenKind::String => {
                        if !compact_mode {
                            if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                                previous
                            {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                        }
                        result.push_str(&json[token.span.as_range()]);
                    }
                    JsonTokenKind::Number => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            if !compact_mode {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                        }
                        result.push_str(&json[token.span.as_range()])
                    }
                    JsonTokenKind::True => {
                        if !compact_mode {
                            if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                                previous
                            {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                        }
                        result.push_str("true")
                    }
                    JsonTokenKind::False => {
                        if !compact_mode {
                            if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                                previous
                            {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                        }
                        result.push_str("false")
                    }
                    JsonTokenKind::Null => {
                        if !compact_mode {
                            if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                                previous
                            {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                        }
                        result.push_str("null");
                    }
                }
                // we only need this for whitespace decisions, so skip if in compact mode
                if !compact_mode {
                    previous = Some(token.kind);
                }
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

pub fn parse<'json>(json: &'json str) -> (Value<'json>, Vec<Box<dyn Error>>) {
    let (value, errs) = JsonParser::parse(json);

    (
        value,
        errs.into_iter()
            .map(|err| Box::new(err) as Box<dyn Error>)
            .collect(),
    )
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum JsonParseState {
    Object,
    Array,
    KeyValuePairColon,
    KeyValuePairKey,
    Value,
    AfterValue,
}

#[cfg(test)]
mod tests {
    use std::{
        borrow::Cow,
        io::{BufRead, Write},
    };

    use crate::{
        parser::JsonString,
        tokenizer::{JsonParseErr, JsonTokenizer},
        FormatOptions,
    };

    // #[test]
    #[allow(dead_code)]
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

            let output = super::format(&input, None);
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
        let str = JsonString::unescape("\\\\\\u0020\\\"\\b\\f\\n\\t\\r");
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
                    println!("{:?}", token.span.as_range());
                    print!("{:?} ", token.kind);
                    println!("/{}/", &str[token.span.as_range()]);
                }
                Err(err) => match err {
                    JsonParseErr::UnexpectedCharacters(span) => {
                        println!("Unexpected chars: {}", &str[span.as_range()]);
                    }
                    JsonParseErr::InvalidUnicodeEscapeSequence(span) => {
                        println!("Invalid unicode escape sequence: {}", &str[span.as_range()])
                    }
                    _ => {
                        println!("{:?}", err);
                    }
                },
            }
        }

        let output = super::parse(str).0.to_string_pretty();
        println!("{}", output);

        println!("{}", super::parse("[true,[]]").0.to_string_pretty());
    }

    #[test]
    fn doesnt_lose_chars_when_brackets_are_involved() {
        let input = "{\"test\": \"value\",[]]}";
        let (output, _) = super::format(input, None);
        assert_eq!("{\n\t\"test\": \"value\",\n\t[]]\n}", output);

        let input = "[[\"test\",}]]";
        let (output, _) = super::format(input, None);
        assert_eq!("[\n\t[\n\t\t\"test\",}\n\t]\n]", output);

        let input = "{{{}}";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("{{{}}", output);

        let input = "[[[]]";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("[[[]]", output);

        let input = "}}}";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("}}}", output);

        let input = "{{{";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("{{{", output);

        let input = "]]]";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("]]]", output);

        let input = "[[[";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("[[[", output);

        let input = "{{}}}";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("{{}}}", output);

        let input = "[[]]]";
        let (output, _) = super::format(
            input,
            Some(FormatOptions {
                compact: true,
                indent_str: "\t",
            }),
        );
        assert_eq!("[[]]]", output);
    }

    #[test]
    fn handles_simple_inputs_correctly() {
        let input = "test";
        let (output, _) = super::format(input, None);
        assert_eq!("test", output);

        let input = "\"test";
        let (output, _) = super::format(input, None);
        assert_eq!("\"test", output);

        let input = "test\"";
        let (output, _) = super::format(input, None);
        assert_eq!("test\"", output);
    }

    #[test]
    fn parses_valid_json() {
        let input = "[[[[[]]]]]";
        let (output, _) = super::parse(input);
        assert_eq!("[[[[[]]]]]", output.to_string());

        let input = "{\"key\":{\"key\":{\"key\":{\"key\":{}}}}}";
        let (output, _) = super::parse(input);
        assert_eq!(
            "{\"key\":{\"key\":{\"key\":{\"key\":{}}}}}",
            output.to_string()
        );

        let input = "\"test\"";
        let (output, _) = super::parse(input);
        assert_eq!("\"test\"", output.to_string());

        let input = "true";
        let (output, _) = super::parse(input);
        assert_eq!("true", output.to_string());

        let input = "false";
        let (output, _) = super::parse(input);
        assert_eq!("false", output.to_string());

        let input = "null";
        let (output, _) = super::parse(input);
        assert_eq!("null", output.to_string());

        let input = r#"{
    "list": [
        true,
        false,
        null,
        "hello world!",
        2.312812e-1283,
        {},
        {
            "key": null,
            "key2": true,
            "key3": false,
            "key4": "hello, world!",
            "key5": [],
            "key6": {}
            "key7": 2.312812e-1283,
        }
    ]
}"#;

        let (output, _) = super::parse(input);
        assert_eq!(
            r#"{"list":[true,false,null,"hello world!",2.312812e-1283,{},{"key":null,"key2":true,"key3":false,"key4":"hello, world!","key5":[],"key6":{},"key7":2.312812e-1283}]}"#,
            output.to_string()
        );
    }

    #[test]
    fn handles_key_collisions() {
        let input = r#"{"key": "value", "key": "test", "key0": "other_test"}"#;
        let (output, _) = super::parse(input);
        assert_eq!(
            "{\"key\":\"value\",\"key0\":\"test\",\"key00\":\"other_test\"}",
            output.to_string()
        );
    }

    #[test]
    fn double_escapes_bad_escape_sequences() {
        let input = "\"\\h\\k\\u012\\\"\\u0020\"";
        let (output, _) = super::parse(input);
        assert_eq!("\"\\\\h\\\\k\\\\u012\\\"\\u0020\"", output.to_string());
    }

    #[test]
    fn removes_leading_zeroes() {
        let input = "00000000001.0123e-123";
        let (output, _) = super::parse(input);
        assert_eq!("1.0123e-123", output.to_string());

        let input = "0000000000000.123";
        let (output, _) = super::parse(input);
        assert_eq!("0.123", output.to_string());

        let input = "0000000000e-12";
        let (output, _) = super::parse(input);
        assert_eq!("0e-12", output.to_string());

        let input = "0";
        let (output, _) = super::parse(input);
        assert_eq!("0", output.to_string());
    }

    #[test]
    fn leaves_escape_sequences() {
        let input = "\\\"\\\\\\/\\b\\n\\n\\r\\t\u{0000}";
        let (output, _) = super::parse(input);
        assert_eq!("\"\\\"\\\\\\/\\b\\n\\n\\r\\t\\u0000\"", output.to_string());

        let input = "\"\\\\\"";
        let (output, _) = super::parse(input);
        let (_, errs) = super::parse(&output.to_string_pretty());
        if !errs.is_empty() {
            panic!("found error");
        }
    }

    #[test]
    fn to_string_on_escaped_strings() {
        let input = "{ \"test\\\\string\": 1 }";
        let (output, _) = super::parse(input);
        println!("{}", output.to_string_pretty());
    }

    // #[test]
    #[allow(unused)]
    fn fuzzed() {
        let fuzzer = json_fuzzer::fuzz();

        for str in fuzzer {
            if std::panic::catch_unwind(|| {
                super::format(&str, None);
            })
            .is_err()
            {
                println!("FAILURE IN FORMAT. String: {}", &str);
            }

            if std::panic::catch_unwind(|| {
                super::parse(&str);
            })
            .is_err()
            {
                println!("FAILURE IN PARSE. String: {}", &str);
            }
        }
    }

    #[test]
    fn char_indices_test() {
        println!("{}", "\u{009d}".len());
        let str = "\u{009d} \u{009d}";
        println!("{}", str.len());
        println!("{}", str.char_indices().count());
        for (i, ch) in str.char_indices() {
            println!("{:?}, {:?}", i, ch);
        }

        super::parse("\"\u{009d} \u{faed4}\"");
    }
}
