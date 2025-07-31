pub mod tokenizer;
pub(crate) use parser::{JsonParser, Value};
pub(crate) use tokenizer::{JsonParseErr, JsonTokenKind, JsonTokenizer};
pub(crate) mod parser;

use std::error::Error;

pub(crate) struct FormatOptions<'a> {
    /// Compact mode removes all whitespace
    pub(crate) compact: bool,
    /// This string will be used as the indentation string
    pub(crate) indent_str: &'a str,
}

pub(crate) fn parse<'json>(json: &'json str) -> (Value<'json>, Vec<Box<dyn Error>>) {
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
    };

    use super::{
        parser::JsonString,
    };

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
