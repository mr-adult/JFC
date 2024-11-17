#![cfg(test)]

use std::convert::Infallible;

use serde_derive::Deserialize;
use serde_json::{Map, Number, Value};

use crate::{from_key_value_pair_stream, from_value_stream, values_to_json_stream};

#[test]
fn lazy_parse_object() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Test {
        value: String,
    }

    let json = r#"{"one":{"value":"one"},"two":{"value":"two"},"three":{"value":"three"}}"#;
    let mut kvp_stream =
        from_key_value_pair_stream::<Test, _, Infallible>(json.chars().map(|ch| Ok(ch)));

    let first = kvp_stream.next().unwrap().unwrap();
    assert_eq!("one", first.0);
    assert_eq!(
        Test {
            value: "one".to_string()
        },
        first.1
    );

    let second = kvp_stream.next().unwrap().unwrap();
    assert_eq!("two", second.0);
    assert_eq!(
        Test {
            value: "two".to_string()
        },
        second.1
    );

    let third = kvp_stream.next().unwrap().unwrap();
    assert_eq!("three", third.0);
    assert_eq!(
        Test {
            value: "three".to_string()
        },
        third.1
    );
}

#[test]
fn lazy_parse_super_large() {
    let json = values_to_json_stream(vec![
        Value::Null,
        Value::Bool(true),
        Value::Bool(false),
        Value::Number(Number::from(128361)),
        Value::String("\"\"\t".to_string()),
        Value::Array(vec![
            Value::Null,
            Value::Bool(true),
            Value::Bool(false),
            Value::Number(Number::from(128361)),
            Value::String("\"\"\t".to_string()),
        ]),
        Value::Object(Map::new()),
    ]);

    let json = json;

    let json_chars = json
        .flat_map(|result| result)
        .flat_map(|str| str.chars().collect::<Vec<_>>())
        .map(|ch| Ok::<char, Infallible>(ch))
        .collect::<Vec<_>>();
    println!("{json_chars:?}");
    let json_values = from_value_stream::<Value, _, _>(json_chars).collect::<Vec<_>>();
    println!("{json_values:?}");
    let json_output = json_values
        .into_iter()
        .flat_map(|result| result)
        .collect::<Vec<_>>();
    println!("{json_output:?}");
    for value in values_to_json_stream(json_output) {
        print!("{}", value.unwrap());
    }
}

// #[test]
#[allow(dead_code)]
fn fuzz_value() {
    std::panic::catch_unwind(|| {
        let fuzz = json_fuzzer::fuzz();
        for item in fuzz {
            for _ in from_value_stream::<Value, _, Infallible>(
                item.chars().map(|ch| Ok::<_, Infallible>(ch)),
            ) {}
        }
    })
    .unwrap();
}

// #[test]
#[allow(dead_code)]
fn fuzz_key_value_pairs() {
    std::panic::catch_unwind(|| {
        let fuzz = json_fuzzer::fuzz();
        for item in fuzz {
            for _ in from_key_value_pair_stream::<Value, _, Infallible>(
                item.chars().map(|ch| Ok::<_, Infallible>(ch)),
            ) {}
        }
    })
    .unwrap();
}
