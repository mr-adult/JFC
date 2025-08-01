#![feature(str_from_utf16_endian)]
#![doc = include_str!("../README.md")]

use std::{
    error::Error,
    fs::OpenOptions,
    io::{stderr, stdin, stdout, BufRead, BufReader, Read, Write},
    process::ExitCode,
};

mod json;

use clap::{command, Arg, ArgAction};
use colored::Colorize;
use json::{
    parse,
    parser::{JsonString, Value},
    FormatOptions, JsonParseErr, JsonTokenKind, JsonTokenizer,
};

fn main() -> Result<(), ExitCode> {
    let arg_matches = command!()
        .about(r#"
                 ___  ________ ________     
                |\  \|\  _____\\   ____\    
                \ \  \ \  \__/\ \  \___|    
              __ \ \  \ \   __\\ \  \       
             |\  \\_\  \ \  \_| \ \  \____  
             \ \________\ \__\   \ \_______\
              \|________|\|__|    \|_______|

JSON Formatting CLI (JFC) is a CLI tool written in Rust for working 
with malformed JSON. It does its best to transform whatever text you 
input on standard in into valid JSON without dropping any meaningful 
characters from the input. Any errors found along the way are emitted 
to standard error. In raw mode, no characters from the input are (or
at least should be) dropped. If you see characters being dropped in 
raw mode, please submit an issue to https://github.com/mr-adult/JFC.
"#)
        .arg(
            Arg::new("compact")
                .long("compact")
                .short('c')
                .action(ArgAction::SetTrue)
                .help("compact instead of pretty-printed output"),
        )
        .arg(
            Arg::new("raw")
            .long("raw")
                .short('r')
                .action(ArgAction::SetTrue)
                .help("tells jfc not to insert any new tokens to fix the output. Instead, only whitespace will be inserted into the output")
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .help("read the input as a file path instead of a JSON string")
        )
        .arg(
            Arg::new("recursive")
                .long("recursive")
                .short('R')
                .action(ArgAction::SetTrue)
                .help("[non-raw mode only] pass each string in the output back through the parser to be parsed as JSON; this process is done recursively")
        )
        .arg(
            Arg::new("tab")
                .long("tab")
                .action(ArgAction::SetTrue)
                .help("use tabs for indentation (default: 2 spaces)"),
        )
        .get_matches();

    let compact = arg_matches.get_one("compact").unwrap_or(&false);
    let raw = arg_matches.get_one("raw").unwrap_or(&false);
    let empty_string = String::with_capacity(0);
    let file = arg_matches
        .get_one::<String>("file")
        .unwrap_or(&empty_string);
    let recursive = arg_matches.get_one::<bool>("recursive").unwrap_or(&false);

    let tab = arg_matches.get_one::<bool>("tab").unwrap_or(&false);
    let indent_str = if *tab { "\t" } else { "  " };

    let input = if file.is_empty() {
        #[cfg(debug_assertions)]
        {
            r#"{
    "glossary": {
        "title": "example glossary",
		"GlossDiv": {
            "title": "S",
			"GlossList": {
                "GlossEntry": {
                    "ID": "SGML",
					"SortAs": "SGML",
					"GlossTerm": "Standard Generalized Markup Language",
					"Acronym": "SGML",
					"Abbrev": "ISO 8879:1986",
					"GlossDef": {
                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
						"GlossSeeAlso": ["GML", "XML"]
                    },
					"GlossSee": "markup"
                }
            }
        }
    }
}"#.to_string()
        }
        #[cfg(not(debug_assertions))]
        {
            let reader = BufReader::new(stdin().lock());
            read_buf_to_string(reader)?
        }
    } else {
        match OpenOptions::new().read(true).write(false).open(file) {
            Ok(file) => {
                let reader = BufReader::new(file);
                read_buf_to_string(reader)?
            }
            Err(err) => {
                let mut stderr = stderr().lock();
                stderr
                    .write_all(
                        format!("Failed to open file '{}'. Message: {}", file, err).as_bytes(),
                    )
                    .ok();
                stderr.write(&[b'\n']).ok();
                stderr.flush().ok();
                return Err(ExitCode::FAILURE);
            }
        }
    };

    let errs_to_report;
    let mut errs_from_recursive_parsing: Vec<(String, Vec<Box<dyn Error>>)> = Vec::with_capacity(0);
    if *raw {
        let (output, errs) = format_colored(
            &input,
            Some(FormatOptions {
                compact: *compact,
                indent_str,
            }),
        );
        let mut stdout = stdout().lock();
        stdout.write_all(output.as_bytes()).ok();
        stdout.write(&[b'\n', b'\n']).ok();
        stdout.flush().ok();
        errs_to_report = errs;
    } else {
        let (mut output, errs) = parse(&input);

        if *recursive {
            let mut stack = output
                .get_all_leaves_iter_mut()
                .filter(|value| matches!(value, Value::String(_)))
                .collect::<Vec<_>>();

            while let Some(top) = stack.pop() {
                // We need this for basically the duration of the program, so just leak
                // it and let the OS clean up after us.
                let string: &'static str =
                    Box::leak(Box::new(JsonString::unescape(&top.to_string()).to_string()));
                let (result, errs) = parse(string);
                *top = result;
                errs_from_recursive_parsing.push((top.to_string(), errs));

                // First value is top itself. We don't want infinite loops, so skip 1.
                for value in top.get_all_leaves_iter_mut().skip(1) {
                    if let Value::String(_) = value {
                        stack.push(value);
                    }
                }
            }
        }

        let mut stdout = stdout().lock();
        if *compact {
            stdout.write_all(to_bytes(&output).as_bytes()).ok();
        } else {
            stdout
                .write_all(&to_bytes_pretty_with_indent_str(
                    &output,
                    if *tab { "\t" } else { "  " },
                ))
                .ok();
        }
        stdout.write(&[b'\n', b'\n']).ok();
        stdout.flush().ok();
        errs_to_report = errs;
    }

    if !errs_to_report.is_empty() {
        let err_out = errs_to_report
            .into_iter()
            .map(|err| format!("{}", err))
            .collect::<Vec<_>>()
            .join("\n");

        let mut stderr = stderr().lock();
        stderr.write_all(err_out.as_bytes()).ok();
        stderr.write_all(&[b'\n', b'\n']).ok();
        stderr.flush().ok();
    }

    if !errs_from_recursive_parsing.is_empty() {
        let err_out = errs_from_recursive_parsing
            .into_iter()
            .map(|(json, errs)| {
                if errs.is_empty() {
                    return None;
                }
                let mut report = "Found the following errors while parsing the string ".to_string();
                if json.len() > 70 {
                    report.push_str(&json[0..70]);
                    report.push_str("...");
                } else {
                    report.push_str(&json);
                }
                report.push_str("\n");
                for err in errs {
                    report.push_str(&format!("{}\n", err));
                }
                Some(report)
            })
            .flat_map(|opt| opt)
            .collect::<Vec<_>>()
            .join("\n");

        let mut stderr = stderr().lock();
        stderr.write_all(err_out.as_bytes()).ok();
        stderr.write_all(&[b'\n', b'\n']).ok();
        stderr.flush().ok();
    }

    Ok(())
}

pub fn read_buf_to_string<T: Read>(mut reader: BufReader<T>) -> Result<String, ExitCode> {
    let mut input = Vec::new();
    let mut line = Vec::new();
    // Ok(0) signals EOF
    while let Ok(1..) = reader.read_until(b'\n', &mut line) {
        if line.is_empty() {
            break;
        } else {
            for byte in line {
                input.push(byte);
            }
        }
        line = Vec::new();
    }

    if input.is_empty() {
        return Ok("".to_string());
    }
    let parsed_input;
    match input[0] {
        0xEF => {
            if input.len() < 3 || input[1] != 0xBB || input[2] != 0xBF {
                stderr().write(b"\nFirst byte was 0xEF, which was expected to signify UTF-8 encoding, but UTF-8 Byte Order Mark was not found.").ok();
                return Err(ExitCode::FAILURE);
            }

            match String::from_utf8(input) {
                Err(err) => {
                    stderr()
                        .write(
                            format!("Failed to parse UTF-8 string with error: {}", err).as_bytes(),
                        )
                        .ok();
                    return Err(ExitCode::FAILURE);
                }
                Ok(str) => {
                    parsed_input = str;
                }
            }
        }
        0xFE => {
            if input.len() < 2 || input[1] != 0xFF {
                stderr().write(b"\nFirst byte was 0xFE, which was expected to signify UTF-16 Big Endian encoding, but UTF-16 Big Endian Byte Order Mark was not found.").ok();
                return Err(ExitCode::FAILURE);
            }
            match String::from_utf16be(&input) {
                Ok(str) => parsed_input = str,
                Err(err) => {
                    stderr().write(format!("Found Byte Order Mark signifying UTF-16 Big Endian encoding in input, but failed to parse as UTF-16 BE. Message: {}", err).as_bytes()).ok();
                    return Err(ExitCode::FAILURE);
                }
            }
        }
        0xFF => {
            if input.len() < 2 || input[1] != 0xFE {
                stderr().write(b"\nFirst byte was 0xFF, which was expected to signify UTF-16 Little Endian encoding, but UTF-16 Little Endian Byte Order Mark was not found.").ok();
                return Err(ExitCode::FAILURE);
            }
            parsed_input = match String::from_utf16le(&input) {
                Ok(str) => str,
                Err(err) => {
                    stderr().write(format!("Found Byte Order Mark signifying UTF-16 Little Endian encoding in input, but failed to parse as UTF-16 LE. Message: {}", err).as_bytes()).ok();
                    return Err(ExitCode::FAILURE);
                }
            };
        }
        _ => match String::from_utf8(input) {
            Ok(str) => parsed_input = str,
            Err(err) => {
                stderr().write(format!("\nNo known Byte Order Mark was found, so input was parsed as UTF-8. The string was not valid UTF-8. Message: {}", err).as_bytes()).ok();
                return Err(ExitCode::FAILURE);
            }
        },
    }

    Ok(parsed_input)
}

pub(crate) fn format_colored(
    json: &str,
    options: Option<FormatOptions<'_>>,
) -> (String, Vec<Box<dyn Error>>) {
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
                match token.kind() {
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

                        let colored_bracket = match indent % 3 {
                            0 => "{".magenta(),
                            1 => "{".green(),
                            2 => "{".yellow(),
                            _ => unreachable!(),
                        };

                        indent += 1;
                        result.push_str(&format!("{}", colored_bracket));
                    }
                    JsonTokenKind::ObjectEnd => {
                        if indent > 0 {
                            indent -= 1;
                        }

                        let colored_bracket = match indent % 3 {
                            0 => "}".magenta(),
                            1 => "}".green(),
                            2 => "}".yellow(),
                            _ => unreachable!(),
                        };

                        if let Some(JsonTokenKind::ObjectStart) = previous {
                            result.push_str(&format!("{}", colored_bracket));
                        } else {
                            if !compact_mode {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                            result.push_str(&format!("{}", colored_bracket));
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

                        let colored_bracket = match indent % 3 {
                            0 => "[".magenta(),
                            1 => "[".green(),
                            2 => "[".yellow(),
                            _ => unreachable!(),
                        };

                        indent += 1;
                        result.push_str(&format!("{}", colored_bracket));
                    }
                    JsonTokenKind::ArrayEnd => {
                        if indent > 0 {
                            indent -= 1;
                        }

                        let colored_bracket = match indent % 3 {
                            0 => "]".magenta(),
                            1 => "]".green(),
                            2 => "]".yellow(),
                            _ => unreachable!(),
                        };

                        if let Some(JsonTokenKind::ArrayStart) = previous {
                            result.push_str(&format!("{}", colored_bracket));
                        } else {
                            if !compact_mode {
                                result.push('\n');
                                for _ in 0..indent {
                                    result.push_str(indent_str);
                                }
                            }
                            result.push_str(&format!("{}", colored_bracket));
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
                        result.push_str(&format!("{}", &json[token.span().as_range()].cyan()));
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
                        result.push_str(&format!("{}", &json[token.span().as_range()].green()))
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
                        result.push_str(&format!("{}", "true".green()))
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
                        result.push_str(&format!("{}", "false".green()))
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
                        result.push_str(&format!("{}", "null".green()));
                    }
                }
                // we only need this for whitespace decisions, so skip if in compact mode
                if !compact_mode {
                    previous = Some(token.kind());
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

pub fn to_bytes(value: &Value) -> String {
    let mut result = Vec::new();
    to_string_colorized(value, &mut result, false, 0, "");
    unsafe { String::from_utf8_unchecked(result) }
}

pub fn to_bytes_pretty(value: &Value) -> Vec<u8> {
    let mut result = Vec::new();
    to_string_colorized(&value, &mut result, true, 0, "  ");
    result
}

pub fn to_bytes_pretty_with_indent_str(value: &Value, indent_str: &str) -> Vec<u8> {
    let mut result = Vec::new();
    to_string_colorized(value, &mut result, true, 0, indent_str);
    result
}

fn to_string_colorized(
    value: &Value,
    buf: &mut Vec<u8>,
    pretty: bool,
    indent_level: usize,
    indent_str: &str,
) {
    match value {
        Value::Null => {
            buf.extend_from_slice(&format!("{}", "null".green()).as_bytes());
        }
        Value::Bool(bool) => {
            if *bool {
                buf.extend_from_slice(&format!("{}", "true".green()).as_bytes());
            } else {
                buf.extend_from_slice(&format!("{}", "false".green()).as_bytes());
            }
        }
        Value::Number(num) => {
            buf.extend_from_slice(format!("{}", num.sanitized().green()).as_bytes());
        }
        Value::String(str) => {
            to_string_for_string(str, buf);
        }
        Value::Array(vec) => {
            let colored_bracket = match indent_level % 3 {
                0 => "[".magenta(),
                1 => "[".green(),
                2 => "[".yellow(),
                _ => unreachable!(),
            };
            buf.extend_from_slice(format!("{}", colored_bracket).as_bytes());

            for (i, item) in vec.iter().enumerate() {
                if i != 0 {
                    buf.push(b',');
                }
                if pretty {
                    buf.push(b'\n');
                    for _ in 0..indent_level + 1 {
                        buf.extend_from_slice(indent_str.as_bytes());
                    }
                }
                to_string_colorized(item, buf, pretty, indent_level + 1, indent_str);
            }

            if pretty && vec.len() > 0 {
                buf.push(b'\n');
                for _ in 0..indent_level {
                    buf.extend_from_slice(indent_str.as_bytes());
                }
            }

            let colored_bracket = match indent_level % 3 {
                0 => "]".magenta(),
                1 => "]".green(),
                2 => "]".yellow(),
                _ => unreachable!(),
            };
            buf.extend_from_slice(format!("{}", colored_bracket).as_bytes());
        }
        Value::Object(obj) => {
            let colored_bracket = match indent_level % 3 {
                0 => "{".magenta(),
                1 => "{".green(),
                2 => "{".yellow(),
                _ => unreachable!(),
            };
            buf.extend_from_slice(format!("{}", colored_bracket).as_bytes());

            for (i, cow) in obj.keys_in_order_found().enumerate() {
                if i != 0 {
                    buf.push(b',');
                }
                if pretty {
                    buf.push(b'\n');
                    for _ in 0..indent_level + 1 {
                        buf.extend_from_slice(indent_str.as_bytes());
                    }
                }
                let key = obj
                    .get_map()
                    .get_key_value(&JsonString::from(cow.clone()))
                    .expect("BUG: key to be in the object")
                    .0;

                buf.extend_from_slice(format!("{}", format!("\"{}\"", key.sanitized()).cyan()).as_bytes());

                buf.push(b':');
                if pretty {
                    buf.push(b' ');
                }

                let value = obj.get_map().get(&JsonString::from(cow.clone())).expect("BUG: values in the keys in found order vec should always be in the object hashmap as well.");
                to_string_colorized(value, buf, pretty, indent_level + 1, indent_str);
            }

            if pretty && obj.keys_in_order_found().len() > 0 {
                buf.push(b'\n');
                for _ in 0..indent_level {
                    buf.extend_from_slice(indent_str.as_bytes());
                }
            }

            let colored_bracket = match indent_level % 3 {
                0 => "}".magenta(),
                1 => "}".green(),
                2 => "}".yellow(),
                _ => unreachable!(),
            };

            buf.extend_from_slice(&format!("{}", colored_bracket).as_bytes());
        }
    }
}

fn to_string_for_string(str: &JsonString, buf: &mut Vec<u8>) {
    buf.extend_from_slice(format!("{}", format!("\"{}\"", str.sanitized()).yellow()).as_bytes());
}
