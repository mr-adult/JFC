#![feature(str_from_utf16_endian)]

use std::{
    io::{stderr, stdin, BufReader, Read, Write},
    process::ExitCode,
};

use clap::{crate_authors, crate_version, Arg, ArgAction, Command};
use serde_json::Value;

fn main() -> Result<(), ExitCode> {
    let command = Command::new("rq")
        .author(crate_authors!())
        .version(crate_version!())
        .about("A rust-based clone of the popular jq CLI tool.")
        .arg(
            Arg::new("compact")
                .long("compact")
                .short('c')
                .help("compact instead of pretty-printed output"),
        )
        .arg(
            Arg::new("null")
                .short('n')
                .help("use `null` as the single input value"),
        )
        .arg(
            Arg::new("exit")
                .short('e')
                .help("set the exit status code based on the output"),
        )
        .arg(
            Arg::new("slurp")
                .short('s')
                .help("read (slurp) all inputs into an array; apply filter to it"),
        )
        .arg(
            Arg::new("write_raw")
                .short('r')
                .help("output raw strings, not [file format] texts"),
        )
        .arg(
            Arg::new("read_raw")
                .short('R')
                .help("read raw strings, not [file format] texts"),
        )
        .arg(
            Arg::new("colorize")
                .short('C')
                .help("colorize [file format]"),
        )
        .arg(
            Arg::new("monochrome")
                .short('M')
                .help("monochrome (don't colorize [file format])"),
        )
        .arg(
            Arg::new("sort")
                .short('S')
                .help("sort keys on objects on output"),
        )
        .arg(Arg::new("tab").long("tab").help("use tabs for indentation"))
        .arg(Arg::new("file format"))
        .arg(Arg::new("filter"))
        .arg(
            Arg::new("last")
                .last(true)
                .help("terminates argument processing"),
        )
        .after_help(
            r#"
rq is a rust-based tool for processing various inputs, applying the 
given filter to the text inputs and producing the filter's results in
the same format on standard output.

The simplest filter is ., which copies rq's input to its output
unmodified (except for formatting, but note that IEEE754 is used
for number representation internally, with all that that implies).

For more advanced filters see the jq(1) manpage ("man jq")
and/or https://stedolan.github.io/jq

Example:

    $ echo '{"foo": 0}' | rq j .
    {
        "foo": 0
    }
"#,
        )
        .get_matches();

    println!(
        "filter: {:?}",
        command
            .get_one::<String>("filter")
            .unwrap_or(&".".to_string())
    );

    return Ok(());
    let input = read_stdin_to_string()?;

    let parsed: Value = match serde_json::from_str(&input) {
        Err(err) => {
            stderr()
                .write(format!("Failed to parse JSON. Message: {}", err).as_bytes())
                .ok();
            return Err(ExitCode::FAILURE);
        }
        Ok(value) => value,
    };

    return Ok(());
}

pub fn read_stdin_to_string() -> Result<String, ExitCode> {
    let mut input = Vec::new();
    match BufReader::new(stdin().lock()).read_to_end(&mut input) {
        Ok(_) => {}
        Err(err) => {
            stderr()
                .write(format!("\n{}", err).as_bytes())
                .expect("Failed to write to standard error");
        }
    }

    if input.len() == 0 {
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

    return Ok(parsed_input);
}
