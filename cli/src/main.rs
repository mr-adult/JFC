#![feature(str_from_utf16_endian)]
#![doc = include_str!("../README.md")]

use std::{
    fs::OpenOptions, io::{stderr, stdin, stdout, BufRead, BufReader, Read, Write}, process::ExitCode
};

use clap::{command, Arg, ArgAction};
use toy_json_formatter::{format, parse, FormatOptions};

fn main() -> Result<(), ExitCode> {
    let arg_matches = command!()
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
            Arg::new("tab")
                .long("tab")
                .action(ArgAction::SetTrue)
                .help("use tabs for indentation (default: 2 spaces)"),
        )
        .after_help(
            r#"
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
"#,
        )
        .get_matches();

    let compact = arg_matches.get_one("compact").unwrap_or(&false);
    let raw = arg_matches.get_one("raw").unwrap_or(&false);
    let empty_string = String::with_capacity(0);                                                                                                    
    let file = arg_matches.get_one::<String>("file").unwrap_or(&empty_string);
    
    let tab = arg_matches.get_one::<bool>("tab").unwrap_or(&false);
    let indent_str = if *tab { "\t" } else { "  " };
   
    let input = if file.is_empty() {
        let reader = BufReader::new(stdin().lock());                                                                                     
        read_buf_to_string(reader)?
    } else {
        match OpenOptions::new()
            .read(true)
            .write(false)
            .open(file) {
                Ok(file) => {
                    let reader = BufReader::new(file);
                    read_buf_to_string(reader)?
                } Err(err) => {
                    let mut stderr = stderr().lock();
                    stderr.write_all(format!("Failed to open file '{}'. Message: {}", file, err).as_bytes()).ok();
                    stderr.write(&[b'\n']).ok();
                    stderr.flush().ok();
                    return Err(ExitCode::FAILURE)
                }
            }
    };

    let errs_to_report;
    if *raw {
        let (output, errs) = format(&input, Some(FormatOptions { compact: *compact, indent_str }));
        let mut stdout = stdout().lock();
        stdout.write_all(output.as_bytes()).ok();
        stdout.write(&[b'\n', b'\n']).ok();
        stdout.flush().ok();
        errs_to_report = errs;
    } else {
        let (output, errs) = parse(&input);
        let mut stdout = stdout().lock();
        if *compact {
            stdout.write_all(output.to_string().as_bytes()).ok();
        } else {
            stdout.write_all(output.to_string_pretty().as_bytes()).ok();
        }
        stdout.write(&[b'\n', b'\n']).ok();
        stdout.flush().ok();
        errs_to_report = errs;
    }

    if !errs_to_report.is_empty() {
        let err_out = errs_to_report.into_iter().map(|err| format!("{}", err)).collect::<Vec<_>>().join("\n");
        let mut stderr = stderr().lock();
        stderr.write_all(err_out.as_bytes()).ok();
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
