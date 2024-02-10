#![feature(str_from_utf16_endian)]
use std::{io::{stderr, stdin, BufReader, Read, Write}, process::ExitCode};

use clap::{command, ArgMatches};

pub fn build_command(name: &'static str) -> ArgMatches {
    return command!(name).get_matches();
}

pub fn parse_std_in() -> Result<String, ExitCode> {
    let mut input = Vec::new();
    match BufReader::new(stdin().lock()).read_to_end(&mut input) {
        Ok(_) => {}
        Err(err) => {
            stderr().write(format!("\n{}", err).as_bytes()).expect("Failed to write to standard error");
        }
    }

    if input.len() == 0 { return Ok("".to_string()); }
    let parsed_input;
    match input[0] {
        0xEF => {
            if input.len() < 3 || input[1] != 0xBB || input[2] != 0xBF { 
                stderr().write(b"\nFirst byte was 0xEF, which was expected to signify UTF-8 encoding, but UTF-8 Byte Order Mark was not found.").ok();
                return Err(ExitCode::FAILURE); 
            }

            match String::from_utf8(input) {
                Err(err) => { 
                    stderr().write(format!("Failed to parse UTF-8 string with error: {}", err).as_bytes()).ok();
                    return Err(ExitCode::FAILURE); 
                }
                Ok(str) => { parsed_input = str; }
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
                return Err(ExitCode::FAILURE)
            }
            parsed_input = match String::from_utf16le(&input) {
                Ok(str) => str,
                Err(err) => {
                    stderr().write(format!("Found Byte Order Mark signifying UTF-16 Little Endian encoding in input, but failed to parse as UTF-16 LE. Message: {}", err).as_bytes()).ok();
                    return Err(ExitCode::FAILURE);
                }
            };
        }
        _ => {
            match String::from_utf8(input) {
                Ok(str) => parsed_input = str,
                Err(err) => {
                    stderr().write(format!("\nNo known Byte Order Mark was found, so input was parsed as UTF-8. The string was not valid UTF-8. Message: {}", err).as_bytes()).ok();
                    return Err(ExitCode::FAILURE);
                }
            }
        }
    }

    return Ok(parsed_input);
}