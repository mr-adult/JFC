#![feature(str_from_utf16_endian)]
use std::{io::{stderr, stdin, BufReader, Read, Write}, process::ExitCode};

use clap::{command, ArgMatches};

pub fn build_command(name: &'static str) -> ArgMatches {
    return command!(name).get_matches();
}

pub fn parse_std_in() -> Result<String, ExitCode> {
    todo!();
}