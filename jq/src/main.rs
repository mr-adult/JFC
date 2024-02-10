
use std::{io::{stderr, stdin, BufReader, Read, Write}, process::{exit, ExitCode, ExitStatus}};

fn main() -> Result<(), ExitCode> {
    let args = dq_core::build_command("jq");
    let input = dq_core::parse_std_in()?;

    

    return Ok(());
}
