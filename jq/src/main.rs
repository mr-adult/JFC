use std::{io::{stderr, Write}, process::ExitCode};

use serde_json::Value;

fn main() -> Result<(), ExitCode> {
    let args = dq_core::build_command("jq");
    let input = dq_core::parse_std_in()?;

    let parsed: Value = match serde_json::from_str(&input) {
        Err(err) => {
            stderr().write(format!("Failed to parse JSON. Message: {}", err).as_bytes()).ok();
            return Err(ExitCode::FAILURE)
        }
        Ok(value) => value,
    };

    return Ok(());
}
