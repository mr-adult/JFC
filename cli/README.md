                 ___  ________ ________     
                |\  \|\  _____\\   ____\    
                \ \  \ \  \__/\ \  \___|    
              __ \ \  \ \   __\\ \  \       
             |\  \\_\  \ \  \_| \ \  \____  
             \ \________\ \__\   \ \_______\
              \|________|\|__|    \|_______|

JSON Formatting CLI (JFC) is a CLI tool written in Rust for working with malformed JSON. It does its best to transform whatever text you input on standard in into valid JSON without dropping any meaningful characters from the input. Any errors found along the way are emitted to standard error. In raw mode, no characters from the input are (or at least should be) dropped. 

## Installation
To install JFC: 
1. Install the rust toolchain following the directions on [doc.rust-land.org](https://doc.rust-lang.org/cargo/getting-started/installation.html) to install cargo.
2. Run `cargo install jfc`
3. You can find the executable in the ~/.cargo/bin folder

## Usage
For usage, see the help (-h or --help menu).
