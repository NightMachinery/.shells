#!/usr/bin/env scriptisto

// scriptisto-begin
// script_src: src/main.rs
// build_cmd: cargo build --release
// target_bin: ./target/release/script
// files:
//  - path: Cargo.toml
//    content: |
//     package = { name = "script", version = "0.1.0", edition = "2018"}
//     [dependencies]
// scriptisto-end

// @alt `sed '$ s/\x30*$//'`
// Specifically, $ stands for the last line. If you wanted to replace something in the 13th line, you would analogously write sed '13 s/foo/bar/'. Just for completeness sake, the other $ in the regex means "end of line" and has nothing to do with the first $.
// For more details, see here in the sed manual: https://www.gnu.org/software/sed/manual/sed.html#sed-addresses
//
// https://users.rust-lang.org/t/count-trailing-zero-bytes-of-a-binary-file/42503/4

use std::env;
use std::fs;

fn main() {
    let filename = env::args().nth(1).unwrap();
    let buffer = fs::read(filename).unwrap();
    let count = buffer.iter().rev().take_while(|b| **b == 0).count();
    println!("{}", count);
}
