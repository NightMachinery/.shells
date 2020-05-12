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

// https://users.rust-lang.org/t/count-trailing-zero-bytes-of-a-binary-file/42503/4

use std::env;
use std::fs;

fn main() {
    let filename = env::args().nth(1).unwrap();
    let buffer = fs::read(filename).unwrap();
    let count = buffer.iter().rev().take_while(|b| **b == 0).count();
    println!("{}", count);
}
