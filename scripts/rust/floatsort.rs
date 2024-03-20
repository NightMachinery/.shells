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
//     clap = "3.0"
//     regex = "1"
// scriptisto-end

// @GPT4T

use clap::{App, Arg};
use regex::Regex;
use std::io::{self, BufRead};
use std::fs::File;

fn main() -> io::Result<()> {
    let matches = App::new("floatsort")
        .version("0.1")
        .author("Feraidoon")
        .about("Sorts its input lines based on a number extracted from the given pattern")
        .arg(Arg::with_name("INPUT")
             .help("Sets the input file to use")
             .required(false)
             .index(1))
        .arg(Arg::with_name("sort-by-pattern")
             .short('e') // 'e' is a char, not a string (like "e")
             .long("sort-by-pattern")
             .takes_value(true)
             .help("Sets the regex pattern to sort by. The first capture group should be the number. The default is a pattern to extract the last floating point number on the line."))
        .get_matches();

    let stdin = io::stdin();
    let reader: Box<dyn BufRead> = match matches.value_of("INPUT") {
        Some(input_file) => Box::new(io::BufReader::new(File::open(input_file)?)),
        None => Box::new(stdin.lock()),
    };

    let default_pattern = r"([-+]?\d+(?:\.\d*)?(?:[eE][-+]?\d+)?)\D*$";
    let pattern = matches.value_of("sort-by-pattern").unwrap_or(default_pattern);
    let float_pat = Regex::new(pattern).expect("Invalid regex pattern");

    let mut lines: Vec<(f64, String)> = reader.lines()
        .filter_map(|line| line.ok())
        .map(|line| {
            let num = float_pat.captures(&line)
                .and_then(|cap| cap.get(1))
                .and_then(|m| m.as_str().parse::<f64>().ok())
                .unwrap_or(f64::NEG_INFINITY); // Use NEG_INFINITY if no match is found

            (num, line)
        })
        .collect();

    // Sort by the extracted number
    // @GPT4T Stable Sort
    lines.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));

    // Print the lines
    for (_, line) in lines {
        println!("{}", line);
    }

    Ok(())
}
