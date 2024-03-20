#!/usr/bin/env scriptisto

// scriptisto-begin
// script_src: src/main.rs
// build_cmd: cargo build --release
// target_bin: ./target/release/unique_by
// files:
//  - path: Cargo.toml
//    content: |
//     [package]
//     name = "unique_by"
//     version = "0.1.0"
//     edition = "2018"
//     [dependencies]
//     clap = "3.0"
//     regex = "1"
// scriptisto-end

// @GPT4T

use clap::{App, Arg};
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead};
use std::fs::File;

fn main() -> io::Result<()> {
    let matches = App::new("unique_by")
        .version("0.1")
        .author("Your Name")
        .about("Filters input lines based on a unique key extracted from each line and retains specified indices of occurrences")
        .arg(Arg::with_name("INPUT")
             .help("Sets the input file to use")
             .required(false)
             .index(1))
        .arg(Arg::with_name("pattern")
             .short('e')
             .long("pattern")
             .takes_value(true)
             .required(true)
             .help("Sets the regex pattern to extract the key. All the capture groups will become the key."))
        .arg(Arg::with_name("retain")
             .long("retain")
             .short('r')
             .takes_value(true)
             .required(false)
             .default_value("0")
             .help("Sets the comma-separated indices of the occurrences to retain for each unique key. Example: --retain=-1"))
        .get_matches();

    let stdin = io::stdin();
    let reader: Box<dyn BufRead> = match matches.value_of("INPUT") {
        Some(input_file) => Box::new(io::BufReader::new(File::open(input_file)?)),
        None => Box::new(stdin.lock()),
    };

    let pattern = matches.value_of("pattern").unwrap();
    let key_pat = Regex::new(pattern).expect("Invalid regex pattern");

    let retain_str = matches.value_of("retain").unwrap();
    let retain_indices: Vec<isize> = retain_str.split(',')
                                               .map(|s| s.parse::<isize>().expect("Invalid index"))

                                               .collect();

    // 1. Read all lines and store them in a vector.
    // 2. Extract keys from each line and store the indices of occurrences in a HashMap.
    // 3. Iterate through the HashMap and determine which occurrences should be retained based on the retain_indices.
    // 4. Create a HashSet of indices to be retained.
    // 5. Iterate through the original lines vector and print the lines whose indices are in the HashSet.
    let mut occurrences: HashMap<String, Vec<usize>> = HashMap::new();
    let mut lines: Vec<String> = Vec::new();

    let mut output_indices: HashSet<usize> = HashSet::new();

    for (i, line) in reader.lines().filter_map(|line| line.ok()).enumerate() {
        if let Some(caps) = key_pat.captures(&line) {
            // Collect all captured groups into a single String key, using the null character as a separator
            let key = caps.iter().skip(1) // Skip the entire match, start with the first capture group

                .filter_map(|c| c.map(|m| m.as_str()))
                .collect::<Vec<&str>>()

                .join("\0"); // Join all captures with the null character

            let entry = occurrences.entry(key).or_insert_with(Vec::new);
            entry.push(i); // Store the index of occurrence
        }
        // if let Some(caps) = key_pat.captures(&line) {
        //     if let Some(key) = caps.get(1) {
        //         let entry = occurrences.entry(key.as_str().to_string()).or_insert_with(Vec::new);
        //         entry.push(i); // Store the index of occurrence
        //     }
        else {
            // We'll retain lines that do NOT match the pattern.
            output_indices.insert(i);
        }

        lines.push(line); // Store the line itself
    }

    for (_, indices) in occurrences {
        for &index in &retain_indices {
            if index >= 0 {
                if let Some(&occurrence) = indices.get(index as usize) {
                    output_indices.insert(occurrence);
                }
            } else {
                let reverse_index = (-index - 1) as usize;
                if let Some(&occurrence) = indices.iter().rev().nth(reverse_index) {
                    output_indices.insert(occurrence);
                }
            }
        }
    }

    // Output the lines that should be retained, in the original order
    for (i, line) in lines.into_iter().enumerate() {

        if output_indices.contains(&i) {
            println!("{}", line);
        }
    }

    Ok(())
}
