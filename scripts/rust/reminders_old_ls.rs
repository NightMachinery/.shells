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
//     walkdir = "2"
//     regex = "1"
//     clap = "2"
// scriptisto-end

use std::env;
use std::path::Path;
use walkdir::WalkDir;
use regex::Regex;
use clap::{App, Arg};

fn main() {
    // Set up CLI arguments using clap.
    let matches = App::new("Reminday script")
        .arg(
            Arg::with_name("end-pattern")
                .long("end-pattern")
                .takes_value(true)
                .help("Specifies the file end pattern."),
        )
        .get_matches();
    // Retrieve the --end-pattern CLI argument or use the default pattern.
    let end_pattern = matches.value_of("end-pattern").unwrap_or(r"\.(md|org|txt|zsh)");

    // Retrieve the remindayRootDir environment variable.
    let reminday_root_dir = env::var("remindayRootDir").expect("remindayRootDir is not set!");

    // Retrieve the directories to exclude from environment variables.
    let reminday_bak_c_dir = env::var("remindayBakCDir").expect("remindayBakCDir is not set!");
    let reminday_bak_dir = env::var("remindayBakDir").expect("remindayBakDir is not set!");

    // Retrieve today's date from the environment variable `datej`.
    let today = env::var("datej").expect("datej is not set!");
    let date_regex = Regex::new(r"(\d+)/(\d+)/(\d+)").unwrap();
    let caps = date_regex.captures(&today).unwrap();
    let year: i32 = caps[1].parse().unwrap();
    let month: i32 = caps[2].parse().unwrap();
    let day: i32 = caps[3].parse().unwrap();

    let file_regex_str = format!(r"/(?P<year>\d+)/(?P<month>\d+)/(?P<day>\d+)[^/]*{}", end_pattern);
    let file_regex = Regex::new(&file_regex_str).unwrap();

    for entry in WalkDir::new(reminday_root_dir).into_iter().filter_map(|e| e.ok()) {
        let path_str = entry.path().to_str().unwrap();

        if path_str.starts_with(&reminday_bak_c_dir) || path_str.starts_with(&reminday_bak_dir) {
            continue;  // Skip the excluded directories
        }

        if let Some(caps) = file_regex.captures(path_str) {
            let file_year: i32 = caps["year"].parse().unwrap();
            let file_month: i32 = caps["month"].parse().unwrap();
            let file_day: i32 = caps["day"].parse().unwrap();

            if file_year < year
                || (file_year == year && file_month < month)
                || (file_year == year && file_month == month && file_day < day) {
                println!("{}", path_str);
            }
        }
    }
}
