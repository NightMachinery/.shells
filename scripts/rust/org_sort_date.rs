#!/usr/bin/env scriptisto


// scriptisto-begin
// script_src: src/main.rs
// build_cmd: cargo build --release
// target_bin: ./target/release/org_sort_by_jalali
// files:
//   - path: Cargo.toml
//     content: |
//       [package]
//       name = "org_sort_by_jalali"
//       version = "0.1.1"
//       edition = "2021"
//
//       [dependencies]
//       regex = "1"
//       icu_calendar = "2"
// scriptisto-end

use icu_calendar::{Date, Gregorian};
use regex::Regex;
use std::cmp::Ordering;
use std::io::{self, Read};

fn jalali_to_gregorian(year: i32, month: i32, day: i32) -> Option<Date<Gregorian>> {
    // Boost used 0-based months; ICU4X expects 1-based, so pass as-is.
    // Reject non-positive parts to mirror the C++ guard behavior.
    if year <= 0 || month <= 0 || day <= 0 {
        return None;
    }
    // Empty/non-numeric defaulted to 1 per your update.
    let y = year;
    let m = u8::try_from(month).ok()?;
    let d = u8::try_from(day).ok()?;

    let persian = Date::try_new_persian(y, m, d).ok()?;
    // Converting between calendars is algorithmic; no data provider needed here.
    Some(persian.to_calendar(Gregorian))
}

fn main() {
    // Read all stdin
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();

    // Split by top-level Org headings starting with "* " at the beginning of a line
    let root_heading_re = Regex::new(r"(?m)^\*\s+").unwrap();

    // Capture groups to mirror the C++: 1=whole link, 2=year, 3=month, 4=day
    let date_re = Regex::new(
        r#"(?i)(?:\[|<)((?:jalali):([^]/]+)(?:/([^]/]+))?(?:/([^]/]+))?(?:/([^]]+))?)(?:\]|>)"#,
    )
    .unwrap();

    let mut blocks: Vec<(String, Date<Gregorian>)> = Vec::new();

    for block in root_heading_re.split(&s) {
        let mut dates: Vec<Date<Gregorian>> = Vec::new();

        for caps in date_re.captures_iter(block) {
            // 1: full link string (unused), 2: year, 3: month, 4: day
            let year_s = caps.get(2).map(|m| m.as_str()).unwrap_or("");
            let month_s = caps.get(3).map(|m| m.as_str()).unwrap_or("");
            let day_s = caps.get(4).map(|m| m.as_str()).unwrap_or("");

            // Your update: default to 1 instead of 0 on parse failure.
            let year_i = year_s.parse::<i32>().unwrap_or(1);
            let month_i = month_s.parse::<i32>().unwrap_or(1);
            let day_i = day_s.parse::<i32>().unwrap_or(1);

            if let Some(g) = jalali_to_gregorian(year_i, month_i, day_i) {
                dates.push(g);
            }
        }

        if let Some(earliest) = dates.into_iter().min() {
            // Store the whole block; we'll re-add "* " on output exactly like the C++.
            blocks.push((block.to_string(), earliest));
        }
    }

    // Sort by the selected Gregorian date asc (like std::sort with a < comparator)
    blocks.sort_by(|a, b| {
        if a.1 < b.1 {
            Ordering::Less
        } else if a.1 > b.1 {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    });

    // Print each block prefixed by "* " (matching the original)
    for (block_text, _) in blocks {
        print!("* {}", block_text);
    }
}
