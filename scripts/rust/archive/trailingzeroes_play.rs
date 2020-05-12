#!/bin/sh
#![allow()] /*
            exec cargo-play --cached --release $0 -- "$@"
                        */

use std::env;
use std::fs::File;
// use std::io::BufReader;
use std::io::Read;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let mut f = File::open(filename).unwrap();
    // let f = BufReader::new(f);
    // let f = f.collect().rev();

    let mut buffer = Vec::new();
    // read the whole file
    f.read_to_end(&mut buffer).unwrap();

    let mut count = 0;
    for b in (&buffer).iter().rev() {
        if b == &0 {
            count += 1
        } else {
            // println!("{}", b);
            break;
        }
    }
    println!("{}", count);
}
