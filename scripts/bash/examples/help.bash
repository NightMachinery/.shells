#!/usr/bin/env bash
###
### my-script — does one thing well
###
### Usage:
###   my-script <input> <output>
###
### Options:
###   <input>   Input file to read.
###   <output>  Output file to write. Use '-' for stdout.
###   -h        Show this message.

help() {
    awk -F'### ' '/^###/ { print $2 }' "$0"
}

if [[ $# == 0 || "$1" == "-h" ]]; then
    help
    exit 1
fi

echo Hello World
