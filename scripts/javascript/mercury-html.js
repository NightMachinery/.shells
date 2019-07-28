#!/usr/bin/env node
// USAGE: url html-file output-mode

// console.log(process.argv)
const Mercury = require('@postlight/mercury-parser');
var fs = require('fs');
 
var contents = fs.readFileSync(process.argv[3], 'utf8');

Mercury.parse(process.argv[2], {
  html:
    contents,
    contentType: process.argv[4] }).then(result => console.log(JSON.stringify(result)));
