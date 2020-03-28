#!/usr/bin/env node

const readTimeEstimate = require('read-time-estimate');
var fs = require('fs');
var doc = fs.readFileSync(process.stdin.fd, 'utf-8')
est = readTimeEstimate.default(doc, 200, 12, 500, ['img', 'Image']); // this '.default' wasn't in the original code
const {
    humanizedDuration, // 'less than a minute'
    duration, // 0.23272727272727273
    totalWords, // 9
    wordTime, // 0.03272727272727273
    totalImages, // 1
    imageTime, //  0.2
    otherLanguageTimeCharacters, // 6
    otherLanguageTime, // 0.012
} = est;
// Args:
// string
// Speed of reading the text in Words per Minute,
// Speed of reading the image in seconds
// Speed of reading the Chinese / Korean / Japanese characters in Characters per Minute
// Custom Image tags to parse in the input string

console.log(JSON.stringify(est));
