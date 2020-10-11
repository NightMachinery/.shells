#!/usr/bin/env node

// Add ES6 polyfills.
require('babel-polyfill')

var diff = require('rus-diff').diff

var fs = require('fs');
var a = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
var b = JSON.parse(fs.readFileSync(process.argv[3], 'utf8'));

// console.log(diff(a, b, options = {inc: true})) // is buggy
console.log(diff(a, b))
