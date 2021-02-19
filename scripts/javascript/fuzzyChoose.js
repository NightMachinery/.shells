#!/usr/bin/env node

const FuzzySet = require('fuzzyset')
var fs = require("fs");
var choices = fs.readFileSync(0).toString().split("\n")
f = FuzzySet(choices)
input = process.argv[2]
res = f.get(input, "NULL", 0)
console.log(JSON.stringify(res))
