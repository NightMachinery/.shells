#!/usr/bin/env node

const input = process.argv[2];

const normalizeUrl = require('normalize-url');

res = normalizeUrl(input);
console.log(res);
