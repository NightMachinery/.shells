#!/usr/bin/env node
// @todo0 refactor this to a package
//
// still not as good as the mobile apps! :(
//
// deps:
// jpeg-js pngjs jsqr
///

var input_path = process.argv[2];
var input_path_lower = input_path.toLowerCase();

var fs = require("fs");
var fileData = fs.readFileSync(input_path);

if (input_path_lower.endsWith('.jpg') || input_path_lower.endsWith('.jpeg')) {
    // https://www.npmjs.com/package/jpeg-js#decoding-jpegs
    var jpeg = require('jpeg-js');
    var decodedData = jpeg.decode(fileData, {useTArray: true});
    // console.log(decodedData);

    var imageData = decodedData['data']
    var width = decodedData['width']
    var height = decodedData['height']
} else if (input_path_lower.endsWith('.png')) {
    // https://github.com/lukeapage/pngjs/blob/master/README.md#property-data
    PNG = require("pngjs").PNG;
    var decodedData = PNG.sync.read(fileData);

    var imageData = decodedData['data']
    var width = decodedData['width']
    var height = decodedData['height']
} else {
    console.error("qr_decode.js: Only JPG and PNG images are currently supported!")
    process.exit(1)
}

const jsQR = require("jsqr");

const code = jsQR(imageData, width, height);

if (code) {
    console.log(JSON.stringify(code));
}
else {
    console.error("qr_decode.js: could not decode")
    process.exit(1)
}
