#!/usr/bin/env node

const program = require('commander');
var randomColor = require('randomcolor');

program.option('-u, --hue <hue>', 'Controls the hue of the generated color. You can pass a string representing a color name: red, orange, yellow, green, blue, purple, pink and monochrome are currently supported. If you pass a hexidecimal color string such as #00FFFF, randomColor will extract its hue value and use that to generate colors.', 'random');
program.option('-l, --luminosity <luminosity>', 'Controls the luminosity of the generated color. You can specify a string containing bright, light or dark.', 'random');
// program.option('-c, --count <count>', 'An integer which specifies the number of colors to generate.', '1');
// program.option('-s, --seed <seed>', 'An integer or string which when passed will cause randomColor to return the same color each time. The default value is generated using Math.random.', Math.random().toString());
program.option('-f, --format <format>', 'A string which specifies the format of the generated color. Possible values are rgb, rgba, rgbArray, hsl, hsla, hslArray and hex.', 'rgb');
program.option('-a, --alpha <alpha>', ' A decimal between 0 and 1. Only relevant when using a format with an alpha channel (rgba and hsla).', '1');

program.parse(process.argv);

console.log(JSON.stringify(randomColor({
    hue: program.hue,
    // count: program.count,
    luminosity: program.luminosity,
    format: program.format,
    // seed: program.seed,
    alpha: program.alpha
})));
