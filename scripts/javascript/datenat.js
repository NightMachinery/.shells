#!/usr/bin/env node

try {
    const chrono = require('chrono-node');
    const moment = require('moment');

    const input = process.argv[2];

    var currentTime = new Date();
    var res = chrono.parseDate(input, currentTime, { forwardDate: true })
    res.setHours(0,0,0,0)
    var resM = moment(res)
    if (resM.diff(currentTime) <= 0) {
        console.error("The requested date is in the past.")
        process.exit(1)
    }
    var resStr = resM.format("YYYY/MM/DD")
    console.log(resStr);
}
catch(err) {
    console.error("datenat.js (Probably the parsing has failed): " + err.message)
    process.exit(1)
}
