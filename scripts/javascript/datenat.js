#!/usr/bin/env node
/* tests
gdate --date="@$(datenat_unix=y datenat.js 'fri 6:12 AM')"
*/

try {
    const chrono = require('chrono-node');
    const moment = require('moment');

    const input = process.argv[2];
    const nopast = process.env.datenat_nopast;
    const unixMode = process.env.datenat_unix;
    const hardCodeTime = process.env.datenat_hardcode_time;

    var currentTime = new Date();
    if (hardCodeTime) {
        currentTime.setUTCHours(9,29,59,0)
		// equivalent to '12:59:59' in Iran's timezone
        // This value is being used by [agfi:h-unix-allday-p]
    }

    var res
    if (nopast) {
        res = chrono.parseDate(input, currentTime, { forwardDate: true })
    } else {
        res = chrono.parseDate(input, currentTime)
    }
    if (!res) {
        console.error("Failed to parse a date from the input '" + input + "'")
        process.exit(1)
    }
    if (nopast) {
        tmp = new Date(res)
        tmp.setHours(0,0,0,0)
        var resM = moment(tmp)
        if (resM.diff(currentTime) <= 0) {
            console.error("The requested date is in the past.")
            process.exit(1)
        }
    }
    var resStr
    if (unixMode) {
        resStr = String(Math.floor(res.valueOf() / 1000))
    } else {
        var resM = moment(res)
        resStr = resM.format("YYYY/MM/DD")
    }
    console.log(resStr);
}
catch(err) {
    console.error("datenat.js (Probably the parsing has failed): " + err.message)
    process.exit(1)
}
