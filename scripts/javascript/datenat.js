#!/usr/bin/env node
/* tests
  gdate --date="@$(datenat_unix=y datenat.js 'fri 6:12 AM')"
*/

try {
    const chrono = require('chrono-node');
    //: moment is a legacy project in maintenance mode, and it was here only for
    //: one format call and one comparison, both one-liners on a native Date.

    const input = (process.argv[2] || '').trim();
    const nopast = process.env.datenat_nopast;
    const unixMode = process.env.datenat_unix;
    const hardCodeTime = process.env.datenat_hardcode_time;
    const strict = process.env.datenat_strict;

    var currentTime = new Date();
    if (hardCodeTime) {
        currentTime.setUTCHours(9,29,59,0)
        // equivalent to '12:59:59' in Iran's timezone
        // This value is being used by [agfi:h-unix-allday-p]
    }

    // Timezones are also supported. See [[https://www.npmjs.com/package/chrono-node][chrono-node - npm]]
    //
    // parse() rather than parseDate(): parseDate is parse()[0].start.date(), so
    // it throws away the match span and the field-certainty flags. Those are
    // exactly what catches chrono's silent misparses, and datenat_strict=y
    // turns them into errors. Two classes, both verified on 2.7.0:
    //
    //   partial match:  "1h:30m later" matches only "30m later" at index 3, so
    //                   the 1h: is dropped and you get thirty minutes.
    //   invented hour:  "next friday" matches fully but names no time of day,
    //                   and chrono fills in 12:00. "tomorrow" carries the
    //                   current clock time instead.
    //
    // Both return a plausible time and no error, which for an alarm is the
    // worst possible outcome. Off by default: callers passing free prose rely
    // on partial matching, and only the alarm path wants the strictness.
    const opts = nopast ? { forwardDate: true } : {};
    const results = chrono.parse(input, currentTime, opts);
    if (!results.length) {
        console.error("Failed to parse a date from the input '" + input + "'")
        process.exit(1)
    }
    const first = results[0];
    if (strict) {
        if (results.length > 1 || first.index !== 0 || first.text.length !== input.length) {
            console.error("datenat.js: refusing a partial parse of '" + input +
                "': chrono understood only '" + first.text + "'")
            process.exit(1)
        }
        if (!first.start.isCertain('hour')) {
            console.error("datenat.js: '" + input +
                "' names no time of day, and chrono would invent one. Add a time, e.g. '" +
                input + " 9am'.")
            process.exit(1)
        }
    }
    var res = first.start.date()
    if (nopast) {
        tmp = new Date(res)
        tmp.setHours(0,0,0,0)
        if (tmp.getTime() - currentTime.getTime() <= 0) {
            console.error("The requested date is in the past.")
            process.exit(1)
        }
    }
    var resStr
    if (unixMode) {
        resStr = String(Math.floor(res.valueOf() / 1000))
    } else {
        const pad = (n) => String(n).padStart(2, "0")
        resStr = res.getFullYear() + "/" + pad(res.getMonth() + 1) + "/" + pad(res.getDate())
    }
    console.log(resStr);
}
catch(err) {
    console.error("datenat.js (Probably the parsing has failed): " + err.message)
    process.exit(1)
}
