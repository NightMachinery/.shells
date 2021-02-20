#!/usr/bin/env node

var fs = require('fs');
var a = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
var b = JSON.parse(fs.readFileSync(process.argv[3], 'utf8'));

const marked_courses = JSON.parse((process.env.marked_courses) || '[]').map(x => x.toString())
// console.log(JSON.stringify(marked_courses))

///
// > "" == 0
// true
// > 10 < ""
// false
// > 10 == ""
// false
// > 10 > ""
// true
///

function course_summary(course) {
    // return course.Name + " (group: " + course.Group + ", id: " + course.CourseID + ", " + (course.Professor || "NA") + ", " + (course.ClassTime || "NA") + ")"
    return course.Name + " (group: " + course.Group + ", id: " + course.CourseID + ", " + (course.Professor || "استاد نامشخص") + "، " + (course.ClassTime || "زمان نامشخص") + ")"
}

for ( var i=0; i < b.length ; i++ ){
    bi = b[i]
    bid = bi.CourseID
    bn = course_summary(bi)

    ai = null
    for (var at in a) {
        at = a[at]
        if (bid == at.CourseID && bi.Group == at.Group) {
            ai = at
        }
    }
    if (ai == null) {
        console.log("ƪ " + bn + " is new:\n" + JSON.stringify(bi, null, 4) + "\n")
        continue
    }
}
for ( var i=0; i < a.length ; i++ ){
    ai = a[i]
    aid = ai.CourseID
    an = course_summary(ai)

    bi = null
    for (var bt in b) {
        bt = b[bt] // js sucks
        // console.log("bt id: " + JSON.stringify(bt,null,4))
        if (aid == bt.CourseID && ai.Group == bt.Group) {
            bi = bt
        }
    }
    if (bi == null) {
        console.log("WARN: " + an + " has changed its ID or group or the course has been deleted. (Skipping it.)")
        continue
    }

    bid = bi.CourseID
    bn = course_summary(bi)


    if (aid != bid) {
        console.log("WARN_IMPOSSIBLE: " + an + "'s id has changed from " + aid + " to " + bid)
        continue
    }
    if (ai.Name != bi.Name) {
        console.log("WARN: " + an + "'s name has changed to " + bn)
        continue
    }
    if (ai.Professor != bi.Professor) {
        console.log("WARN: " + an + "'s Professor has changed to " + bn)
        continue
    }
    if (ai.ClassTime != bi.ClassTime) {
        console.log("WARN: " + an + "'s class schedule has changed to " + bn)
        continue
    }

    ac = parseInt(ai.Capacity) || Infinity
    bc = parseInt(bi.Capacity) || Infinity

    ae = parseInt(ai.EnrolledStudents) || 0
    be = parseInt(bi.EnrolledStudents) || 0

    if (bc > ac) {
        // "ƪ " forces LTR in Telegram
        console.log("ƪ " + bn + "'s capacity increased from " + ac + " to " + bc + " (" + be + " students currently enrolled)" + "\n")
    }

    if (be < ae && (marked_courses.includes(aid.toString()) || ac == ae)) {
        console.log("ƪ " + bn + "'s enrolled students decreased from " + ae + " to " + be + " (current capacity: " + bc + ")" + "\n")
    }

}
