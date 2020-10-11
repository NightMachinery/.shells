#!/usr/bin/env node

var fs = require('fs');
var a = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
var b = JSON.parse(fs.readFileSync(process.argv[3], 'utf8'));

const marked_courses = JSON.parse((process.env.marked_courses) || '[]').map(x => x.toString())
// console.log(JSON.stringify(marked_courses))

for ( var i=0; i < a.length ; i++ ){
    aid = a[i].CourseID
    bid = b[i].CourseID

    if (aid != bid) {
        console.log("ERROR: " + a[i].Name + "'s id has changed from " + aid + " to " + bid)
        continue
    }

    ac = a[i].Capacity
    bc = b[i].Capacity

    if (bc > ac) {
        console.log(a[i].Name + "'s capacity increased from " + ac + " to " + bc)
    }

    ae = a[i].EnrolledStudents
    be = b[i].EnrolledStudents

    if (be < ae && (marked_courses.includes(aid.toString()) || ac == ae)) {
        console.log(a[i].Name + "'s enrolled students decreased from " + ae + " to " + be)
    }

}
