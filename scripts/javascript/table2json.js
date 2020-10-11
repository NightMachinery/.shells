#!/usr/bin/env node

// Fetch a URL and parse all it's tables into JSON, using promises
var tabletojson = require('tabletojson').Tabletojson;

if (process.argv.length >= 3) {
    const url = process.argv[2];
    tabletojson.convertUrl(url)
               .then(function(tablesAsJson) {
                   console.log(JSON.stringify(tablesAsJson))
               });
} else {
    (async () => {
        const getStdin = require('get-stdin');
        var html = await getStdin();
        var tablesAsJson = tabletojson.convert(html,
                                               {
                                                   // headings: ['CourseID','GroupID','Credits','Name','Prerequisites', 'Capacity','EnrolledStudents','Professor','ExamDate','ClassTime','Comments','Message'],
                                                   useFirstRowForHeadings: true
                                               }
                                              );
        console.log(JSON.stringify(tablesAsJson))
    })();
}
