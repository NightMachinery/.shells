#!/usr/bin/env node

// https://github.com/brandon93s/html-table-to-json
// https://github.com/brandon93s/html-table-to-json/commit/01259ccf231a9256e82d7341d61e3079104c9f2e
const HtmlTableToJson = require('html-table-to-json');
(async () => {
    const getStdin = require('get-stdin');
    var html = await getStdin();
    res = HtmlTableToJson.parse(html, { values: true });
    console.log(JSON.stringify(res.results))
})();
