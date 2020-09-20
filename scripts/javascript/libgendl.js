#!/usr/bin/env node

(async () => {
    const libgen = require("libgen")
    // const urlString = await libgen.mirror()
    // console.log(`${urlString} is currently fastest`)
    const query =  process.argv[2];
    const options = {
        mirror: 'http://gen.lib.rus.ec',
        query: query,
        count: 5,
        sort_by: 'year',
        reverse: true
    }

    try {
        const data = await libgen.search(options)
        console.log(JSON.stringify(data))
        // let n = data.length
        // console.log(`${n} results for "${options.query}"`)
        // while (n--){
        //     console.log('');
        //     console.log('Title: ' + data[n].title)
        //     console.log('Author: ' + data[n].author)
        //     console.log('Download: ' +
        //                 'http://gen.lib.rus.ec/book/index.php?md5=' +
        //                 data[n].md5.toLowerCase())
        // }
    } catch (err) {
        console.error(err)
    }
})();
