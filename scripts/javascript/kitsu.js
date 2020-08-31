#!/usr/bin/env node

const Kitsu = require('kitsu')
const api = new Kitsu()


const input = process.argv[2];
(async () => {
    // const res = await api.get(input)
    var res
    res = await api.fetch('anime', { include: 'genres', filter: { text: input } })
    console.log(JSON.stringify(res))
})();
