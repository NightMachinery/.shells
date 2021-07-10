#!/usr/bin/env node

const Kitsu = require('kitsu')
// https://github.com/wopian/kitsu/issues/562
// npmi kitsu@9

const api = new Kitsu()


const input = process.argv[2];
(async () => {
    // const res = await api.get(input)
    var res
    res = await api.fetch('anime', { include: 'genres', filter: { text: input } })
    console.log(JSON.stringify(res))
})();
