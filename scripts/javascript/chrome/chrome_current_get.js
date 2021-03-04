#!/usr/bin/env node

const CDP = require('chrome-remote-interface');

async function example() {
    let client;
    try {
        // connect to endpoint
        client = await CDP(options={port: 8953});
        // extract domains
        const {Runtime} = client;
        console.log(JSON.stringify(await Runtime.evaluate({expression: 'window.location.toString()'})))
    } catch (err) {
        console.error(err);
    } finally {
        if (client) {
            await client.close();
        }
    }
}

example();
