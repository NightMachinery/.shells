#!/usr/bin/env node
// const puppeteer = require('puppeteer');
const puppeteer = require('puppeteer-extra');
// puppeteer.use(require('puppeteer-extra-plugin-repl')())

// add stealth plugin and use defaults (all evasion techniques)
const StealthPlugin = require('puppeteer-extra-plugin-stealth');
puppeteer.use(StealthPlugin());

(async () => {
    const url = process.argv[2];
    const browser = await puppeteer.launch({ headless: true });
    // process.exit(31);
    // use tor
    //const browser = await puppeteer.launch({args:['--proxy-server=socks5://127.0.0.1:9050']});
    const page = await browser.newPage();
    await page.goto(url, {waitUntil: 'networkidle2'}); // load
    await page.waitFor(5000);

    // Start an interactive REPL here with the `page` instance.
    // await page.repl()
    // Afterwards start REPL with the `browser` instance.
    // await browser.repl()

    //const title = await page.title();
    //console.log(title);
    const html = await page.content();
    console.log(html);

    browser.close();
})();
