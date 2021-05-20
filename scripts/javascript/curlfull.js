#!/usr/bin/env node

const timeout = ((process.env.cfTimeout) || 20) * 1000

// const puppeteer = require('puppeteer');
const puppeteer = require('puppeteer-extra');
// puppeteer.use(require('puppeteer-extra-plugin-repl')())

// add stealth plugin and use defaults (all evasion techniques)
const StealthPlugin = require('puppeteer-extra-plugin-stealth');
puppeteer.use(StealthPlugin());

(async () => {
    const url = process.argv[2];
    var cookieFile = ''; // @output
    if (process.argv.length >= 4) {
        cookieFile = process.argv[3];
    }
    var browser
    if (process.env.ALL_PROXY) {
        browser = await puppeteer.launch({ headless: true, args:['--proxy-server=' + process.env.ALL_PROXY] });
    } else {
        browser = await puppeteer.launch({ headless: true });
    }
    const page = await browser.newPage();

    // https://github.com/puppeteer/puppeteer/blob/master/docs/api.md#pagegotourl-options
    const waittill = { timeout: 300000, waitUntil: ['networkidle2', 'domcontentloaded'] }
    await page.goto(url, waittill);
    // await page.waitForNavigation(waittill); // can get stuck
    await page.waitForTimeout(timeout);

    // Start an interactive REPL here with the `page` instance.
    // await page.repl()
    // Afterwards start REPL with the `browser` instance.
    // await browser.repl()

    //const title = await page.title();
    //console.log(title);
    const html = await page.content();
    console.log(html);

    if (cookieFile != '') {
        const fs = require('fs').promises;

        const cookies = await page.cookies();
        await fs.writeFile(cookieFile, JSON.stringify(cookies, null, 2));
    }

    browser.close();
})();
