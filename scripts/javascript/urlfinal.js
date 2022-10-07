#!/usr/bin/env node
// @deprecated Use `cfUrlFinal=1 curlfull.js`.
///
// const puppeteer = require('puppeteer');
const puppeteer = require('puppeteer-extra');
puppeteer.use(require('puppeteer-extra-plugin-repl')())

// add stealth plugin and use defaults (all evasion techniques)
const StealthPlugin = require('puppeteer-extra-plugin-stealth');
puppeteer.use(StealthPlugin());

(async () => {
    const url = process.argv[2];
    const browser = await puppeteer.launch({ headless: true });
    const page = await browser.newPage();
    const waittill = {timeout: 300000, waitUntil:  ['networkidle2', 'domcontentloaded']}
    await page.goto(url, waittill);
    // await page.waitForNavigation(waittill);
    await page.waitFor(30000);
    // await page.repl()
    const finalurl = await page.url();
    console.log(finalurl);

    browser.close();
})();
