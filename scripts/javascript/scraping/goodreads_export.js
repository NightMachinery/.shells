#!/usr/bin/env node
// @untested
// @todo add cookies
///
const puppeteer = require('puppeteer-extra');
const StealthPlugin = require('puppeteer-extra-plugin-stealth');
puppeteer.use(StealthPlugin());

(async () => {
  const browser = await puppeteer.launch()
  const page = await browser.newPage()

  const c = JSON.parse(process.argv[2]) // @input

  await page.setCookie(...c);
  await page.goto('https://www.goodreads.com/review/import/')

  var ret = 0
  sel = '.js-LibraryExport'
  try {
    await page.waitForSelector(sel)
    await page.click(sel)
  } catch (err) {
    ret = 1
    console.error("An error happened: ");
    console.error(err);
  } finally {
    const html = await page.content();
    console.log(html);
    await browser.close()
  }
  process.exit(ret)
})()
