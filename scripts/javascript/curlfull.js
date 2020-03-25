#!/usr/bin/env node
const puppeteer = require('puppeteer');

(async () => {
  const url = process.argv[2];
  const browser = await puppeteer.launch();
  // use tor
  //const browser = await puppeteer.launch({args:['--proxy-server=socks5://127.0.0.1:9050']});
  const page = await browser.newPage();
  await page.goto(url, {waitUntil: 'networkidle2'}); // load
  await page.waitFor(5000);
  //const title = await page.title();
  //console.log(title);
  const html = await page.content();
  console.log(html);

  browser.close();
})();
