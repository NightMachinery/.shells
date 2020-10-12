#!/usr/bin/env node

    const url = process.argv[2];
const puppeteer = require('puppeteer');
(async () => {
  const browser = await puppeteer.launch()
  const page = await browser.newPage()


  await page.goto(url)

  await page.waitForSelector('.con-wizard > .wizard-body > #consent-text > .content-list > .list-item:nth-child(1)')
  await page.click('.con-wizard > .wizard-body > #consent-text > .content-list > .list-item:nth-child(1)')

  await page.waitForSelector('.con-wizard > .wizard-body > .actions > .consent-form > .primary')
  await page.click('.con-wizard > .wizard-body > .actions > .consent-form > .primary')


	const timeout = ((process.env.cfTimeout) || 20) * 1000

    await page.waitFor(timeout);
    const html = await page.content();
    console.log(html);

  await browser.close()
})()
