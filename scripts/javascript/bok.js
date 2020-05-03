#!/usr/bin/env node --unhandled-rejections=strict
// const puppeteer = require('puppeteer');
const puppeteer = require('puppeteer-extra');
puppeteer.use(require('puppeteer-extra-plugin-repl')())

// add stealth plugin and use defaults (all evasion techniques)
const StealthPlugin = require('puppeteer-extra-plugin-stealth');
puppeteer.use(StealthPlugin());

function clickToDownload(page, elementHandle, downloadPath = '/tmp', resourceType = 'xhr', filename = '') {

  return new Promise(async (resolve, reject) => {

    // Create the listener.
    const listener = async (response) => {
      const url = response.url();
        // console.log("url:");
        if (url.startsWith('http://dl')) {
            console.log(url);
            process.exit();
        }
        if (response.request().resourceType() === resourceType) {
          console.log("Downloading ...");

        const file = await response.buffer();
        
        // If a filename is specified, use that. If not, use the URL requested
        // but without any query parameters if any.
        const destFilename = (filename) ? 
          filename : url.split('/').pop().replace(/\?.*$/, '');
        const filePath = path.resolve(downloadPath, destFilename);

        // Create a writable stream and write to it.
        const writeStream = fs.createWriteStream(filePath);
        writeStream.write(file, (err) => {
          if (err) reject(err);
          console.log(`Bytes written: ${writeStream.bytesWritten}`);
          onDone(filePath);
        });
      }
    };

    // When the file is saved, remove this listener, and return the file path.
    const onDone = (filePath) => {
      console.log(`Done downloading to ${filePath}`);
      page.removeListener('request', listener);
      resolve(filePath);
    };

    // Tell the page to start watching for a download then click the button 
    // to start a download.
    page.on('response', listener);
    await elementHandle.click();
  });
    }

(async () => {
    const url = process.argv[2];
    const browser = await puppeteer.launch({ headless: true });
    // process.exit(31);
    // use tor
    //const browser = await puppeteer.launch({args:['--proxy-server=socks5://127.0.0.1:9050']});
    const page = await browser.newPage();

    // https://github.com/puppeteer/puppeteer/blob/master/docs/api.md#pagegotourl-options
    const waittill = {timeout: 10000, waitUntil:  ['networkidle2']}
    await page.goto(url, waittill);

    const downloadButton = await page.$('.dlButton');
    const myDownload = await clickToDownload(page, downloadButton, {
        // downloadPath: '/tmp/',
        // filename: 'mydownload.csv'
    });
    console.log(`My file is now located in ${myDownload}.`);

    // await page.repl()
    // await browser.repl()

    browser.close();
})();
