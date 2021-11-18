#!/usr/bin/env node
/*
  - @docs https://pptr.dev/#?product=Puppeteer&show=api-pagepdfoptions
  - This script is intended to be used with local HTML files; but it might still work for remote URLs as well.
*/
const puppeteer = require('puppeteer');

class Webpage {
    static async generatePDF(url) {
        const browser = await puppeteer.launch({ headless: true }); // Puppeteer can only generate pdf in headless mode.
        const page = await browser.newPage();

        const waitTill = { timeout: 120000, waitUntil: ['networkidle0', 'domcontentloaded'] }
        await page.goto(url, waitTill);
        const timeout = 10000;
        await page.waitForTimeout(timeout);
        const pdfConfig = {
            // path: 'url.pdf', // Saves pdf to disk. (Orthogonal to the byte buffer output.)

            printBackground: true,

            /*
              See https://erikastokes.com/kindle-and-ebook-formatting/ereader-aspect-ratios-kindles-nooks-ipads-tablets-phones-and-more/ for the resolutions of the different Kindle devices
              - Oasis 2016: 1430x1080
            */
            width: 1080,
            height: 1430,
            // format: 'A5',

            scale: 2.0, /* Scale amount must be between 0.1 and 2. */

            margin: { // Word's default A4 margins
                top: '0.2cm',
                bottom: '0.3cm',
                left: '0.3cm',
                right: '0.2cm'
            }
        };

        /* page.pdf() generates a pdf of the page with print CSS media. To generate a pdf with screen media, call page.emulateMediaType('screen') before calling page.pdf() */
        // await page.emulateMediaType('screen');

        const pdf = await page.pdf(pdfConfig); // Return the pdf buffer. Useful for saving the file not to disk.

        await browser.close();

        return pdf;
    }
}

(async() => {
    const url = process.argv[2];
    const buffer = await Webpage.generatePDF(url);
    process.stdout.write(buffer)
})();
