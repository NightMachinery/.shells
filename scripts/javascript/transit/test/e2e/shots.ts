// A screenshot-only run of the phone harness, for looking at the board.
//
// The assertion suite beside this file answers "is the layout still correct".
// This one answers "does it look right", which is a question only a picture can
// answer, and which has to be asked again after every change to the stylesheet.
// Running the whole suite to get four pictures costs a minute of waiting on a
// deliberate thirty-second refresh cycle, so the two are separate entry points
// over one harness: the server, the Chrome start, the fixtures and the CDP glue
// are imported from `phone.ts` rather than copied, because two copies of a
// fixture drift and then the pictures stop describing what the suite tested.
//
// It records nothing. If a view cannot be reached it says so on stdout and goes
// on to the next one, because a missing picture is a thing to look at rather
// than a failure to propagate.
//
//   TRANSIT_E2E_SCREENSHOTS=/somewhere bun run test/e2e/shots.ts
//
// Every view is taken twice, once at the phone width the suite asserts against
// and once at a desktop width, because the stylesheet has a breakpoint between
// them and a change that improves one can quietly ruin the other.

import {
  ANCHOR_MS,
  CHROME_PATH,
  Cdp,
  PAGE_DIR,
  PageIo,
  SCRATCH_E2E_DIR,
  buildConfig,
  contentTypeFor,
  fileAt,
  firstPageTarget,
  handleRequestPaused,
  killChromeTree,
  loadMvgBody,
  loadPlanBody,
  loadStoptimesBody,
  readDevToolsUrl,
  sleep,
  type FetchRequestPausedEvent,
} from './phone.ts';

/** The two widths the stylesheet is written for, and what each one is called. */
const PHONE = { width: 390, height: 844, deviceScaleFactor: 3, mobile: true } as const;
const DESKTOP = { width: 1280, height: 900, deviceScaleFactor: 2, mobile: false } as const;

async function main(): Promise<void> {
  if (!(await fileAt(CHROME_PATH).exists())) {
    console.log(`[shots] Chrome binary not found at ${CHROME_PATH}; nothing to do.`);
    process.exit(0);
  }

  const configJson = JSON.stringify(buildConfig());
  const STAMPED = new Set(['index.html', 'route.html', 'sw.js']);

  const server = Bun.serve({
    hostname: '127.0.0.1',
    port: 0,
    fetch(request: Request): Response {
      const url = new URL(request.url);
      let pathname = url.pathname;
      if (pathname === '/data/config.json') {
        return new Response(configJson, { headers: { 'content-type': 'application/json; charset=utf-8' } });
      }
      if (pathname === '/' || pathname === '') pathname = '/index.html';
      const relative = pathname.replace(/^\/+/, '');
      if (relative.includes('..')) return new Response('forbidden', { status: 403 });
      const filePath = `${PAGE_DIR}/${relative}`;
      const stamp = STAMPED.has(relative);
      return fileAt(filePath)
        .exists()
        .then((exists) => {
          if (!exists) return new Response('not found', { status: 404 });
          if (stamp) {
            return fileAt(filePath)
              .text()
              .then(
                (body) =>
                  new Response(body.replaceAll('__BUILD_ID__', 'shots').replaceAll('__SHELL_HASH__', 'shotsshotsss'), {
                    headers: { 'content-type': contentTypeFor(filePath) },
                  }),
              );
          }
          return fileAt(filePath)
            .arrayBuffer()
            .then((body) => new Response(body, { headers: { 'content-type': contentTypeFor(filePath) } }));
        }) as unknown as Response;
    },
  });
  const localOrigin = `http://127.0.0.1:${server.port}`;

  const mvgBody = await loadMvgBody();
  const planBody = await loadPlanBody();
  const stoptimesBody = await loadStoptimesBody();

  const userDataDir = `${SCRATCH_E2E_DIR}/shots-profile-${ANCHOR_MS}-${Math.floor(Math.random() * 1_000_000)}`;
  Bun.spawnSync({ cmd: ['mkdir', '-p', userDataDir] });

  const chromeProc = Bun.spawn({
    cmd: [
      CHROME_PATH,
      `--user-data-dir=${userDataDir}`,
      '--headless=new',
      '--remote-debugging-port=0',
      '--remote-allow-origins=*',
      '--no-first-run',
      '--no-default-browser-check',
      '--disable-extensions',
      '--disable-background-networking',
      '--disable-sync',
      '--hide-scrollbars',
      '--mute-audio',
      '--window-size=390,844',
      'about:blank',
    ],
    stdout: 'pipe',
    stderr: 'pipe',
  });

  let cdp: Cdp | null = null;
  try {
    const wsUrl = await readDevToolsUrl(chromeProc);
    const portMatch = wsUrl.match(/:(\d+)\//);
    if (portMatch === null) throw new Error(`could not parse a port out of ${wsUrl}`);
    const page = await firstPageTarget(Number(portMatch[1]));
    cdp = new Cdp(page.webSocketDebuggerUrl);
    await cdp.ready();

    cdp.on('Fetch.requestPaused', (params) => {
      void handleRequestPaused(cdp as Cdp, params as FetchRequestPausedEvent, localOrigin, mvgBody, planBody, stoptimesBody);
    });

    await cdp.send('Page.enable');
    await cdp.send('Runtime.enable');
    await cdp.send('Network.enable');
    await cdp.send('Network.setBypassServiceWorker', { bypass: true });
    await cdp.send('Fetch.enable', { patterns: [{ urlPattern: '*', requestStage: 'Request' }] });

    // The overlay only exists in a page that believes it is installed, and the
    // only way to make it believe that is to answer the media query before the
    // document runs. Installed from the start rather than part way through, so
    // every picture in one run is of the same page; nothing else on the board
    // looks different in standalone.
    await cdp.send('Page.addScriptToEvaluateOnNewDocument', {
      source: `(() => {
        const real = window.matchMedia.bind(window);
        window.matchMedia = (query) =>
          String(query).includes('display-mode: standalone')
            ? { matches: true, media: query, addListener() {}, removeListener() {}, addEventListener() {}, removeEventListener() {}, dispatchEvent() { return false; } }
            : real(query);
      })()`,
    });

    const io = new PageIo(cdp);

    for (const [label, device] of [
      ['phone', PHONE],
      ['desktop', DESKTOP],
    ] as const) {
      await cdp.send('Emulation.setDeviceMetricsOverride', { ...device });
      // Touch stays emulated at the desktop width too. The sheet and the
      // overlay are reached by tapping, and `Input.dispatchTouchEvent` refuses
      // to fire at all without it; what the pictures are of is the layout at
      // that width, which `mobile: false` and the viewport decide, not the
      // pointer type.
      await cdp.send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 5 });
      // A fresh document per width. The board caches its own column decisions
      // per render and the boards remember which view they are in, so reloading
      // is the only way to be sure a width is being looked at on its own terms.
      await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });
      const booted = await io.waitFor("document.querySelectorAll('li.row').length > 0", 20_000);
      if (!booted) {
        console.log(`[shots] ${label}: no rows appeared; skipping this width.`);
        continue;
      }
      // The journeys arrive after the departures and are half of what a row
      // shows, so a picture taken before them is a picture of a different page.
      await io.waitFor("document.querySelector('li.row a.route') !== null", 25_000, 100);
      await sleep(400);

      await io.screenshot(`${label}-rows.png`);
      console.log(`[shots] ${label}-rows.png`);

      // What compactness actually is, as three numbers rather than an
      // impression: how many departures a reader gets without scrolling, how
      // tall a row is (the tap-target floor is 40px and nothing here may go
      // under it), and how much of the screen the page spends before the first
      // departure. Printed every run so a change can be judged against the
      // previous run's numbers and not against memory.
      const metrics = await io.evalJs<{ visible: number; minRow: number; medianRow: number; toFirstRow: number; docHeight: number }>(`(() => {
        const rows = [...document.querySelectorAll('li.row')];
        const heights = rows.map((r) => r.getBoundingClientRect().height).sort((a, b) => a - b);
        const visible = rows.filter((r) => { const b = r.getBoundingClientRect(); return b.top >= 0 && b.bottom <= window.innerHeight; }).length;
        const first = rows[0]?.getBoundingClientRect().top ?? -1;
        return {
          visible,
          minRow: Math.round((heights[0] ?? 0) * 10) / 10,
          medianRow: Math.round((heights[Math.floor(heights.length / 2)] ?? 0) * 10) / 10,
          toFirstRow: Math.round(first),
          docHeight: Math.round(document.documentElement.scrollHeight),
        };
      })()`);
      console.log(
        `[shots] ${label}: ${metrics.visible} rows in the viewport, row height min ${metrics.minRow}px / median ${metrics.medianRow}px, ` +
          `first row at ${metrics.toFirstRow}px, document ${metrics.docHeight}px`,
      );

      // The same board on the dark palette. Worth its own picture because the
      // two schemes do not differ only in colour: the card and the page are
      // near neighbours in the dark one, so anything that relied on a card's
      // edge to separate it from the background has to be checked there rather
      // than assumed from the light one.
      await cdp.send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value: 'dark' }] });
      await sleep(250);
      await io.screenshot(`${label}-rows-dark.png`);
      console.log(`[shots] ${label}-rows-dark.png`);
      await cdp.send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value: 'light' }] });
      await sleep(250);

      // The strip view, in the first board rather than in whichever board
      // happens to default to it: the first board is the one with five
      // departures across five lines, so its strips show branching, a legend
      // and several times a group, which is what the view is for. Reached by
      // walking the header's own view cycle, because that is how a reader
      // reaches it and it leaves the page in a state the page can be in.
      let stripped = false;
      for (let attempt = 0; attempt < 3 && !stripped; attempt += 1) {
        await io.tapExpr("document.querySelector('section.board .board-title')");
        await sleep(250);
        stripped = await io.evalJs<boolean>("document.querySelector('section.board')?.classList.contains('board-integrated') === true");
      }
      if (stripped) {
        await io.evalJs('window.scrollTo(0, 0)');
        await sleep(250);
        await io.screenshot(`${label}-strip.png`);
        console.log(`[shots] ${label}-strip.png`);
      } else {
        console.log(`[shots] ${label}: could not put the first board into strips.`);
      }

      // Back to a fresh document, and to a fresh memory of it: the view cycle
      // above is remembered in local storage, so without clearing it the board
      // that carries the journeys would still be in strips and there would be
      // no row left to open a sheet from.
      await io.evalJs('(() => { try { localStorage.clear(); } catch {} return null; })()');
      await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });
      await io.waitFor("document.querySelectorAll('li.row').length > 0", 20_000);
      await io.waitFor("document.querySelector('li.row a.route') !== null", 25_000, 100);
      await sleep(300);

      // `.tip` rather than `.tip-sheet`: the same explanation is a centred
      // sheet on a narrow screen and an anchored popover on a wide one, and
      // both are worth looking at.
      await io.tapExpr("document.querySelector('li.row:has(a.route)')");
      const sheetUp = await io.waitFor("document.querySelector('.tip') !== null", 3000, 25);
      if (sheetUp) {
        await sleep(300);
        await io.screenshot(`${label}-sheet.png`);
        console.log(`[shots] ${label}-sheet.png`);
      } else {
        console.log(`[shots] ${label}: the sheet did not open.`);
      }

      const opener = "(document.querySelector('.tip .tip-alternative-open') ?? document.querySelector('.tip .tip-open'))";
      const hasOpener = await io.evalJs<boolean>(`${opener} !== null`);
      if (sheetUp && hasOpener) {
        await io.tapExpr(opener);
        const up = await io.waitFor("document.querySelector('.route-overlay') !== null", 4000, 25);
        if (up) {
          await io.waitFor(
            "document.querySelector('.route-overlay .route-option') !== null || document.querySelector('.route-overlay .empty') !== null",
            6000,
            50,
          );
          await sleep(300);
          await io.screenshot(`${label}-overlay.png`);
          console.log(`[shots] ${label}-overlay.png`);
        } else {
          console.log(`[shots] ${label}: the overlay never appeared.`);
        }
      } else {
        console.log(`[shots] ${label}: nothing in the sheet to open a journey with.`);
      }
    }
  } catch (error) {
    console.error(`[shots] ${error instanceof Error ? error.stack ?? error.message : String(error)}`);
  } finally {
    if (cdp !== null) cdp.close();
    server.stop(true);
    await killChromeTree(userDataDir, chromeProc);
    Bun.spawnSync({ cmd: ['rm', '-rf', userDataDir] });
  }

  process.exit(0);
}

void main();
