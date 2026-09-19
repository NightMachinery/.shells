// The post-deploy gate: does the page that is actually live, serving actual
// upstream data, still look like a departure board.
//
// Every other file in this directory answers "does the code behave" against
// fixtures and a local static server, which is the right question while
// writing the code and the wrong one after a deploy: a fixture cannot notice
// a stale CDN edge, a broken reverse proxy, or an upstream API that changed
// shape under us. So this file asks a narrower question of the real thing:
// load the URL a deploy just put out, at a phone's width, and check that the
// board still looks like a board. It does not touch fixtures, does not run
// a local server, and does not intercept a single request; what it measures
// is whatever the network hands back.
//
// Chrome start, the CDP wire and the page-I/O helpers are imported from
// `phone.ts` rather than re-implemented, for the reason `shots.ts` gives for
// doing the same thing: two copies of that plumbing drift, and a harness that
// drifts from the thing it is supposed to be checking is worse than no
// harness.
//
//   TRANSIT_SMOKE_URL=https://example.invalid/index.html bun test/e2e/smoke.ts

import { CHROME_PATH, PageIo, SCRATCH_E2E_DIR, base64ToBytes, firstPageTarget, killChromeTree, readDevToolsUrl, sleep, Cdp } from './phone.ts';

// --------------------------------------------------------------- assertions

interface AssertionResult {
  name: string;
  pass: boolean;
  detail: string;
}

const results: AssertionResult[] = [];

function record(name: string, pass: boolean, detail: string): void {
  results.push({ name, pass, detail });
  console.log(`${pass ? 'PASS' : 'FAIL'}  ${name}  ${detail}`);
}

// ------------------------------------------------------------------- build id

/** `window.__BUILD__`, turned into something safe for a filename. */
function sanitizeBuild(build: string | null): string {
  if (build === null || build.length === 0) return 'unknown';
  return build.replace(/[^A-Za-z0-9.-]/g, '-');
}

// ------------------------------------------------------------------------ main

async function main(): Promise<void> {
  const targetUrl = process.env.TRANSIT_SMOKE_URL;
  if (targetUrl === undefined || targetUrl.length === 0) {
    console.error(
      'smoke: TRANSIT_SMOKE_URL is not set. The caller must supply the deployed page\'s URL in the environment; ' +
        'this check never hardcodes one, so there is nothing to fall back to.',
    );
    process.exit(2);
  }

  const shotsDir = process.env.TRANSIT_SMOKE_SHOTS ?? `${SCRATCH_E2E_DIR}/smoke`;
  Bun.spawnSync({ cmd: ['mkdir', '-p', shotsDir] });

  const userDataDir = `${SCRATCH_E2E_DIR}/smoke-profile-${Date.now()}-${Math.floor(Math.random() * 1_000_000)}`;
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
  let exitCode = 1;

  try {
    const wsUrl = await readDevToolsUrl(chromeProc);
    const portMatch = wsUrl.match(/:(\d+)\//);
    if (portMatch === null) throw new Error(`could not parse a port out of ${wsUrl}`);
    const page = await firstPageTarget(Number(portMatch[1]));
    cdp = new Cdp(page.webSocketDebuggerUrl);
    await cdp.ready();

    await cdp.send('Page.enable');
    await cdp.send('Runtime.enable');
    await cdp.send('Network.enable');
    // A smoke check has to measure what the server serves right now, not what
    // an installed service worker cached on some earlier visit. There is no
    // installed worker in a fresh profile anyway, but a stale one is exactly
    // the kind of thing a deploy gate exists to catch, so this is asserted
    // rather than assumed.
    await cdp.send('Network.setBypassServiceWorker', { bypass: true });
    await cdp.send('Emulation.setDeviceMetricsOverride', { width: 390, height: 844, deviceScaleFactor: 3, mobile: true });
    await cdp.send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 5 });

    await cdp.send('Page.navigate', { url: targetUrl });

    const io = new PageIo(cdp);

    const booted = await io.waitFor("document.querySelectorAll('li.row').length > 0", 30_000);
    if (!booted) {
      const appText = await io.evalJs<string>('document.getElementById("app")?.textContent ?? "(no #app)"');
      throw new Error(`the deployed page never rendered a li.row within 30s; #app says: ${appText.slice(0, 300)}`);
    }
    await sleep(300); // let the render settle before measuring anything

    // --------------------------------------------------------- screenshot

    const buildId = await io.evalJs<string | null>('window.__BUILD__ ?? null');
    const pageHeight = await io.evalJs<number>(
      'Math.max(document.documentElement.scrollHeight, document.body ? document.body.scrollHeight : 0, document.documentElement.clientHeight)',
    );
    const shot = await cdp.send<{ data: string }>('Page.captureScreenshot', {
      format: 'png',
      captureBeyondViewport: true,
      clip: { x: 0, y: 0, width: 390, height: pageHeight, scale: 1 },
    });
    const shotPath = `${shotsDir}/smoke-${sanitizeBuild(buildId)}.png`;
    await Bun.write(shotPath, base64ToBytes(shot.data));
    console.log(`smoke: screenshot ${shotPath}`);

    // ---------------------------------------------------------- assertion 1

    record(
      'the shell reports a build id',
      buildId !== null && buildId.length > 0 && buildId !== '__BUILD_ID__',
      `window.__BUILD__=${JSON.stringify(buildId)}`,
    );

    // ---------------------------------------------------------- assertion 2

    const overflow = await io.evalJs<{ scrollWidth: number; clientWidth: number; worstRight: number; worstSelector: string }>(`(() => {
      const de = document.documentElement;
      let worstRight = 0; let worstSelector = '';
      for (const element of document.querySelectorAll('body *')) {
        if (element.closest('.chips') || element.closest('.bar-controls')) continue;
        const r = element.getBoundingClientRect();
        if (r.right > worstRight) {
          worstRight = r.right;
          worstSelector = element.className ? '.' + String(element.className).trim().split(/\\s+/).join('.') : element.tagName;
        }
      }
      return { scrollWidth: de.scrollWidth, clientWidth: de.clientWidth, worstRight: Math.round(worstRight * 100) / 100, worstSelector };
    })()`);
    record(
      'no horizontal overflow at 390px',
      overflow.scrollWidth === overflow.clientWidth && overflow.scrollWidth === 390 && overflow.clientWidth === 390 && overflow.worstRight <= 391,
      `scrollWidth=${overflow.scrollWidth} clientWidth=${overflow.clientWidth} worstRight=${overflow.worstRight}px (${overflow.worstSelector})`,
    );

    // ---------------------------------------------------------- assertion 3

    const containment = await io.evalJs<{ rows: number; worstOverflowPx: number; worstClass: string }>(`(() => {
      let worst = 0; let worstClass = '';
      const rows = document.querySelectorAll('li.row');
      for (const row of rows) {
        const rr = row.getBoundingClientRect();
        for (const descendant of row.querySelectorAll('*')) {
          const cr = descendant.getBoundingClientRect();
          if (cr.width === 0 && cr.height === 0) continue; // an empty box cannot overshoot
          const overshoot = Math.max(rr.left - cr.left, rr.top - cr.top, cr.right - rr.right, cr.bottom - rr.bottom);
          if (overshoot > worst) { worst = overshoot; worstClass = descendant.className || descendant.tagName; }
        }
      }
      return { rows: rows.length, worstOverflowPx: Math.round(worst * 100) / 100, worstClass };
    })()`);
    record(
      "every row keeps its children inside itself",
      containment.worstOverflowPx <= 1,
      `${containment.rows} rows checked, worst overshoot ${containment.worstOverflowPx}px in .${containment.worstClass}`,
    );

    // ---------------------------------------------------------- assertion 4

    const badge = await io.evalJs<{ badges: number; onCorner: number; worstEdge: number }>(`(() => {
      const rows = document.querySelectorAll('li.row');
      let badges = 0; let onCorner = 0; let worstEdge = 0;
      for (const row of rows) {
        const mark = row.querySelector(':scope > .platform');
        if (mark === null) continue;
        badges += 1;
        const style = window.getComputedStyle(mark);
        const rr = row.getBoundingClientRect();
        const br = mark.getBoundingClientRect();
        const edge = Math.max(Math.abs(br.top - rr.top), Math.abs(br.right - rr.right));
        if (edge > worstEdge) worstEdge = edge;
        if (style.position === 'absolute' && edge <= 1) onCorner += 1;
      }
      return { badges, onCorner, worstEdge: Math.round(worstEdge * 100) / 100 };
    })()`);
    record(
      "the state badge is in the row's corner",
      badge.badges > 0 && badge.onCorner === badge.badges,
      `${badge.onCorner} of ${badge.badges} badges checked sat flush in the corner (worst edge distance ${badge.worstEdge}px)`,
    );

    // ---------------------------------------------------------- assertion 5

    const stateWord = await io.evalJs<{ checked: number; matches: number; firstClass: string }>(`(() => {
      const banned = new Set(['live', 'planned', 'cancelled']);
      let checked = 0; let matches = 0; let firstClass = '';
      for (const row of document.querySelectorAll('li.row')) {
        for (const element of row.querySelectorAll('*')) {
          checked += 1;
          const r = element.getBoundingClientRect();
          if (r.width <= 0 || r.height <= 0) continue;
          // The cancellation flag says the word on purpose, and it is the one
          // place a state word belongs: beside the time it invalidates, on the
          // rows that have one. What this assertion is looking for is the old
          // state column coming back, which is a word on every row.
          if (element.closest('.flag') !== null) continue;
          const text = (element.textContent ?? '').trim().toLowerCase();
          if (banned.has(text)) {
            matches += 1;
            if (firstClass === '') firstClass = element.className || element.tagName;
          }
        }
      }
      return { checked, matches, firstClass };
    })()`);
    record(
      'no state word is drawn',
      stateWord.matches === 0,
      stateWord.matches === 0
        ? `${stateWord.checked} elements inside li.row checked, none said live/planned/cancelled`
        : `${stateWord.matches} of ${stateWord.checked} elements said a state word, first in .${stateWord.firstClass}`,
    );

    // ---------------------------------------------------------- assertion 6

    // The progress lines are drawn while a board is being fetched and while its
    // journeys are being planned, and they are supposed to go away when the work
    // they describe is finished. A reader's screenshot had both of them still on
    // screen under a board that had clearly finished loading, so this waits for
    // them rather than reading once: the question is not "is anything loading"
    // but "does anything stay loading".
    //
    // Matched by class rather than by text. The wording changes with the backend
    // and the page number, and a check written against one phrasing quietly stops
    // covering the other. The stale note is a different thing wearing the same
    // class: it says how old the journeys on screen are, which is information the
    // reader wants, not a leftover.
    const LINGERING = 'document.querySelectorAll(".board-progress:not(.board-stale)")';
    const settled = await io.waitFor(`${LINGERING}.length === 0`, 25_000, 250);
    const progressLeak = await io.evalJs<{ count: number; texts: string[] }>(`(() => {
      const nodes = [...${LINGERING}].filter((element) => {
        const r = element.getBoundingClientRect();
        return r.width > 0 && r.height > 0;
      });
      return { count: nodes.length, texts: nodes.map((n) => (n.textContent ?? '').trim().slice(0, 120)) };
    })()`);
    record(
      'no progress line is left behind once the boards have loaded',
      settled && progressLeak.count === 0,
      progressLeak.count === 0
        ? 'every board finished and cleared its own progress line'
        : `${progressLeak.count} still on screen after 25s: ${progressLeak.texts.map((t) => `"${t}"`).join(', ')}`,
    );

    // ---------------------------------------------------------- assertion 7

    const destinationCheck = await io.evalJs<{
      found: number;
      passable: boolean;
      widestClientWidth: number;
      widestText: string;
      widestScrollWidth: number;
    }>(`(() => {
      const board = document.querySelector('section.board');
      const cells = board === null ? [] : [...board.querySelectorAll('.destination')];
      let widest = null;
      const entries = cells.map((cell) => {
        const style = window.getComputedStyle(cell);
        const text = cell.textContent ?? '';
        const ellipsised = text.endsWith('\\u2026');
        const cssClipped = style.textOverflow === 'ellipsis' && cell.scrollWidth > cell.clientWidth + 1;
        const truncated = ellipsised || cssClipped;
        if (widest === null || cell.clientWidth > widest.clientWidth) {
          widest = { clientWidth: cell.clientWidth, text, scrollWidth: cell.scrollWidth };
        }
        return { text, truncated, length: text.length };
      });
      const passable =
        entries.some((e) => !e.truncated && e.length >= 12) || (entries.length > 0 && entries.every((e) => !e.truncated && e.length < 12));
      return {
        found: entries.length,
        passable,
        widestClientWidth: widest ? widest.clientWidth : 0,
        widestText: widest ? widest.text : '',
        widestScrollWidth: widest ? widest.scrollWidth : 0,
      };
    })()`);
    record(
      'a destination shows at least 12 characters',
      destinationCheck.found > 0 && destinationCheck.passable,
      destinationCheck.found > 0
        ? `widest .destination clientWidth=${destinationCheck.widestClientWidth}px text="${destinationCheck.widestText}" scrollWidth=${destinationCheck.widestScrollWidth}px`
        : 'no .destination found in the first board',
    );

    // ---------------------------------------------------------- assertion 8

    // Every profile has to be reachable. This is the one check that runs
    // against the real configuration rather than a fixture, so it is the only
    // place a fourth profile, or a title too long for the bar, shows up at all:
    // the touch harness has two invented profiles and would still be green.
    const tabs = await io.evalJs<{
      right: { backend: number; refresh: number; age: number; ageText: string; barRight: number };
      drawn: string[];
      labels: string[];
      tops: number[];
      navScroll: number;
      navClient: number;
      lastRight: number;
      navRight: number;
      slack: number;
    }>(`(() => {
      const nav = document.querySelector('.tabs');
      const nodes = nav === null ? [] : [...nav.querySelectorAll('.tab')];
      const last = nodes.length === 0 ? null : nodes[nodes.length - 1];
      const widthOf = (selector) => {
        const node = document.querySelector(selector);
        return node === null ? 0 : Math.round(node.getBoundingClientRect().width * 100) / 100;
      };
      return {
        right: {
          backend: widthOf('.bar-backend'),
          refresh: widthOf('.refresh'),
          age: widthOf('.refresh-age'),
          ageText: (document.querySelector('.refresh-age') || {}).textContent || '',
          barRight: widthOf('.bar-right'),
        },
        drawn: nodes.map((node) => node.textContent ?? ''),
        labels: nodes.map((node) => node.getAttribute('aria-label') ?? ''),
        tops: nodes.map((node) => Math.round(node.getBoundingClientRect().top)),
        navScroll: nav === null ? 0 : nav.scrollWidth,
        navClient: nav === null ? 0 : nav.clientWidth,
        lastRight: last === null ? 0 : Math.round(last.getBoundingClientRect().right * 100) / 100,
        navRight: nav === null ? 0 : Math.round(nav.getBoundingClientRect().right * 100) / 100,
        // How much room is left between the last tab and the controls on the
        // right. This is what a fifth profile, or a longer short label, would
        // be spending; when it reaches zero the tabs start being clipped.
        slack:
          last === null || document.querySelector('.bar-right') === null
            ? 0
            : Math.round((document.querySelector('.bar-right').getBoundingClientRect().left - last.getBoundingClientRect().right) * 100) / 100,
      };
    })()`);
    record(
      'every profile tab is drawn, on one line, with the last one fully on screen',
      tabs.drawn.length > 1 &&
        new Set(tabs.tops).size === 1 &&
        tabs.navScroll <= tabs.navClient + 1 &&
        tabs.lastRight <= tabs.navRight + 1,
      `${tabs.drawn.length} tabs ${JSON.stringify(tabs.drawn)} (called ${JSON.stringify(tabs.labels)}); nav needs ${tabs.navScroll}px of ${tabs.navClient}px; last tab right=${tabs.lastRight} nav right=${tabs.navRight}; ${tabs.slack}px of room left before the controls; the right of the bar takes ${tabs.right.barRight}px (backend ${tabs.right.backend}, refresh ${tabs.right.refresh}, age ${tabs.right.age} saying "${tabs.right.ageText}")`,
    );

    // --------------------------------------------------------------- summary

    const passed = results.filter((r) => r.pass).length;
    console.log(`\n${passed}/${results.length} smoke assertions passed.`);
    exitCode = passed === results.length ? 0 : 1;
  } catch (error) {
    console.error(`smoke: ${error instanceof Error ? (error.stack ?? error.message) : String(error)}`);
    exitCode = 1;
  } finally {
    if (cdp !== null) cdp.close();
    await killChromeTree(userDataDir, chromeProc);
    Bun.spawnSync({ cmd: ['rm', '-rf', userDataDir] });
  }

  process.exit(exitCode);
}

void main();
