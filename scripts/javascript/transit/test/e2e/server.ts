// Does the page use the planning server when there is one, and does it still
// work when there is not.
//
// This is the only check that runs the page against a real planning server:
// `phone.ts` answers everything from fixtures, and `smoke.ts` looks at the
// deployed page, which has no server yet. So the questions here are the ones
// only a live pair can answer.
//
//   1. With a server, does the phone stop talking to the upstream APIs at all.
//   2. Without one, does the page do the work itself and say so.
//   3. When the server stops answering mid-session, does the page carry on.
//   4. How much does the server actually save on a slow connection.
//
// The last one is a measurement rather than a pass, and it is reported both
// ways round, because "it is faster" is the whole claim being made and a claim
// like that should come with two numbers.
//
// It needs a page served with the API mounted beside it, exactly as the mirror
// serves it: `<page>/api/*` proxied to the server with the mount stripped.
//
// It starts both halves itself, so there is nothing to set up and nothing left
// running afterwards: a planning server on one loopback port, and a static
// server on another that serves the page directory and proxies `/address/api/*`
// to the first with the mount stripped, which is what Caddy does on the mirror.
// All it needs is the directory the page and its server live in, because this
// package is public and the address of somebody's page is not.
//
//   TRANSIT_SERVER_PAGE=~/code/sites/address-pages/address bun test/e2e/server.ts

import {
  CHROME_PATH,
  Cdp,
  PageIo,
  SCRATCH_E2E_DIR,
  contentTypeFor,
  fileAt,
  firstPageTarget,
  killChromeTree,
  readDevToolsUrl,
  sleep,
} from './phone.ts';

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

/**
 * Chrome's "Slow 4G", by its numbers rather than by its name.
 *
 * The same numbers the fixture harness uses for its timing assertion, so the
 * two are comparable. The processor is throttled with them because a phone
 * that has a slow connection is not usually a fast phone either.
 */
const SLOW_4G = {
  offline: false,
  latency: 150,
  downloadThroughput: Math.round((1.6 * 1024 * 1024) / 8),
  uploadThroughput: Math.round((750 * 1024) / 8),
  connectionType: 'cellular4g',
};
const SLOW_CPU_RATE = 4;

interface RequestSeen {
  url: string;
  at: number;
}

interface RunResult {
  toRowsMs: number | null;
  toRoutesMs: number | null;
  rows: number;
  routes: number;
  /** What the page says about where its data came from. */
  provenance: string;
  /** Which source the page settled on, and why. */
  state: { kind: string; reason: string | null };
  apiRequests: string[];
  upstreamRequests: string[];
}

/** A request to one of the two public transit APIs, which the server exists to save. */
function isUpstream(url: string): boolean {
  return url.includes('mvg.de') || url.includes('transitous.org');
}

/** A loopback port nothing else on this machine is likely to want. */
const API_PORT = Number(process.env.TRANSIT_SERVER_API_PORT ?? 8793);
const SITE_PORT = Number(process.env.TRANSIT_SERVER_SITE_PORT ?? 8794);

async function main(): Promise<void> {
  const pageDir = process.env.TRANSIT_SERVER_PAGE;
  if (pageDir === undefined || pageDir.length === 0) {
    console.error(
      'server: TRANSIT_SERVER_PAGE is not set. It is the directory holding the built page and its ' +
        'server/main.ts; this package is public and never hardcodes one.',
    );
    process.exit(2);
  }
  if (!(await fileAt(`${pageDir}/server/main.ts`).exists())) {
    console.error(`server: ${pageDir}/server/main.ts does not exist, so there is no planning server to check.`);
    process.exit(2);
  }

  // The planning server, as a plain process on loopback, exactly as the unit on
  // the mirror runs it.
  const apiProc = Bun.spawn({
    cmd: ['bun', 'run', 'server/main.ts'],
    cwd: pageDir,
    env: { ...process.env, PORT: String(API_PORT) },
    stdout: 'pipe',
    stderr: 'pipe',
  });

  /**
   * The mirror's Caddy, in miniature: files, plus one proxied mount.
   *
   * `decompress: false` is the whole reason this is written out rather than
   * left to a one-liner. bun's fetch decodes a compressed answer by default
   * and keeps the `content-encoding` header on it, so a proxy written the
   * obvious way hands the browser decoded bytes labelled as gzip, and every
   * request through it dies with ERR_CONTENT_DECODING_FAILED. Measured the
   * hard way: it looked exactly like the page refusing to use the server.
   */
  const site = Bun.serve({
    port: SITE_PORT,
    hostname: '127.0.0.1',
    idleTimeout: 120,
    async fetch(request: Request): Promise<Response> {
      const url = new URL(request.url);
      if (url.pathname.startsWith('/address/api/')) {
        const target = `http://127.0.0.1:${API_PORT}${url.pathname.slice('/address/api'.length)}${url.search}`;
        try {
          return await fetch(target, {
            headers: request.headers,
            method: request.method,
            decompress: false,
          } as RequestInit);
        } catch (error) {
          return new Response(JSON.stringify({ error: String(error) }), { status: 502 });
        }
      }
      // Served from the parent of the page directory, because the page is
      // mounted at `/address/` and its own links say so.
      const file = fileAt(`${pageDir}/..${url.pathname}`);
      if (!(await file.exists())) return new Response('not found', { status: 404 });
      return new Response(await file.arrayBuffer(), {
        headers: { 'content-type': contentTypeFor(url.pathname) },
      });
    },
  });
  const targetUrl = `http://127.0.0.1:${SITE_PORT}/address/index.html`;
  console.log(`server: page at ${targetUrl}, planning server on ${API_PORT}`);
  // Give the planning server time to read its configuration and bind.
  for (let attempt = 0; attempt < 40; attempt += 1) {
    try {
      const health = await fetch(`http://127.0.0.1:${API_PORT}/health`);
      if (health.ok) break;
    } catch {
      // not up yet
    }
    await sleep(250);
  }

  const userDataDir = `${SCRATCH_E2E_DIR}/server-profile-${Date.now()}-${Math.floor(Math.random() * 1_000_000)}`;
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
    const io = new PageIo(cdp);

    await cdp.send('Page.enable');
    await cdp.send('Runtime.enable');
    await cdp.send('Network.enable');
    await cdp.send('Network.setBypassServiceWorker', { bypass: true });
    await cdp.send('Network.setCacheDisabled', { cacheDisabled: true });
    await cdp.send('Emulation.setDeviceMetricsOverride', { width: 390, height: 844, deviceScaleFactor: 3, mobile: true });
    await cdp.send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 5 });

    const seen: RequestSeen[] = [];
    const failures: string[] = [];
    const byRequestId = new Map<string, string>();
    cdp.on('Network.requestWillBeSent', (params) => {
      const event = params as { requestId?: string; request?: { url?: string } };
      if (event.request?.url === undefined) return;
      seen.push({ url: event.request.url, at: Date.now() });
      if (event.requestId !== undefined) byRequestId.set(event.requestId, event.request.url);
    });
    // A request that never gets a response says why here and nowhere else, and
    // "the page fell back" is a useless finding without it.
    cdp.on('Network.loadingFailed', (params) => {
      const event = params as { requestId?: string; errorText?: string; canceled?: boolean };
      const url = byRequestId.get(event.requestId ?? '') ?? '(unknown)';
      if (!url.includes('/address/api/')) return;
      failures.push(`${url.slice(0, 90)}: ${event.errorText ?? 'unknown'}${event.canceled === true ? ' (canceled)' : ''}`);
    });

    // Whether the API is allowed through, and what it says when it is not.
    // Answering 404 rather than hanging is what a host without the server
    // actually does: Caddy has no route for the mount and says so at once.
    let apiAnswers: 'through' | 404 | 503 = 'through';
    // Narrow on purpose. Both public transit APIs have `/api/` in their own
    // paths, so a looser pattern pauses every upstream request as well and
    // turns a two second page load into twenty, which is measured rather than
    // guessed: it happened, and every measurement in the run was wrong.
    await cdp.send('Fetch.enable', { patterns: [{ urlPattern: '*/address/api/*' }] });
    cdp.on('Fetch.requestPaused', (params) => {
      const event = params as { requestId: string; request: { url: string } };
      const mine = event.request.url.includes('/address/api/');
      if (!mine || apiAnswers === 'through') {
        void cdp?.send('Fetch.continueRequest', { requestId: event.requestId });
        return;
      }
      void cdp?.send('Fetch.fulfillRequest', {
        requestId: event.requestId,
        responseCode: apiAnswers,
        responseHeaders: [{ name: 'content-type', value: 'application/json' }],
        body: btoa(JSON.stringify({ error: 'no server here' })),
      });
    });

    /** One cold load of the page, measured from the navigation. */
    async function load(label: string, slow: boolean): Promise<RunResult> {
      // A cold load means cold: the page keeps its boards and its journeys in
      // IndexedDB precisely so that a second visit has something to show, and
      // that would make this measure the disk rather than the connection.
      await cdp?.send('Page.navigate', { url: 'about:blank' });
      await sleep(200);
      await cdp?.send('Storage.clearDataForOrigin', {
        origin: new URL(targetUrl).origin,
        storageTypes: 'all',
      });
      seen.length = 0;
      failures.length = 0;
      await cdp?.send('Emulation.setCPUThrottlingRate', { rate: slow ? SLOW_CPU_RATE : 1 });
      await cdp?.send('Network.emulateNetworkConditions', slow ? SLOW_4G : { offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1 });

      const started = Date.now();
      await cdp?.send('Page.navigate', { url: targetUrl });
      // Generous, and it has to be. Doing this work from the phone means a
      // journey search per commute board against a free aggregator, and on a
      // bad evening that has been measured at eighty seconds; a timeout
      // shorter than the thing being measured turns the comparison into a
      // comparison of timeouts.
      const gotRows = await io.waitFor("document.querySelectorAll('li.row').length > 0", 150_000, 50);
      const toRowsMs = gotRows ? Date.now() - started : null;
      const gotRoutes = await io.waitFor("document.querySelector('li.row a.route') !== null", 150_000, 50);
      const toRoutesMs = gotRoutes ? Date.now() - started : null;
      await sleep(300);

      const counts = await io.evalJs<{ rows: number; routes: number }>(
        "({ rows: document.querySelectorAll('li.row').length, routes: document.querySelectorAll('li.row a.route').length })",
      );
      const state = await io.evalJs<{ kind: string; reason: string | null }>(
        'typeof window.__transitSource === "function" ? window.__transitSource() : { kind: "(not published)", reason: null }',
      );
      // The page publishes its timing record, and the source note is the first
      // thing its description says; reading the description rather than a new
      // global keeps this asserting what a reader would see in the popover.
      const line = await io.evalJs<string>(
        '(() => { const p = document.querySelector(".filter-timing"); return p ? p.textContent : ""; })()',
      );
      console.log(
        `  ${label}: rows ${toRowsMs ?? '-'} ms, routes ${toRoutesMs ?? '-'} ms, ${counts.rows} rows, ` +
          `${counts.routes} journeys, source ${state.kind}${state.reason === null ? '' : ` (${state.reason})`}`,
      );
      for (const failure of failures) console.log(`    api request failed: ${failure}`);
      return {
        toRowsMs,
        toRoutesMs,
        rows: counts.rows,
        routes: counts.routes,
        provenance: line,
        state,
        apiRequests: seen.filter((entry) => entry.url.includes('/address/api/')).map((entry) => entry.url),
        upstreamRequests: seen.filter((entry) => isUpstream(entry.url)).map((entry) => entry.url),
      };
    }

    /** The timing line the reader would see, which is where the provenance is written. */
    async function timingLine(): Promise<string> {
      await io.tap('.board .filter-button');
      await io.waitFor("document.querySelector('.filter-popover') !== null", 3000);
      const line = await io.evalJs<string>(
        '(() => { const p = document.querySelector(".filter-timing"); return p ? p.textContent : "(no timing line)"; })()',
      );
      await io.evalJs("document.querySelector('.board .filter-button').click(); void 0");
      await sleep(100);
      return line;
    }

    // ------------------------------------------------- with a server, warm

    // Warmed before it is measured, because that is the state it is in on a
    // machine that has been serving this page for more than half a minute, and
    // a cold server measures the upstream rather than the server.
    apiAnswers = 'through';
    await load('warming the server', false);
    const warm = await load('warm server', false);
    const warmLine = await timingLine();

    record(
      'with a server, the page asks it and nobody else',
      warm.rows > 0 && warm.routes > 0 && warm.state.kind === 'server' && warm.upstreamRequests.length === 0,
      `${warm.rows} rows and ${warm.routes} journeys from ${warm.apiRequests.length} request(s) to the API and ` +
        `${warm.upstreamRequests.length} to the transit APIs directly; the page settled on ${warm.state.kind}`,
    );
    record(
      'the page says where its data came from',
      warmLine.includes('via server'),
      `the timing line reads "${warmLine}"`,
    );

    // --------------------------------------------- the server stops answering

    apiAnswers = 503;
    await io.evalJs("document.querySelector('.refresh').click(); void 0");
    const recovered = await io.waitFor("document.querySelectorAll('li.row').length > 0", 150_000, 50);
    await sleep(1500);
    const afterLine = await timingLine();
    const afterRows = await io.evalJs<number>("document.querySelectorAll('li.row').length");
    record(
      'a server that stops answering is a slower page, not a broken one',
      recovered && afterRows > 0 && afterLine.includes('worked out here'),
      `${afterRows} rows still on screen after the API started refusing; the timing line reads "${afterLine}"`,
    );

    // -------------------------------------------------------- with no server

    apiAnswers = 404;
    const direct = await load('no server', false);
    const directLine = await timingLine();
    record(
      'with no server, the page does the work itself',
      direct.rows > 0 && direct.routes > 0 && direct.state.kind === 'direct' && direct.upstreamRequests.length > 0 && directLine.includes('worked out here'),
      `${direct.rows} rows and ${direct.routes} journeys from ${direct.upstreamRequests.length} request(s) straight to the ` +
        `transit APIs; the timing line reads "${directLine}"`,
    );

    // --------------------------------------------- what the server is worth

    apiAnswers = 'through';
    await load('warming the server again', false);
    const slowServer = await load('slow connection, warm server', true);
    apiAnswers = 404;
    const slowDirect = await load('slow connection, no server', true);
    await cdp.send('Emulation.setCPUThrottlingRate', { rate: 1 });
    await cdp.send('Network.emulateNetworkConditions', { offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1 });

    const both =
      slowServer.toRoutesMs !== null && slowDirect.toRoutesMs !== null
        ? `${slowServer.toRoutesMs} ms with the server against ${slowDirect.toRoutesMs} ms without it ` +
          `(rows: ${slowServer.toRowsMs} ms against ${slowDirect.toRowsMs} ms; ` +
          `requests: ${slowServer.apiRequests.length} to the API and ${slowServer.upstreamRequests.length} upstream, ` +
          `against ${slowDirect.apiRequests.length} and ${slowDirect.upstreamRequests.length})`
        : 'one of the two runs never drew a journey';
    record(
      'on a slow connection the server is the faster way to a journey',
      slowServer.toRoutesMs !== null && slowDirect.toRoutesMs !== null && slowServer.toRoutesMs <= slowDirect.toRoutesMs,
      both,
    );

    const passed = results.filter((result) => result.pass).length;
    console.log(`\n${passed}/${results.length} server assertions passed.`);
    exitCode = passed === results.length ? 0 : 1;
  } catch (error) {
    console.error(`server: ${error instanceof Error ? (error.stack ?? error.message) : String(error)}`);
    exitCode = 1;
  } finally {
    if (cdp !== null) cdp.close();
    await killChromeTree(userDataDir, chromeProc);
    Bun.spawnSync({ cmd: ['rm', '-rf', userDataDir] });
    // Both halves came up with this process and go down with it. A planning
    // server left running would answer the next run from a cache it built
    // under different conditions, which is the sort of thing that makes a
    // measurement quietly meaningless.
    site.stop(true);
    apiProc.kill();
  }

  process.exit(exitCode);
}

void main();
