// Do two browsers reading this page pay for the same translation twice.
//
// They should not. The first one to translate a notice offers the result to the
// planning server, and the next one asks before it translates anything, so the
// second reader gets the text for nothing. That is the whole claim, and it is
// the kind of claim that looks true from the outside while being false: a page
// that translated the notice itself, quickly, and a page that was handed the
// translation look identical on screen. So this counts the calls rather than
// watching the screen, by replacing the browser's Translator with one that
// increments a counter, and asserts that the second instance's counter never
// moved.
//
// Two instances rather than two tabs, because two tabs of the same origin share
// IndexedDB and the second one would find the translation in the local cache
// and never ask anybody. Storage is cleared between the runs, which is what
// makes the second one a different reader rather than the same one again.
//
// The third phase asks the other half of the question. A planning server that
// was given a translation credential does not wait for a reader to translate
// anything: it translates the notice itself and every reader is the second
// reader. That phase runs a SECOND planning server, pointed at a fake
// translation endpoint this harness serves itself, because the one thing this
// must not do is call a paid API.
//
// What is real here and what is not:
//
//   real      the planning servers, their translation stores, their files, and
//             every request the page makes to `/translations`
//   stubbed   the browser's Translator API, which counts its calls, and the
//             translation API, which is a loopback endpoint in this file
//   fixtures  the service messages and the profile answer, so that a run does
//             not depend on what the operator happens to have posted today,
//             and so nothing in this harness reaches the transit APIs at all
//
// It needs the same thing `server.ts` needs, and for the same reason: the
// directory holding the page's own server. This package is public and the
// address of somebody's page is not.
//
//   TRANSIT_SERVER_PAGE=~/code/sites/address-pages/address bun test/e2e/share.ts

import {
  CHROME_PATH,
  Cdp,
  PageIo,
  SCRATCH_E2E_DIR,
  buildConfig,
  contentTypeFor,
  fileAt,
  firstPageTarget,
  killChromeTree,
  readDevToolsUrl,
  sleep,
} from './phone.ts';
import { messageHash } from '../../src/message-hash.ts';
import { WIRE_VERSION, type WireProfileAnswer, type WireTranslations, type WireTranslationsAnswer } from '../../src/page/wire.ts';
import type { Message } from '../../src/model.ts';

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

/** Loopback ports nothing else on this machine is likely to want. */
const API_PORT = Number(process.env.TRANSIT_SHARE_API_PORT ?? 8795);
const SITE_PORT = Number(process.env.TRANSIT_SHARE_SITE_PORT ?? 8796);
/** The second planning server, the one with a translation credential. */
const API2_PORT = Number(process.env.TRANSIT_SHARE_API2_PORT ?? 8797);

/** What the fake translation API prefixes, so its output is unmistakable. */
const SERVER_PREFIX = 'FROM THE SERVER: ';

/**
 * One service message, invented.
 *
 * German, because that is what the panel translates, and about a line the
 * fixture configuration names, because a notice about a line no board shows is
 * correctly hidden. Nothing in it is a real place: this file is public.
 */
const NOTICE: Message = {
  title: 'U6: Stellwerksstörung',
  text:
    'Wegen einer Stellwerksstörung verkehrt die Linie U6 zwischen den beiden Endhaltestellen ' +
    'bis auf Weiteres nicht. Ein Ersatzverkehr mit Bussen ist eingerichtet. Bitte rechnen Sie ' +
    'mit längeren Fahrzeiten.',
  lines: ['U6'],
  backend: 'mvg',
};

/**
 * The translator this harness installs in place of the browser's.
 *
 * It counts two things separately. `availability` is asked on every load and is
 * not translating; `create` and `translate` are, and both have to stay at zero
 * in the second instance. Counting `create` as well as `translate` matters
 * because creating the translator is what downloads a language pack, which is
 * the expensive half on a phone.
 */
const TRANSLATOR_STUB = `
(() => {
  window.__translatorCreates = 0;
  window.__translatorCalls = 0;
  window.__translatorAvailability = 0;
  self.Translator = {
    availability: async () => { window.__translatorAvailability += 1; return 'available'; },
    create: async () => {
      window.__translatorCreates += 1;
      return {
        translate: async (input) => {
          window.__translatorCalls += 1;
          return 'ON-DEVICE: ' + input.slice(0, 40);
        },
      };
    },
  };
})();
`;

interface RunCounts {
  creates: number;
  calls: number;
  availability: number;
  /** What the panel put under the translation, which is where the credit is. */
  credit: string;
  /** The translated body as the reader sees it. */
  text: string;
}

async function main(): Promise<void> {
  const pageDir = process.env.TRANSIT_SERVER_PAGE;
  if (pageDir === undefined || pageDir.length === 0) {
    console.error(
      'share: TRANSIT_SERVER_PAGE is not set. It is the directory holding the page and its ' +
        'server/main.ts; this package is public and never hardcodes one.',
    );
    process.exit(2);
  }
  if (!(await fileAt(`${pageDir}/server/main.ts`).exists())) {
    console.error(`share: ${pageDir}/server/main.ts does not exist, so there is no planning server to check.`);
    process.exit(2);
  }

  // A page directory of this harness's own, built from the working tree rather
  // than from whatever was last deployed, and thrown away at the end. The real
  // page directory is left alone: it is in another repository and running its
  // refresh script here would commit this harness to whatever else is in flight
  // in this one.
  const scratch = `${SCRATCH_E2E_DIR}/share-${Date.now()}-${Math.floor(Math.random() * 1_000_000)}`;
  const pageRoot = `${scratch}/address`;
  Bun.spawnSync({ cmd: ['mkdir', '-p', `${pageRoot}/data`] });
  const build = Bun.spawnSync({
    cmd: ['bun', 'build', 'src/page.ts', '--target', 'browser', '--outfile', `${pageRoot}/app.js`],
    stdout: 'pipe',
    stderr: 'pipe',
  });
  if (!build.success) {
    console.error(`share: could not build the page: ${new TextDecoder().decode(build.stderr)}`);
    process.exit(2);
  }
  for (const name of ['index.html', 'theme.css', 'route.html', 'manifest.webmanifest']) {
    Bun.spawnSync({ cmd: ['cp', '--', `page/${name}`, `${pageRoot}/${name}`] });
  }

  // The fixture configuration, with the departure backend pointed at this
  // harness's own loopback stand-in and the journey planner pointed at a name
  // that does not resolve. Nothing here may reach a transit API, and a URL that
  // cannot be dialled is the only way to be sure of that rather than hopeful.
  //
  // The stand-in is what lets the PLANNING SERVER see the notice, which is the
  // whole of the third phase: the server can only translate a notice it fetched
  // itself, and a fixture injected into the browser is not one.
  const config = buildConfig();
  config.backends = {
    mvg_base_url: `http://127.0.0.1:${SITE_PORT}/fake-mvg`,
    transitous_base_url: 'https://transitous.invalid/api',
  };
  const configPath = `${pageRoot}/data/config.json`;
  await Bun.write(configPath, JSON.stringify(config));

  const storePath = `${scratch}/translations.json`;
  const apiProc = Bun.spawn({
    cmd: ['bun', 'run', 'server/main.ts'],
    cwd: pageDir,
    env: {
      ...process.env,
      PORT: String(API_PORT),
      // The fixture configuration, so this harness never reads the private one.
      ADDRESS_SERVER_CONFIG: configPath,
      // Outside everything, so a run neither reads nor writes the store the
      // machine's own server keeps.
      ADDRESS_SERVER_TRANSLATIONS: storePath,
    },
    stdout: 'pipe',
    stderr: 'pipe',
  });

  // Which planning server the page is talking to. The third phase swaps it for
  // one that has a translation credential, without the page noticing: it is the
  // same mount, the same origin and the same bytes, which is what the mirror
  // would look like on the day the credential is placed.
  let apiPort = API_PORT;
  /** Every batch the fake translation API was asked for, in order. */
  const translateCalls: string[][] = [];

  const site = Bun.serve({
    port: SITE_PORT,
    hostname: '127.0.0.1',
    idleTimeout: 120,
    async fetch(request: Request): Promise<Response> {
      const url = new URL(request.url);
      if (url.pathname === '/fake-mvg/messages') {
        // The departure backend's own shape for a service message, and only
        // that: one invented notice about a line the fixture profile names.
        return Response.json([{ title: NOTICE.title, text: NOTICE.text, lines: NOTICE.lines }]);
      }
      if (url.pathname === '/fake-translate/v2') {
        // Cloud Translation v2's answer shape, and nothing else about it. The
        // planning server is pointed here so that this harness can exercise the
        // server's own translation tier end to end without spending a cent.
        const body = (await request.json()) as { q?: unknown; target?: unknown };
        const texts = Array.isArray(body.q) ? body.q.map((value) => String(value)) : [];
        translateCalls.push(texts);
        return Response.json({
          data: { translations: texts.map((text) => ({ translatedText: `${SERVER_PREFIX}${text.slice(0, 40)}` })) },
        });
      }
      if (url.pathname.startsWith('/address/api/')) {
        const target = `http://127.0.0.1:${apiPort}${url.pathname.slice('/address/api'.length)}${url.search}`;
        try {
          // `decompress: false` for the reason written out in `server.ts`: bun
          // decodes a gzipped answer and keeps the header saying it is gzipped,
          // and a proxy that passes both on kills every request through it.
          return await fetch(target, {
            headers: request.headers,
            method: request.method,
            body: request.method === 'GET' || request.method === 'HEAD' ? undefined : await request.arrayBuffer(),
            decompress: false,
          } as RequestInit);
        } catch (error) {
          return new Response(JSON.stringify({ error: String(error) }), { status: 502 });
        }
      }
      const file = fileAt(`${scratch}${url.pathname}`);
      if (!(await file.exists())) return new Response('not found', { status: 404 });
      return new Response(await file.arrayBuffer(), { headers: { 'content-type': contentTypeFor(url.pathname) } });
    },
  });

  const targetUrl = `http://127.0.0.1:${SITE_PORT}/address/index.html`;
  console.log(`share: page at ${targetUrl}, planning server on ${API_PORT}, store at ${storePath}`);
  let up = false;
  for (let attempt = 0; attempt < 40; attempt += 1) {
    try {
      const health = await fetch(`http://127.0.0.1:${API_PORT}/health`);
      if (health.ok) {
        up = true;
        break;
      }
    } catch {
      // not up yet
    }
    await sleep(250);
  }
  if (!up) console.error('share: the planning server never answered its health check');

  // The second planning server: same code, same fixtures, its own store, and a
  // translation credential whose endpoint is the loopback stand-in above. The
  // key is a string that is not a key, which is the point: nothing here reaches
  // Google, so nothing here needs a real one.
  const api2Proc = Bun.spawn({
    cmd: ['bun', 'run', 'server/main.ts'],
    cwd: pageDir,
    env: {
      ...process.env,
      PORT: String(API2_PORT),
      ADDRESS_SERVER_CONFIG: configPath,
      ADDRESS_SERVER_TRANSLATIONS: `${scratch}/translations-server.json`,
      GOOGLE_TRANSLATE_API_KEY: 'not-a-real-key',
      ADDRESS_SERVER_TRANSLATE_ENDPOINT: `http://127.0.0.1:${SITE_PORT}/fake-translate/v2`,
    },
    stdout: 'pipe',
    stderr: 'pipe',
  });
  for (let attempt = 0; attempt < 40; attempt += 1) {
    try {
      if ((await fetch(`http://127.0.0.1:${API2_PORT}/health`)).ok) break;
    } catch {
      // not up yet
    }
    await sleep(250);
  }

  const hash = await messageHash(NOTICE.text);
  const userDataDir = `${scratch}/profile`;
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
      '--lang=en-US',
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
    await cdp.send('Page.addScriptToEvaluateOnNewDocument', { source: TRANSLATOR_STUB });

    const seen: string[] = [];
    cdp.on('Network.requestWillBeSent', (params) => {
      const event = params as { request?: { url?: string } };
      if (event.request?.url !== undefined) seen.push(event.request.url);
    });

    // The one answer the page needs that this harness does not want to be real.
    // The boards are not what is being checked and planning them would mean
    // fixtures for two more upstreams; `/health`, `/messages` and
    // `/translations` all go to the planning server, which is the thing under
    // test, and the notices reach it through the departure backend stand-in
    // above rather than being injected here.
    await cdp.send('Fetch.enable', { patterns: [{ urlPattern: '*/address/api/*' }] });
    cdp.on('Fetch.requestPaused', (params) => {
      const event = params as { requestId: string; request: { url: string } };
      const path = new URL(event.request.url).pathname;
      const now = Date.now();
      if (path.includes('/api/profile/')) {
        const answer: WireProfileAnswer = {
          v: WIRE_VERSION,
          at: now,
          profileKey: 'primary',
          startMs: now,
          horizonMinutes: 90,
          backends: ['mvg'],
          boards: [],
          routes: null,
          destinationKey: '',
        };
        void cdp?.send('Fetch.fulfillRequest', {
          requestId: event.requestId,
          responseCode: 200,
          responseHeaders: [{ name: 'content-type', value: 'application/json' }],
          body: btoa(unescape(encodeURIComponent(JSON.stringify(answer)))),
        });
        return;
      }
      void cdp?.send('Fetch.continueRequest', { requestId: event.requestId });
    });

    /** One reader, from a cold browser, through to a translated notice on screen. */
    async function readAs(label: string): Promise<RunCounts> {
      await cdp?.send('Page.navigate', { url: 'about:blank' });
      await sleep(200);
      // The point of the second run: a reader whose browser has never seen this
      // notice, rather than the same browser looking at its own cache again.
      await cdp?.send('Storage.clearDataForOrigin', { origin: new URL(targetUrl).origin, storageTypes: 'all' });
      seen.length = 0;
      await cdp?.send('Page.navigate', { url: targetUrl });
      const translated = await io.waitFor("document.querySelector('.disruption-translation') !== null", 30_000, 50);
      if (!translated) throw new Error(`${label}: no translation ever appeared on screen`);
      // A moment for the offer, which is deliberately not awaited by the page:
      // nothing on screen is waiting for it.
      await sleep(600);
      const counts = await io.evalJs<RunCounts>(`(() => {
        const box = document.querySelector('.disruption-translation');
        const credit = box ? box.querySelector('.disruption-provider') : null;
        return {
          creates: window.__translatorCreates,
          calls: window.__translatorCalls,
          availability: window.__translatorAvailability,
          credit: credit ? credit.textContent : '',
          text: box ? box.childNodes[0].textContent : '',
        };
      })()`);
      console.log(
        `  ${label}: ${counts.calls} translate call(s), ${counts.creates} translator(s) created, ` +
          `credit "${counts.credit}"`,
      );
      return counts;
    }

    // -------------------------------------------------- the first reader

    const first = await readAs('first reader');
    record(
      'the first reader translates the notice itself and says so',
      first.calls > 0 && first.credit === 'translated on this device',
      `${first.calls} call(s) into the translator, and the panel credits "${first.credit}"`,
    );

    // What the planning server was told, asked of the server directly rather
    // than of the page, because "the page thinks it shared it" is not the
    // claim being made.
    let shared: WireTranslations = {};
    for (let attempt = 0; attempt < 20; attempt += 1) {
      const answer = await fetch(`http://127.0.0.1:${API_PORT}/translations?lang=en&hashes=${hash}`);
      shared = ((await answer.json()) as WireTranslationsAnswer).translations;
      if (shared[hash] !== undefined) break;
      await sleep(250);
    }
    record(
      'what it translated reached the planning server, under the shared hash',
      shared[hash]?.text === first.text && shared[hash]?.source === 'browser' && shared[hash]?.lang === 'en',
      shared[hash] === undefined
        ? 'the server knows nothing about that hash'
        : `the server holds "${shared[hash].text.slice(0, 40)}" from ${shared[hash].source} in ${shared[hash].lang}`,
    );

    // ------------------------------------------------- the second reader

    const second = await readAs('second reader');
    record(
      'the second reader renders that translation without translating anything',
      second.calls === 0 && second.creates === 0 && second.text === first.text,
      `${second.calls} translate call(s) and ${second.creates} translator(s) created; ` +
        `the body reads "${second.text.slice(0, 40)}"`,
    );
    record(
      'and the panel says the text came from another client, and which kind',
      second.credit === 'translated by another client · browser',
      `the panel credits "${second.credit}"`,
    );
    record(
      'the second reader still probed its own translator, so the button is still offered',
      second.availability > 0,
      `${second.availability} availability check(s)`,
    );
    record(
      'and nothing in either run went to a transit API or to a translation API',
      !seen.some((url) => url.includes('mvg.de') || url.includes('transitous.org') || url.includes('googleapis.com')),
      `${seen.length} request(s), none of them upstream`,
    );

    // ------------------------------- a server that can translate for itself

    // The same page, the same mount, a different planning server behind it: the
    // one that was given a credential. Nothing about the page changes, which is
    // the claim being made.
    apiPort = API2_PORT;
    translateCalls.length = 0;
    const served = await readAs('reader against a translating server');
    record(
      'against a translating server, no reader translates anything at all',
      served.calls === 0 && served.creates === 0 && served.text.startsWith(SERVER_PREFIX),
      `${served.calls} translate call(s) and ${served.creates} translator(s) created; ` +
        `the body reads "${served.text.slice(0, 40)}"`,
    );
    record(
      'and the panel says the text came from the server',
      served.credit === 'translated by server',
      `the panel credits "${served.credit}"`,
    );
    record(
      'the server translated the notice once, not once per reader',
      translateCalls.length >= 1 && translateCalls.every((batch) => batch.length === 1),
      `${translateCalls.length} call(s) to the translation API, of ${translateCalls.map((batch) => batch.length).join(', ') || '-'} notice(s)`,
    );
    const secondServed = await readAs('a second reader against the same server');
    const extra = translateCalls.length;
    record(
      'and a second reader against it costs no translation at all',
      secondServed.calls === 0 && secondServed.text === served.text,
      `${extra} call(s) to the translation API in total after two readers`,
    );

    const passed = results.filter((result) => result.pass).length;
    console.log(`\n${passed}/${results.length} sharing assertions passed.`);
    exitCode = passed === results.length ? 0 : 1;
  } catch (error) {
    console.error(`share: ${error instanceof Error ? (error.stack ?? error.message) : String(error)}`);
    exitCode = 1;
  } finally {
    if (cdp !== null) cdp.close();
    await killChromeTree(userDataDir, chromeProc);
    // Everything this run made came up with it and goes down with it: a
    // planning server left behind would answer the next run out of a store this
    // one filled, which is how a check quietly stops checking anything.
    site.stop(true);
    apiProc.kill();
    api2Proc.kill();
    await apiProc.exited;
    await api2Proc.exited;
    Bun.spawnSync({ cmd: ['rm', '-rf', scratch] });
  }

  process.exit(exitCode);
}

void main();
