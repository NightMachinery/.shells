// Headless-Chrome touch harness for the phone board page.
//
// Serves the built `page/` directory over a local static server, launches
// headless Chrome with a phone-shaped viewport and real touch emulation,
// answers every MVG/Transitous request from committed fixtures over the CDP
// Fetch domain, and drives the page with real `Input.dispatchTouchEvent`
// gestures rather than synthetic clicks. See the task brief for the full
// assertion list; this file is the whole harness.
//
// No dependency beyond bun and Chrome itself: no puppeteer, no playwright, no
// WebSocket library (`WebSocket` is a bun/DOM global). Ambient bun surface
// this file needs beyond `src/bun.d.ts` lives in `./bun-e2e.d.ts`.

import { DEFAULT_PLAN_MODES, DEFAULT_WALK_WEIGHT } from '../../src/config.ts';
import { ALL_MODES } from '../../src/model.ts';
import type { ExportedConfig } from '../../src/page/types.ts';

// --------------------------------------------------------------------- paths

const CHROME_PATH = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';

const SCRATCH_E2E_DIR = '/private/tmp/claude-501/-Users-evar-scripts/1954a689-bd87-4b52-bf28-d32b8c363927/scratchpad/e2e';
const SCREENSHOT_DIR = '/private/tmp/claude-501/-Users-evar-scripts/1954a689-bd87-4b52-bf28-d32b8c363927/scratchpad/p2';

const HERE = import.meta.dir; // .../test/e2e
const PAGE_DIR = `${HERE}/../../page`;
const FIXTURES_DIR = `${HERE}/fixtures`;

// -------------------------------------------------------------------- utils

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/** The `Bun.file` slice this harness uses, cast past the package's narrower ambient type. */
interface FileLike {
  text(): Promise<string>;
  exists(): Promise<boolean>;
  arrayBuffer(): Promise<ArrayBuffer>;
}

function fileAt(path: string): FileLike {
  return Bun.file(path) as unknown as FileLike;
}

function contentTypeFor(path: string): string {
  if (path.endsWith('.html')) return 'text/html; charset=utf-8';
  if (path.endsWith('.js')) return 'text/javascript; charset=utf-8';
  if (path.endsWith('.css')) return 'text/css; charset=utf-8';
  if (path.endsWith('.json')) return 'application/json; charset=utf-8';
  if (path.endsWith('.webmanifest')) return 'application/manifest+json; charset=utf-8';
  if (path.endsWith('.png')) return 'image/png';
  if (path.endsWith('.ico')) return 'image/x-icon';
  if (path.endsWith('.svg')) return 'image/svg+xml';
  return 'application/octet-stream';
}

function toBase64(text: string): string {
  const bytes = new TextEncoder().encode(text);
  let binary = '';
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary);
}

function base64ToBytes(b64: string): Uint8Array {
  const binary = atob(b64);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) bytes[index] = binary.charCodeAt(index);
  return bytes;
}

// -------------------------------------------------------------- result table

interface AssertionResult {
  name: string;
  pass: boolean;
  value: string;
}

const results: AssertionResult[] = [];

function record(name: string, pass: boolean, value: string): void {
  results.push({ name, pass, value });
  console.log(`[${pass ? 'PASS' : 'FAIL'}] ${name} :: ${value}`);
}

// ------------------------------------------------------------- config.json

const MVG_BASE_URL = 'https://www.mvg.de/api/bgw-pt/v3';
const TRANSITOUS_BASE_URL = 'https://api.transitous.org/api/v1';

/**
 * A synthesised config document with the shapes described in the brief: two
 * profiles, commute boards, a board naming a destination place, and a places
 * list. Every stop id is `de:00000:<n>`; every place name is invented.
 */
function buildConfig(): ExportedConfig {
  return {
    schema_version: 1,
    defaults: {
      horizon_minutes: 90,
      backend: 'mvg',
      fallback: 'transitous',
      transport_types: [...ALL_MODES],
      plan_modes: [...DEFAULT_PLAN_MODES],
      walk_weight: DEFAULT_WALK_WEIGHT,
      timezone: 'Europe/Berlin',
      home: null,
    },
    backends: { mvg_base_url: MVG_BASE_URL, transitous_base_url: TRANSITOUS_BASE_URL },
    profiles: [
      {
        key: 'primary',
        title: 'Zuhause',
        boards: [
          {
            title: 'Nordweg',
            stops: ['de:00000:1'],
            modes: null,
            lines: null,
            direction: null,
            destinations: null,
            walk_minutes: 3,
            walk_minutes_by_stop: null,
            stop_labels: null,
            commute: true,
            destination: null,
            connection: null,
          },
          {
            title: 'Talbogen',
            stops: ['de:00000:2'],
            modes: null,
            lines: null,
            direction: null,
            destinations: null,
            walk_minutes: 4,
            walk_minutes_by_stop: null,
            stop_labels: null,
            commute: true,
            destination: 'work',
            connection: null,
          },
        ],
      },
      {
        key: 'secondary',
        title: 'Büro',
        boards: [
          {
            title: 'Rückweg',
            stops: ['de:00000:1'],
            modes: null,
            lines: null,
            direction: null,
            destinations: null,
            walk_minutes: 3,
            walk_minutes_by_stop: null,
            stop_labels: null,
            commute: true,
            destination: 'home',
            connection: null,
          },
        ],
      },
    ],
    places: [
      { name: 'work', label: 'Maxmonument', lat: 0.001, lon: 0.001, stop: null },
      { name: 'home', label: 'Zuhause', lat: 0.002, lon: 0.002, stop: null },
    ],
  };
}

// ------------------------------------------------------- fixtures & rewrite

/** Captured once, so every request within the run sees the same "now". */
const ANCHOR_MS = Date.now();

function isoAt(offsetMin: number): string {
  return new Date(ANCHOR_MS + offsetMin * 60_000).toISOString();
}

interface MvgFixtureRow {
  plannedOffsetMin: number;
  realtimeOffsetMin: number;
  [key: string]: unknown;
}

async function loadMvgBody(): Promise<unknown[]> {
  const raw = JSON.parse(await fileAt(`${FIXTURES_DIR}/mvg-departures.json`).text()) as MvgFixtureRow[];
  return raw.map((row) => {
    const { plannedOffsetMin, realtimeOffsetMin, ...rest } = row;
    return {
      ...rest,
      plannedDepartureTime: ANCHOR_MS + plannedOffsetMin * 60_000,
      realtimeDepartureTime: ANCHOR_MS + realtimeOffsetMin * 60_000,
    };
  });
}

interface PlanFixtureLeg {
  startTimeOffsetMin?: number;
  endTimeOffsetMin?: number;
  scheduledStartTimeOffsetMin?: number;
  [key: string]: unknown;
}

interface PlanFixtureItinerary {
  startTimeOffsetMin: number;
  endTimeOffsetMin: number;
  legs: PlanFixtureLeg[];
}

interface PlanFixture {
  itineraries: PlanFixtureItinerary[];
  nextPageCursor?: string;
}

async function loadPlanBody(): Promise<unknown> {
  const raw = JSON.parse(await fileAt(`${FIXTURES_DIR}/transitous-plan.json`).text()) as PlanFixture;
  const itineraries = raw.itineraries.map((itinerary) => {
    const legs = itinerary.legs.map((leg) => {
      const { startTimeOffsetMin, endTimeOffsetMin, scheduledStartTimeOffsetMin, ...rest } = leg;
      const out: Record<string, unknown> = { ...rest };
      if (startTimeOffsetMin !== undefined) out.startTime = isoAt(startTimeOffsetMin);
      if (endTimeOffsetMin !== undefined) out.endTime = isoAt(endTimeOffsetMin);
      if (scheduledStartTimeOffsetMin !== undefined) out.scheduledStartTime = isoAt(scheduledStartTimeOffsetMin);
      return out;
    });
    return {
      startTime: isoAt(itinerary.startTimeOffsetMin),
      endTime: isoAt(itinerary.endTimeOffsetMin),
      legs,
    };
  });
  return { itineraries, nextPageCursor: raw.nextPageCursor ?? '' };
}

async function loadStoptimesBody(): Promise<unknown> {
  return JSON.parse(await fileAt(`${FIXTURES_DIR}/transitous-stoptimes.json`).text());
}

// ----------------------------------------------------------------- CDP wire

interface CdpErrorPayload {
  code: number;
  message: string;
}

interface CdpMessage {
  id?: number;
  method?: string;
  params?: unknown;
  result?: unknown;
  error?: CdpErrorPayload;
}

/** How long any single CDP call may wait for its reply, in milliseconds. */
const CDP_TIMEOUT_MS = 20_000;

class Cdp {
  private ws: WebSocket;
  private nextId = 0;
  private pending = new Map<number, { resolve: (value: unknown) => void; reject: (error: Error) => void }>();
  private listeners = new Map<string, Array<(params: unknown) => void>>();
  private opened: Promise<void>;

  constructor(url: string) {
    this.ws = new WebSocket(url);
    this.opened = new Promise((resolve, reject) => {
      this.ws.addEventListener('open', () => resolve());
      this.ws.addEventListener('error', () => reject(new Error(`CDP socket error connecting to ${url}`)));
    });
    this.ws.addEventListener('message', (event: MessageEvent) => {
      let msg: CdpMessage;
      try {
        msg = JSON.parse(String(event.data)) as CdpMessage;
      } catch {
        return;
      }
      if (typeof msg.id === 'number') {
        const waiting = this.pending.get(msg.id);
        if (waiting === undefined) return;
        this.pending.delete(msg.id);
        if (msg.error !== undefined) waiting.reject(new Error(`CDP error ${msg.error.code}: ${msg.error.message}`));
        else waiting.resolve(msg.result);
      } else if (typeof msg.method === 'string') {
        for (const handler of this.listeners.get(msg.method) ?? []) handler(msg.params);
      }
    });
  }

  async ready(): Promise<void> {
    await this.opened;
  }

  send<T = unknown>(method: string, params: Record<string, unknown> = {}): Promise<T> {
    const id = (this.nextId += 1);
    return new Promise<T>((resolve, reject) => {
      // A reply that never comes must fail loudly rather than block. Opening a
      // second tab from the page under test is enough to lose one, and without
      // this the run stops dead with no output and no failing assertion, which
      // reads exactly like a slow test.
      const timer = setTimeout(() => {
        if (!this.pending.delete(id)) return;
        reject(new Error(`CDP ${method} did not answer within ${CDP_TIMEOUT_MS} ms`));
      }, CDP_TIMEOUT_MS);
      const settle = {
        resolve: (value: unknown) => {
          clearTimeout(timer);
          resolve(value as T);
        },
        reject: (error: Error) => {
          clearTimeout(timer);
          reject(error);
        },
      };
      this.pending.set(id, settle);
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  on(method: string, handler: (params: unknown) => void): void {
    const list = this.listeners.get(method) ?? [];
    list.push(handler);
    this.listeners.set(method, list);
  }

  close(): void {
    try {
      this.ws.close();
    } catch {
      // already closed
    }
  }
}

// --------------------------------------------------------------- assertions

interface Rect {
  x: number;
  y: number;
  width: number;
  height: number;
  top: number;
  left: number;
  right: number;
  bottom: number;
}

interface EvaluateResult {
  result: { type: string; value?: unknown };
  exceptionDetails?: { text: string; exception?: { description?: string } };
}

async function main(): Promise<void> {
  const chromeExists = await fileAt(CHROME_PATH).exists();
  if (!chromeExists) {
    console.log(`[e2e] Chrome binary not found at ${CHROME_PATH}; skipping the touch harness.`);
    process.exit(0);
  }

  // ---------------------------------------------------------- static server

  const config = buildConfig();
  const configJson = JSON.stringify(config);

  // Which build this server is publishing. The publish script stamps these two
  // placeholders into the shell for real; here the harness plays that part, so
  // it can put out a second build without rebuilding anything and watch what an
  // installed client does about it.
  let servedBuild = 'build-a';
  let servedShell = 'aaaaaaaaaaaa';
  const publish = (build: string, shell: string): void => {
    servedBuild = build;
    servedShell = shell;
  };
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
      // No leading slash for the join, and no `..` escape out of page/.
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
                  new Response(body.replaceAll('__BUILD_ID__', servedBuild).replaceAll('__SHELL_HASH__', servedShell), {
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
  console.log(`[e2e] static server at ${localOrigin}`);

  // -------------------------------------------------------------- fixtures

  const mvgBody = await loadMvgBody();
  const planBody = await loadPlanBody();
  const stoptimesBody = await loadStoptimesBody();

  // ----------------------------------------------------------- chrome start

  const userDataDir = `${SCRATCH_E2E_DIR}/chrome-profile-${ANCHOR_MS}-${Math.floor(Math.random() * 1_000_000)}`;
  Bun.spawnSync({ cmd: ['mkdir', '-p', userDataDir] });
  Bun.spawnSync({ cmd: ['mkdir', '-p', SCREENSHOT_DIR] });

  const chromeArgs = [
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
  ];

  const chromeProc = Bun.spawn({ cmd: [CHROME_PATH, ...chromeArgs], stdout: 'pipe', stderr: 'pipe' });

  let cdp: Cdp | null = null;
  let exitCode = 1;

  try {
    const wsUrl = await readDevToolsUrl(chromeProc);
    const portMatch = wsUrl.match(/:(\d+)\//);
    if (portMatch === null) throw new Error(`could not parse a port out of ${wsUrl}`);
    const port = Number(portMatch[1]);

    const page = await firstPageTarget(port);
    cdp = new Cdp(page.webSocketDebuggerUrl);
    await cdp.ready();

    // --------------------------------------------------------- fetch domain

    const seenTargets: Array<{ targetId: string; type: string; url: string }> = [];
    cdp.on('Target.targetCreated', (params) => {
      const info = (params as { targetInfo?: { targetId: string; type: string; url: string } }).targetInfo;
      if (info !== undefined) seenTargets.push(info);
    });

    cdp.on('Fetch.requestPaused', (params) => {
      void handleRequestPaused(cdp as Cdp, params as FetchRequestPausedEvent, localOrigin, mvgBody, planBody, stoptimesBody);
    });

    await cdp.send('Page.enable');
    await cdp.send('Runtime.enable');
    await cdp.send('Network.enable');
    await cdp.send('Network.setBypassServiceWorker', { bypass: true });
    await cdp.send('Target.setDiscoverTargets', { discover: true });
    await cdp.send('Emulation.setDeviceMetricsOverride', {
      width: 390,
      height: 844,
      deviceScaleFactor: 3,
      mobile: true,
    });
    await cdp.send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 5 });
    await cdp.send('Fetch.enable', { patterns: [{ urlPattern: '*', requestStage: 'Request' }] });

    await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });

    const io = new PageIo(cdp);

    const booted = await io.waitFor("document.querySelectorAll('li.row').length > 0", 15_000);
    if (!booted) {
      const bodyText = await io.evalJs<string>('document.getElementById("app")?.textContent ?? "(no #app)"');
      throw new Error(`the page never rendered a li.row within 15s; #app says: ${bodyText.slice(0, 300)}`);
    }
    await sleep(150); // let the render settle before measuring anything

    await io.screenshot('r6b-rows.png');

    // ---------------------------------------------------------- assertion 8

    const containment = await io.evalJs<{ worstOverflowPx: number; worstClass: string; rows: number }>(`(() => {
      let worst = 0; let worstClass = '';
      const rows = document.querySelectorAll('li.row');
      for (const row of rows) {
        const rr = row.getBoundingClientRect();
        for (const child of row.children) {
          const cr = child.getBoundingClientRect();
          if (cr.width === 0 && cr.height === 0) continue; // a truly empty box cannot overflow
          const overflow = Math.max(rr.left - cr.left, rr.top - cr.top, cr.right - rr.right, cr.bottom - rr.bottom);
          if (overflow > worst) { worst = overflow; worstClass = child.className || child.tagName; }
        }
      }
      return { worstOverflowPx: Math.round(worst * 100) / 100, worstClass, rows: rows.length };
    })()`);
    record(
      'containment: every row child inside its row (<=1px)',
      containment.worstOverflowPx <= 1,
      `worst overflow ${containment.worstOverflowPx}px in .${containment.worstClass} across ${containment.rows} rows`,
    );

    // Diagnostic requested alongside the containment assertion: if the worst
    // offender is the `.alarm` slot, work out whether the row's own grid
    // columns genuinely add up to more than the row's content width (a page
    // bug) or whether this is something else. Reported regardless of pass/fail
    // so it does not have to be re-derived by hand from a screenshot.
    const gridDiag = await io.evalJs<{
      rowLeft: number;
      rowRight: number;
      rowWidth: number;
      rowClientWidth: number;
      rowPaddingLeft: string;
      rowPaddingRight: string;
      rowBoxSizing: string;
      inlineGridTemplateColumns: string;
      computedGridTemplateColumns: string;
      columnGap: string;
      colAlarmOnRow: string;
      colAlarmOnRoot: string;
      alarmLeft: number | null;
      alarmRight: number | null;
      alarmWidth: number | null;
      alarmClass: string | null;
    } | null>(`(() => {
      const row = document.querySelector('li.row');
      if (!row) return null;
      const alarmCell = row.querySelector('.alarm');
      const rowRect = row.getBoundingClientRect();
      const alarmRect = alarmCell ? alarmCell.getBoundingClientRect() : null;
      const rowStyle = getComputedStyle(row);
      const rootStyle = getComputedStyle(document.documentElement);
      return {
        rowLeft: rowRect.left,
        rowRight: rowRect.right,
        rowWidth: rowRect.width,
        rowClientWidth: row.clientWidth,
        rowPaddingLeft: rowStyle.paddingLeft,
        rowPaddingRight: rowStyle.paddingRight,
        rowBoxSizing: rowStyle.boxSizing,
        inlineGridTemplateColumns: row.style.gridTemplateColumns,
        computedGridTemplateColumns: rowStyle.gridTemplateColumns,
        columnGap: rowStyle.columnGap,
        colAlarmOnRow: rowStyle.getPropertyValue('--col-alarm').trim(),
        colAlarmOnRoot: rootStyle.getPropertyValue('--col-alarm').trim(),
        alarmLeft: alarmRect ? alarmRect.left : null,
        alarmRight: alarmRect ? alarmRect.right : null,
        alarmWidth: alarmRect ? alarmRect.width : null,
        alarmClass: alarmCell ? alarmCell.className : null,
      };
    })()`);
    if (gridDiag !== null) {
      const trackWidths = gridDiag.computedGridTemplateColumns
        .split(/\s+/)
        .filter((token) => token.length > 0)
        .map((token) => Number.parseFloat(token))
        .filter((n) => Number.isFinite(n));
      const gapPx = Number.parseFloat(gridDiag.columnGap) || 0;
      const tracksSum = trackWidths.reduce((a, b) => a + b, 0);
      const gapsSum = trackWidths.length > 1 ? gapPx * (trackWidths.length - 1) : 0;
      const gridTotal = tracksSum + gapsSum;
      console.log(
        `[e2e] grid diagnostic (row 0): rowRect.width=${gridDiag.rowWidth.toFixed(2)} rowClientWidth=${gridDiag.rowClientWidth} ` +
          `padding=(${gridDiag.rowPaddingLeft},${gridDiag.rowPaddingRight}) boxSizing=${gridDiag.rowBoxSizing}\n` +
          `  --col-alarm on row=${gridDiag.colAlarmOnRow || '(unset)'} on root=${gridDiag.colAlarmOnRoot || '(unset)'}\n` +
          `  inline grid-template-columns=${gridDiag.inlineGridTemplateColumns}\n` +
          `  computed grid-template-columns=${gridDiag.computedGridTemplateColumns}\n` +
          `  track widths=[${trackWidths.map((n) => n.toFixed(2)).join(', ')}] columnGap=${gapPx}px\n` +
          `  sum(tracks)=${tracksSum.toFixed(2)}px + sum(gaps)=${gapsSum.toFixed(2)}px = ${gridTotal.toFixed(2)}px vs rowClientWidth=${gridDiag.rowClientWidth}px ` +
          `(excess=${(gridTotal - gridDiag.rowClientWidth).toFixed(2)}px)\n` +
          `  .alarm cell: class="${gridDiag.alarmClass}" rect=[left=${gridDiag.alarmLeft?.toFixed(2)}, right=${gridDiag.alarmRight?.toFixed(2)}, width=${gridDiag.alarmWidth?.toFixed(2)}] ` +
          `vs row rect=[left=${gridDiag.rowLeft.toFixed(2)}, right=${gridDiag.rowRight.toFixed(2)}]`,
      );
    }

    // ---------------------------------------------------------- assertion 9

    const overflow = await io.evalJs<{ scrollWidth: number; clientWidth: number; worstRight: number; worstSelector: string }>(`(() => {
      const de = document.documentElement;
      let worstRight = 0; let worstSelector = '';
      const all = document.querySelectorAll('body *');
      for (const element of all) {
        if (element.closest('.chips') || element.closest('.bar-controls')) continue;
        const r = element.getBoundingClientRect();
        if (r.right > worstRight) {
          worstRight = r.right;
          worstSelector = element.className ? '.' + String(element.className).trim().split(/\\s+/).join('.') : element.tagName;
        }
      }
      return { scrollWidth: de.scrollWidth, clientWidth: de.clientWidth, worstRight: Math.round(worstRight * 100) / 100, worstSelector };
    })()`);
    const overflowPass = overflow.scrollWidth === 390 && overflow.clientWidth === 390 && overflow.worstRight <= 391;
    record(
      'no horizontal overflow (scrollWidth=clientWidth=390, worst right <=391px)',
      overflowPass,
      `scrollWidth=${overflow.scrollWidth} clientWidth=${overflow.clientWidth} worstRight=${overflow.worstRight}px (${overflow.worstSelector})`,
    );

    // -------------------------------------------- state-badge clip check

    const stateClip = await io.evalJs<{ found: boolean; scrollWidth: number; clientWidth: number; text: string }>(`(() => {
      const nodes = document.querySelectorAll('li.row .state');
      for (const node of nodes) {
        if (node.textContent && node.textContent.trim() === 'planned') {
          return { found: true, scrollWidth: node.scrollWidth, clientWidth: node.clientWidth, text: node.textContent.trim() };
        }
      }
      return { found: false, scrollWidth: 0, clientWidth: 0, text: '' };
    })()`);
    record(
      '.state badge ("planned") is not clipped (scrollWidth <= clientWidth)',
      stateClip.found && stateClip.scrollWidth <= stateClip.clientWidth,
      stateClip.found
        ? `scrollWidth=${stateClip.scrollWidth} clientWidth=${stateClip.clientWidth}`
        : 'no li.row .state with text "planned" found',
    );

    // ------------------------------------- destination width on a planned row

    // The destination is the row's subject and it is the column that yields
    // first, so it is the one worth measuring rather than trusting. Measured
    // against a sample word rendered in the cell's own font rather than against
    // a character count, because "at least eight characters" is a different
    // number of pixels in every font the page might be served in.
    const SAMPLE = 'Talbogen';
    const destWidth = await io.evalJs<{
      found: boolean;
      clientWidth: number;
      scrollWidth: number;
      sampleWidth: number;
      text: string;
    }>(`(() => {
      const row = document.querySelector('li.row:has(a.route)');
      const cell = row === null ? null : row.querySelector('.destination');
      if (cell === null) return { found: false, clientWidth: 0, scrollWidth: 0, sampleWidth: 0, text: '' };
      const probe = document.createElement('span');
      const style = window.getComputedStyle(cell);
      probe.style.position = 'absolute';
      probe.style.visibility = 'hidden';
      probe.style.whiteSpace = 'pre';
      probe.style.font = style.font;
      probe.style.letterSpacing = style.letterSpacing;
      probe.textContent = ${JSON.stringify(SAMPLE)};
      document.body.append(probe);
      const sampleWidth = probe.getBoundingClientRect().width;
      probe.remove();
      return {
        found: true,
        clientWidth: cell.clientWidth,
        scrollWidth: cell.scrollWidth,
        sampleWidth,
        text: cell.textContent ?? '',
      };
    })()`);
    record(
      `destination on a row with a journey fits "${SAMPLE}" or its own full text`,
      destWidth.found && (destWidth.clientWidth >= destWidth.sampleWidth || destWidth.scrollWidth <= destWidth.clientWidth),
      destWidth.found
        ? `clientWidth=${destWidth.clientWidth.toFixed(2)} needs=${destWidth.sampleWidth.toFixed(2)} for "${SAMPLE}", own text "${destWidth.text}" wants ${destWidth.scrollWidth}`
        : 'no li.row with an a.route and a .destination found',
    );

    // ------------------------------------------------- assertions 1, 3, 7

    const firstRowTap = await io.tapOpensSheet("document.querySelectorAll('li.row')[0]");
    record(
      'tap on li.row opens .tip-sheet within 300ms',
      firstRowTap.opened && firstRowTap.ms <= 300,
      firstRowTap.opened ? `${firstRowTap.ms}ms` : 'sheet never opened',
    );
    record(
      'selection collapsed after row tap',
      firstRowTap.selectionCollapsed,
      `isCollapsed=${firstRowTap.selectionCollapsed}`,
    );
    await io.screenshot('r6b-sheet.png');

    const backdropRect = await io.rectOfExpr("document.querySelector('.tip-backdrop')");
    let backdropClosed = false;
    if (backdropRect !== null) {
      await io.dispatchTap(backdropRect.left + 4, backdropRect.top + 4);
      backdropClosed = await io.waitFor("document.querySelector('.tip-sheet') === null", 1000);
    }
    record('tap on .tip-backdrop closes the sheet', backdropClosed, `backdropFound=${backdropRect !== null} closed=${backdropClosed}`);

    // ------------------------------------------------------------ assertion 2

    const childTargets: Array<{ label: string; expr: string }> = [
      { label: 'badge', expr: "document.querySelectorAll('li.row')[0].querySelector('.badge')" },
      { label: 'destination', expr: "document.querySelectorAll('li.row')[0].querySelector('.destination')" },
      { label: 'times', expr: "document.querySelectorAll('li.row')[0].querySelector('.times')" },
    ];
    for (const target of childTargets) {
      await io.closeSheetIfOpen();
      const tap = await io.tapOpensSheet(target.expr);
      record(
        `tap on .row .${target.label} opens .tip-sheet within 300ms`,
        tap.opened && tap.ms <= 300,
        tap.opened ? `${tap.ms}ms` : 'sheet never opened',
      );
      record(`selection collapsed after .${target.label} tap`, tap.selectionCollapsed, `isCollapsed=${tap.selectionCollapsed}`);
    }
    await io.closeSheetIfOpen();

    // ------------------------------------------------------------ assertion 4

    // A different row from the one used for the tap assertions above, so its
    // state is untouched going into the long-press.
    const longPressRowExpr = "document.querySelectorAll('li.row')[2]";
    await io.scrollIntoViewExpr(longPressRowExpr);
    await sleep(60);
    const longPressRowRect = await io.rectOfExpr(longPressRowExpr);
    if (longPressRowRect === null) throw new Error('third row not found for the long-press assertion');
    const lpX = longPressRowRect.left + longPressRowRect.width / 2;
    const lpY = longPressRowRect.top + longPressRowRect.height / 2;
    await io.dispatchTouchStart(lpX, lpY);
    await sleep(700);
    await io.dispatchTouchEnd();
    await sleep(150);
    const popupOpened = await io.evalJs<boolean>("document.querySelector('.alarm-popup') !== null");
    const sheetOpenedDuringLongPress = await io.evalJs<boolean>("document.querySelector('.tip-sheet') !== null");
    record(
      '700ms long press on a row opens .alarm-popup and not .tip-sheet',
      popupOpened && !sheetOpenedDuringLongPress,
      `popupOpened=${popupOpened} sheetOpened=${sheetOpenedDuringLongPress}`,
    );
    if (popupOpened) await io.screenshot('r6b-popup.png');
    await io.evalJs("document.querySelector('.alarm-popup')?.remove(); void 0");
    await io.closeSheetIfOpen();

    // ------------------------------------------------------------ assertion 10

    const stability = await io.evalJs<{ added: number; removed: number }>(
      `(async () => {
        const root = document.getElementById('app');
        let added = 0, removed = 0;
        const obs = new MutationObserver((mutations) => {
          for (const m of mutations) {
            for (const node of m.addedNodes) if (node.nodeType === 1 && node.matches && node.matches('li.row')) added++;
            for (const node of m.removedNodes) if (node.nodeType === 1 && node.matches && node.matches('li.row')) removed++;
          }
        });
        if (root) obs.observe(root, { childList: true, subtree: true });
        for (let tick = 0; tick < 6; tick++) await new Promise((resolve) => setTimeout(resolve, 1000));
        obs.disconnect();
        return { added, removed };
      })()`,
      true,
    );
    record(
      'DOM stability: zero li.row added/removed over 6s of unchanged data',
      stability.added === 0 && stability.removed === 0,
      `added=${stability.added} removed=${stability.removed}`,
    );

    // ------------------------------------------------------------ assertion 6

    // Addressed by content, not position: which row sorts first depends on the
    // fixture's departure times (a cancelled row can sort ahead of the planned
    // one), and this assertion needs the specific row the plan fixture matches.
    // `a.route` is the real journey anchor `renderRoute` draws once a plan
    // exists for that row (the empty-column placeholder is a `span`, not an
    // `a`), so a row that has one is unambiguously a planned row.
    const plannedRowExpr = "document.querySelector('li.row:has(a.route)')";
    await io.waitFor(`${plannedRowExpr} !== null`, 5000);
    await io.closeSheetIfOpen();
    const rowTap = await io.tapOpensSheet(plannedRowExpr);
    if (!rowTap.opened) throw new Error('sheet did not open for the anchor assertion');
    // The alternatives list inside the sheet is built from the same plan data,
    // so it is present as soon as the sheet is; this just gives it one more
    // beat in case of a straggling re-render.
    await io.waitFor("document.querySelector('.tip-open') !== null", 5000);

    const altAnchor = await io.checkAnchor("document.querySelector('.tip-alternative-open')");
    record(
      '.tip-alternative-open is a real anchor (A, target=_blank, rel~=noopener, href=route.html#..., decodes v)',
      altAnchor.exists && altAnchor.tagName === 'A' && altAnchor.target === '_blank' && altAnchor.rel.includes('noopener') && altAnchor.hrefOk && altAnchor.decodedHasV,
      JSON.stringify(altAnchor),
    );
    const finalAnchor = await io.checkAnchor("document.querySelector('.tip-open')");
    record(
      '.tip-open ("Open in a new tab") is a real anchor (A, target=_blank, rel~=noopener, href=route.html#..., decodes v)',
      finalAnchor.exists && finalAnchor.tagName === 'A' && finalAnchor.target === '_blank' && finalAnchor.rel.includes('noopener') && finalAnchor.hrefOk && finalAnchor.decodedHasV,
      JSON.stringify(finalAnchor),
    );

    for (const anchor of [
      { label: '.tip-alternative-open', expr: "document.querySelector('.tip-alternative-open')" },
      { label: '.tip-open', expr: "document.querySelector('.tip-open')" },
    ]) {
      const before = seenTargets.length;
      const rect = await io.rectOfExpr(anchor.expr);
      if (rect === null) {
        record(`tap ${anchor.label} opens a new target`, false, 'anchor not found');
        continue;
      }
      const x = rect.left + rect.width / 2;
      const y = rect.top + rect.height / 2;
      await io.dispatchTap(x, y);
      await sleep(1200);
      let created = seenTargets.slice(before).find((t) => t.type === 'page');
      let via = 'touch';
      if (created === undefined) {
        // Possibly blocked by the popup blocker. Try a real mouse click too, and
        // report both outcomes, per the brief.
        await cdp.send('Input.dispatchMouseEvent', { type: 'mousePressed', x, y, button: 'left', clickCount: 1 });
        await cdp.send('Input.dispatchMouseEvent', { type: 'mouseReleased', x, y, button: 'left', clickCount: 1 });
        await sleep(1200);
        created = seenTargets.slice(before).find((t) => t.type === 'page');
        via = created !== undefined ? 'mouse (touch tap was blocked)' : 'neither touch nor mouse';
      }
      record(
        `tap ${anchor.label} creates a new target (Target.targetCreated)`,
        created !== undefined,
        created !== undefined ? `via ${via}, url=${created.url}` : 'no new target: window.open is blocked in this headless session',
      );
      // Close what the tap opened before going on. A second tab left sitting
      // there is not a neutral bystander: it takes the foreground away from the
      // page under test, and the next evaluation on that page can then wait for
      // a reply that never arrives.
      for (const opened of seenTargets.slice(before)) {
        if (opened.type !== 'page') continue;
        try {
          await cdp.send('Target.closeTarget', { targetId: opened.targetId });
        } catch {
          // Already gone, which is the outcome we wanted anyway.
        }
      }
      await sleep(200);
    }

    // ------------------------------------------------------------ assertion 5

    await io.closeSheetIfOpen();
    const marker = `e2e-marker-${Date.now()}`;
    const reopened = await io.tapOpensSheet(plannedRowExpr);
    if (!reopened.opened) throw new Error('sheet did not (re)open before the refresh-survival assertion');
    await io.evalJs(`(() => { const sheet = document.querySelector('.tip-sheet'); if (sheet) sheet.__e2eMarker = ${JSON.stringify(marker)}; return null; })()`);
    const beforeText = await io.evalJs<string>("document.querySelector('.tip-sheet')?.textContent ?? ''");
    console.log('[e2e] waiting ~31s for the real 30s refresh cycle, sheet open, to check it survives in place...');
    await sleep(31_000);
    const stillOpen = await io.evalJs<boolean>("document.querySelector('.tip-sheet') !== null");
    const survivedMarker = await io.evalJs<string | null>(`document.querySelector('.tip-sheet')?.__e2eMarker ?? null`);
    const afterText = await io.evalJs<string>("document.querySelector('.tip-sheet')?.textContent ?? ''");
    record(
      'sheet survives a refresh tick in place (same node, text patched not replaced)',
      stillOpen && survivedMarker === marker,
      `stillOpen=${stillOpen} markerSurvived=${survivedMarker === marker} textChanged=${beforeText !== afterText}`,
    );

    // --------------------------------------------- assertion: the update flow
    //
    // The test this section exists for, written after two deploys in a row
    // reached the server and neither reached the phone. A browser decides
    // whether to install a new service worker by comparing that worker's own
    // bytes, so a deploy that changes the page and not the worker is a deploy
    // an installed client never notices: it keeps its cache, keeps answering
    // from it, and shows a version that is gone. Checking the server had the
    // new bytes proved nothing, because it did.
    //
    // Every other assertion in this file bypasses the service worker, which is
    // right everywhere else and exactly wrong here, so this turns it back on.

    await cdp.send('Network.setBypassServiceWorker', { bypass: false });
    publish('aaaaaaa-20260101', 'aaaaaaaaaaaa');

    // A clean slate, or the worker left over from an earlier run decides this.
    await io.evalJs(
      `(async () => {
        const regs = await navigator.serviceWorker.getRegistrations();
        await Promise.all(regs.map((r) => r.unregister()));
        const keys = await caches.keys();
        await Promise.all(keys.map((k) => caches.delete(k)));
        return true;
      })()`,
      true,
    );

    await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });
    await io.waitFor("document.querySelectorAll('li.row').length > 0", 15_000);
    const controlled = await io.waitFor('navigator.serviceWorker.controller !== null', 10_000, 100);
    // Once more, so this load is genuinely served by the worker rather than
    // being the load that installed it.
    await cdp.send('Page.reload', {});
    await io.waitFor("document.querySelectorAll('li.row').length > 0", 15_000);
    const buildA = await io.evalJs<string | null>('window.__BUILD__ ?? null');
    record(
      'build A is installed and controlling',
      controlled && buildA === 'aaaaaaa-20260101',
      `controller=${controlled} window.__BUILD__=${String(buildA)}`,
    );

    // The deploy. Only the stamp changes, which is precisely the case that used
    // to be invisible: same page, same script, different shell hash.
    publish('bbbbbbb-20260202', 'bbbbbbbbbbbb');
    await cdp.send('Page.reload', {});
    await io.waitFor("document.querySelectorAll('li.row').length > 0", 15_000);
    const toastShown = await io.waitFor("document.querySelector('.update-toast') !== null", 15_000, 100);
    record(
      'a new shell offers the reader a reload',
      toastShown,
      toastShown ? 'the update toast appeared' : 'no .update-toast within 15s of the new shell being published',
    );

    if (toastShown) {
      const toastRect = await io.rectOfExpr("document.querySelector('.update-toast')");
      if (toastRect !== null) {
        await io.dispatchTap(toastRect.left + toastRect.width / 2, toastRect.top + toastRect.height / 2);
      }
      await io.waitFor("document.querySelectorAll('li.row').length > 0", 15_000);
      const buildB = await io.evalJs<string | null>('window.__BUILD__ ?? null');
      record(
        'tapping the toast lands on build B',
        buildB === 'bbbbbbb-20260202',
        `window.__BUILD__=${String(buildB)}`,
      );
    }

    await cdp.send('Network.setBypassServiceWorker', { bypass: true });

    // --------------------------------------------------------------- summary

    console.log('\n=== summary ===');
    const nameWidth = Math.max(...results.map((r) => r.name.length));
    for (const result of results) {
      console.log(`${result.pass ? 'PASS' : 'FAIL'}  ${result.name.padEnd(nameWidth)}  ${result.value}`);
    }
    const failed = results.filter((r) => !r.pass);
    console.log(`\n${results.length - failed.length}/${results.length} assertions passed.`);
    exitCode = failed.length === 0 ? 0 : 1;
  } finally {
    if (cdp !== null) cdp.close();
    server.stop(true);
    await killChromeTree(userDataDir, chromeProc);
    Bun.spawnSync({ cmd: ['rm', '-rf', userDataDir] });
  }

  process.exit(exitCode);
}

// ------------------------------------------------------------- page I/O helper

interface TapOpenResult {
  ms: number;
  opened: boolean;
  selectionCollapsed: boolean;
}

interface AnchorCheck {
  exists: boolean;
  tagName: string;
  target: string;
  rel: string;
  hrefOk: boolean;
  decodedHasV: boolean;
  href: string;
}

class PageIo {
  constructor(private cdp: Cdp) {}

  async evalJs<T>(expression: string, awaitPromise = false): Promise<T> {
    const res = await this.cdp.send<EvaluateResult>('Runtime.evaluate', { expression, returnByValue: true, awaitPromise });
    if (res.exceptionDetails !== undefined) {
      const description = res.exceptionDetails.exception?.description ?? res.exceptionDetails.text;
      throw new Error(`page evaluate failed: ${description}`);
    }
    return res.result.value as T;
  }

  async waitFor(boolExpression: string, timeoutMs: number, intervalMs = 25): Promise<boolean> {
    const deadline = Date.now() + timeoutMs;
    while (Date.now() < deadline) {
      if (await this.evalJs<boolean>(boolExpression)) return true;
      await sleep(intervalMs);
    }
    return false;
  }

  async rectOfExpr(expr: string): Promise<Rect | null> {
    return this.evalJs<Rect | null>(`(() => {
      const el = ${expr};
      if (!el) return null;
      const r = el.getBoundingClientRect();
      return { x: r.x, y: r.y, width: r.width, height: r.height, top: r.top, left: r.left, right: r.right, bottom: r.bottom };
    })()`);
  }

  async scrollIntoViewExpr(expr: string): Promise<void> {
    await this.evalJs(`(() => { const el = ${expr}; if (el) el.scrollIntoView({ block: 'center', inline: 'nearest' }); return null; })()`);
  }

  async dispatchTouchStart(x: number, y: number): Promise<void> {
    await this.cdp.send('Input.dispatchTouchEvent', {
      type: 'touchStart',
      touchPoints: [{ x, y, radiusX: 5, radiusY: 5, force: 1 }],
    });
  }

  async dispatchTouchEnd(): Promise<void> {
    await this.cdp.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
  }

  async dispatchTap(x: number, y: number): Promise<void> {
    await this.dispatchTouchStart(x, y);
    await this.dispatchTouchEnd();
  }

  /** `tap(selector)`: a plain CSS selector, tapped at its own centre. */
  async tap(selector: string): Promise<{ x: number; y: number }> {
    return this.tapExpr(`document.querySelector(${JSON.stringify(selector)})`);
  }

  async tapExpr(expr: string): Promise<{ x: number; y: number }> {
    await this.scrollIntoViewExpr(expr);
    await sleep(60);
    const rect = await this.rectOfExpr(expr);
    if (rect === null) throw new Error(`tap: element not found for ${expr}`);
    const x = rect.left + rect.width / 2;
    const y = rect.top + rect.height / 2;
    await this.dispatchTap(x, y);
    return { x, y };
  }

  /** `longPress(selector, ms)`: touch down, hold, then release. */
  async longPress(selector: string, ms: number): Promise<void> {
    await this.scrollIntoViewExpr(`document.querySelector(${JSON.stringify(selector)})`);
    await sleep(60);
    const rect = await this.rectOfExpr(`document.querySelector(${JSON.stringify(selector)})`);
    if (rect === null) throw new Error(`longPress: element not found for ${selector}`);
    await this.dispatchTouchStart(rect.left + rect.width / 2, rect.top + rect.height / 2);
    await sleep(ms);
    await this.dispatchTouchEnd();
  }

  /** `tapAt(x, y)`: a tap at an arbitrary viewport coordinate. */
  async tapAt(x: number, y: number): Promise<void> {
    await this.dispatchTap(x, y);
  }

  async closeSheetIfOpen(): Promise<void> {
    const present = await this.evalJs<boolean>("document.querySelector('.tip-sheet') !== null");
    if (!present) return;
    const backdrop = await this.rectOfExpr("document.querySelector('.tip-backdrop')");
    if (backdrop !== null) {
      await this.dispatchTap(backdrop.left + 4, backdrop.top + 4);
    } else {
      const close = await this.rectOfExpr("document.querySelector('.tip-close')");
      if (close !== null) await this.dispatchTap(close.left + close.width / 2, close.top + close.height / 2);
    }
    await this.waitFor("document.querySelector('.tip-sheet') === null", 1000);
  }

  async tapOpensSheet(expr: string): Promise<TapOpenResult> {
    await this.closeSheetIfOpen();
    await this.scrollIntoViewExpr(expr);
    await sleep(60);
    const rect = await this.rectOfExpr(expr);
    if (rect === null) throw new Error(`element not found: ${expr}`);
    const x = rect.left + rect.width / 2;
    const y = rect.top + rect.height / 2;
    await this.dispatchTouchStart(x, y);
    const t0 = Date.now();
    await this.dispatchTouchEnd();
    let opened = false;
    let ms = -1;
    const deadline = t0 + 1000;
    while (Date.now() < deadline) {
      if (await this.evalJs<boolean>("document.querySelector('.tip-sheet') !== null")) {
        opened = true;
        ms = Date.now() - t0;
        break;
      }
      await sleep(5);
    }
    const selectionCollapsed = await this.evalJs<boolean>('window.getSelection() ? window.getSelection().isCollapsed : true');
    return { ms, opened, selectionCollapsed };
  }

  async checkAnchor(expr: string): Promise<AnchorCheck> {
    return this.evalJs<AnchorCheck>(`(() => {
      const el = ${expr};
      if (!el) return { exists: false, tagName: '', target: '', rel: '', hrefOk: false, decodedHasV: false, href: '' };
      const href = el.getAttribute('href') || '';
      let decodedHasV = false;
      try {
        const hashIndex = href.indexOf('#');
        const frag = hashIndex >= 0 ? href.slice(hashIndex + 1) : '';
        const padded = frag.replace(/-/g, '+').replace(/_/g, '/');
        const binary = atob(padded + '='.repeat((4 - (padded.length % 4)) % 4));
        const bytes = new Uint8Array(binary.length);
        for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
        const json = JSON.parse(new TextDecoder().decode(bytes));
        decodedHasV = typeof json.v !== 'undefined';
      } catch (error) {
        decodedHasV = false;
      }
      return {
        exists: true,
        tagName: el.tagName,
        target: el.getAttribute('target') || '',
        rel: el.getAttribute('rel') || '',
        hrefOk: href.startsWith('route.html#'),
        decodedHasV,
        href,
      };
    })()`);
  }

  async screenshot(name: string): Promise<void> {
    const res = await this.cdp.send<{ data: string }>('Page.captureScreenshot', {
      format: 'png',
      captureBeyondViewport: false,
    });
    await Bun.write(`${SCREENSHOT_DIR}/${name}`, base64ToBytes(res.data));
  }
}

// --------------------------------------------------------------- chrome glue

async function readDevToolsUrl(proc: Bun.Subprocess): Promise<string> {
  const reader = proc.stderr.getReader();
  const decoder = new TextDecoder();
  let buffer = '';
  const deadline = Date.now() + 15_000;
  try {
    while (Date.now() < deadline) {
      const { value, done } = await reader.read();
      if (done) break;
      buffer += decoder.decode(value, { stream: true });
      const match = buffer.match(/DevTools listening on (ws:\/\/\S+)/);
      if (match !== null && match[1] !== undefined) return match[1];
    }
  } finally {
    reader.releaseLock();
  }
  throw new Error('Chrome did not print a DevTools websocket URL within 15s');
}

interface DevToolsTarget {
  id: string;
  type: string;
  webSocketDebuggerUrl: string;
}

async function firstPageTarget(port: number): Promise<DevToolsTarget> {
  for (let attempt = 0; attempt < 20; attempt += 1) {
    try {
      const response = await fetch(`http://127.0.0.1:${port}/json/list`);
      const list = (await response.json()) as DevToolsTarget[];
      const page = list.find((entry) => entry.type === 'page');
      if (page !== undefined) return page;
    } catch {
      // Chrome may not be listening on the HTTP endpoint quite yet.
    }
    await sleep(150);
  }
  throw new Error('no page target appeared on the DevTools HTTP endpoint');
}

async function killChromeTree(userDataDir: string, proc: Bun.Subprocess): Promise<void> {
  try {
    proc.kill();
  } catch {
    // already gone
  }
  try {
    await proc.exited;
  } catch {
    // ignore
  }
  // Killing the parent does not kill its renderer children; sweep anything
  // still referencing our unique profile directory. That path cannot appear
  // in this script's own command line, so matching on it with `pgrep -f` is
  // safe.
  const found = Bun.spawnSync({ cmd: ['pgrep', '-f', userDataDir] });
  const text = new TextDecoder().decode(found.stdout).trim();
  if (text.length > 0) {
    for (const pid of text.split('\n').map((line) => line.trim()).filter((line) => line.length > 0)) {
      Bun.spawnSync({ cmd: ['kill', '-9', pid] });
    }
  }
}

// -------------------------------------------------------- fetch interception

interface FetchRequestPausedEvent {
  requestId: string;
  request: { url: string; method: string };
}

/**
 * Every fixture response is a cross-origin answer to a real `fetch()` call
 * from the page (127.0.0.1 asking https://www.mvg.de or
 * https://api.transitous.org), so it needs CORS headers of its own: the
 * browser enforces CORS on the response even though the response itself is
 * ours, and without `Access-Control-Allow-Origin` the page's `fetch()`
 * rejects with a bare "Failed to fetch" no matter how faithfully the body is
 * answered.
 */
const CORS_HEADERS = [
  { name: 'access-control-allow-origin', value: '*' },
  { name: 'access-control-allow-methods', value: 'GET, OPTIONS' },
  { name: 'access-control-allow-headers', value: '*' },
];

async function fulfillJson(cdp: Cdp, requestId: string, body: unknown): Promise<void> {
  const text = JSON.stringify(body);
  await cdp.send('Fetch.fulfillRequest', {
    requestId,
    responseCode: 200,
    responseHeaders: [{ name: 'content-type', value: 'application/json; charset=utf-8' }, ...CORS_HEADERS],
    body: toBase64(text),
  });
}

async function fulfillPreflight(cdp: Cdp, requestId: string): Promise<void> {
  await cdp.send('Fetch.fulfillRequest', {
    requestId,
    responseCode: 204,
    responseHeaders: CORS_HEADERS,
  });
}

async function handleRequestPaused(
  cdp: Cdp,
  event: FetchRequestPausedEvent,
  localOrigin: string,
  mvgBody: unknown[],
  planBody: unknown,
  stoptimesBody: unknown,
): Promise<void> {
  const { requestId, request } = event;
  try {
    if (request.url.startsWith(localOrigin)) {
      await cdp.send('Fetch.continueRequest', { requestId });
      return;
    }
    if (request.method === 'OPTIONS') {
      await fulfillPreflight(cdp, requestId);
      return;
    }
    const parsed = new URL(request.url);
    if (parsed.hostname === 'www.mvg.de') {
      if (parsed.pathname.endsWith('/departures')) {
        await fulfillJson(cdp, requestId, mvgBody);
        return;
      }
      if (parsed.pathname.endsWith('/messages')) {
        await fulfillJson(cdp, requestId, []);
        return;
      }
    }
    if (parsed.hostname === 'api.transitous.org') {
      if (parsed.pathname.endsWith('/plan')) {
        await fulfillJson(cdp, requestId, planBody);
        return;
      }
      if (parsed.pathname.endsWith('/stoptimes')) {
        await fulfillJson(cdp, requestId, stoptimesBody);
        return;
      }
      if (parsed.pathname.endsWith('/reverse-geocode') || parsed.pathname.endsWith('/geocode')) {
        await fulfillJson(cdp, requestId, []);
        return;
      }
    }
    // Anything else external: fail it rather than let it reach the real
    // network, so the run stays deterministic and offline.
    await cdp.send('Fetch.failRequest', { requestId, errorReason: 'Failed' });
  } catch (error) {
    console.error(`[e2e] Fetch interception error for ${request.url}: ${error instanceof Error ? error.message : String(error)}`);
    try {
      await cdp.send('Fetch.failRequest', { requestId, errorReason: 'Failed' });
    } catch {
      // the request may already have been handled
    }
  }
}

void main();
