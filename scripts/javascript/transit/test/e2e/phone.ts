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

export const CHROME_PATH = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';

/**
 * Chrome's profile and the run's other scratch files.
 *
 * Derived rather than written down, for the same reason as the screenshots
 * below: what was here was one agent session's scratch path on one machine,
 * committed to a public repository, carrying a home directory name and a
 * session identifier and wrong for every other run.
 */
export const SCRATCH_E2E_DIR = process.env.TRANSIT_E2E_SCRATCH ?? `${process.env.TMPDIR ?? '/tmp'}/transit-e2e-run`;
/**
 * Where the run leaves its screenshots.
 *
 * A directory under the system temp by default rather than a path from
 * whatever machine last ran this, which is what it used to be: a scratch path
 * belonging to one session, committed into a public repository, and wrong for
 * everybody including the next run on the same machine.
 */
const SCREENSHOT_DIR = process.env.TRANSIT_E2E_SCREENSHOTS ?? `${process.env.TMPDIR ?? '/tmp'}/transit-e2e`;

const HERE = import.meta.dir; // .../test/e2e
export const PAGE_DIR = `${HERE}/../../page`;
const FIXTURES_DIR = `${HERE}/fixtures`;

// -------------------------------------------------------------------- utils

export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/** The `Bun.file` slice this harness uses, cast past the package's narrower ambient type. */
export interface FileLike {
  text(): Promise<string>;
  exists(): Promise<boolean>;
  arrayBuffer(): Promise<ArrayBuffer>;
}

export function fileAt(path: string): FileLike {
  return Bun.file(path) as unknown as FileLike;
}

export function contentTypeFor(path: string): string {
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

export function base64ToBytes(b64: string): Uint8Array {
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
export function buildConfig(): ExportedConfig {
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
            via: null,
            destinations: null,
            walk_minutes: 3,
            walk_minutes_by_stop: null,
            stop_labels: null,
            commute: true,
            destination: null,
            connection: null,
          },
          {
            // Deliberately the same stop as the board above, filtered to one
            // line. A profile really does list a stop twice when two lines from
            // it are worth separate boards, and before requests were shared
            // that cost two identical round trips.
            title: 'Nordweg U6',
            stops: ['de:00000:1'],
            modes: null,
            lines: ['U6'],
            direction: null,
            via: null,
            destinations: null,
            walk_minutes: 3,
            walk_minutes_by_stop: null,
            stop_labels: null,
            commute: false,
            destination: null,
            connection: null,
          },
          {
            title: 'Talbogen',
            stops: ['de:00000:2'],
            modes: null,
            lines: null,
            direction: null,
            via: null,
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
            via: null,
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
export const ANCHOR_MS = Date.now();

function isoAt(offsetMin: number): string {
  return new Date(ANCHOR_MS + offsetMin * 60_000).toISOString();
}

interface MvgFixtureRow {
  plannedOffsetMin: number;
  realtimeOffsetMin: number;
  [key: string]: unknown;
}

export async function loadMvgBody(): Promise<unknown[]> {
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

export async function loadPlanBody(): Promise<unknown> {
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

interface StoptimesFixturePlace {
  departureOffsetMin?: number;
  scheduledDepartureOffsetMin?: number;
  [key: string]: unknown;
}

interface StoptimesFixtureRow {
  place?: StoptimesFixturePlace;
  [key: string]: unknown;
}

interface StoptimesFixture {
  stopTimes?: StoptimesFixtureRow[];
  [key: string]: unknown;
}

export async function loadStoptimesBody(): Promise<unknown> {
  const raw = JSON.parse(await fileAt(`${FIXTURES_DIR}/transitous-stoptimes.json`).text()) as StoptimesFixture;
  const stopTimes = (raw.stopTimes ?? []).map((row) => {
    if (row.place === undefined) return row;
    const { departureOffsetMin, scheduledDepartureOffsetMin, ...restPlace } = row.place;
    const place: Record<string, unknown> = { ...restPlace };
    if (departureOffsetMin !== undefined) place.departure = isoAt(departureOffsetMin);
    if (scheduledDepartureOffsetMin !== undefined) place.scheduledDeparture = isoAt(scheduledDepartureOffsetMin);
    return { ...row, place };
  });
  return { ...raw, stopTimes };
}

interface TripFixturePlace {
  arrivalOffsetMin?: number;
  departureOffsetMin?: number;
  scheduledArrivalOffsetMin?: number;
  scheduledDepartureOffsetMin?: number;
  [key: string]: unknown;
}

interface TripFixtureLeg {
  from?: TripFixturePlace;
  to?: TripFixturePlace;
  intermediateStops?: TripFixturePlace[];
}

interface TripFixture {
  legs?: TripFixtureLeg[];
  [key: string]: unknown;
}

function tripPlaceFromOffsets(place: TripFixturePlace | undefined): Record<string, unknown> | undefined {
  if (place === undefined) return undefined;
  const { arrivalOffsetMin, departureOffsetMin, scheduledArrivalOffsetMin, scheduledDepartureOffsetMin, ...rest } = place;
  const out: Record<string, unknown> = { ...rest };
  if (arrivalOffsetMin !== undefined) out.arrival = isoAt(arrivalOffsetMin);
  if (departureOffsetMin !== undefined) out.departure = isoAt(departureOffsetMin);
  if (scheduledArrivalOffsetMin !== undefined) out.scheduledArrival = isoAt(scheduledArrivalOffsetMin);
  if (scheduledDepartureOffsetMin !== undefined) out.scheduledDeparture = isoAt(scheduledDepartureOffsetMin);
  return out;
}

export async function loadTripBody(): Promise<unknown> {
  const raw = JSON.parse(await fileAt(`${FIXTURES_DIR}/transitous-trip.json`).text()) as TripFixture;
  const legs = (raw.legs ?? []).map((leg) => ({
    from: tripPlaceFromOffsets(leg.from),
    to: tripPlaceFromOffsets(leg.to),
    intermediateStops: (leg.intermediateStops ?? []).map((stop) => tripPlaceFromOffsets(stop)),
  }));
  return { legs };
}

/**
 * How many onward calls a sheet names before "and N more", read off
 * `src/page/board.ts` itself rather than copied in as a literal.
 *
 * The number moved once already (8 -> 3, see the file's own comment on
 * `SHEET_CALLS`), and a copy here would have gone stale exactly the way the
 * two onward-calls assertions below did until this read replaced the guess.
 */
export async function sheetCallsFromSource(): Promise<number> {
  const source = await fileAt(`${HERE}/../../src/page/board.ts`).text();
  const match = /const SHEET_CALLS = (\d+);/.exec(source);
  if (match === null) throw new Error('SHEET_CALLS not found in src/page/board.ts; did it move or get renamed?');
  return Number(match[1]);
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

export class Cdp {
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

  // ------------------------------------------------------------ the bundle

  // The server below publishes `page/`, and `page/app.js` in it is a build
  // artefact that nothing else in this run regenerates. It is not in version
  // control either, so a checkout has none and a working tree has whatever the
  // last build left. Both failure modes are silent and both are worse than a
  // broken harness: the run goes green against the previous bundle, which is a
  // harness reporting on code that is no longer there. So the build is part of
  // the run.
  buildPage();

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
  // The script's own idea of which build it is, which the publish script stamps
  // into the bundle separately from the shell. Null means "the same build the
  // shell says", which is every honest deploy. Setting it to something else is
  // how the harness builds the one thing a real deploy must never produce: a
  // page from one build with a script from another.
  let servedBundle: string | null = null;
  const publishBundle = (build: string | null): void => {
    servedBundle = build;
  };
  // Serve one mixed load and heal on the next, which is what a real repair looks
  // like: the mixture is in a cache, the server has one consistent build, and
  // asking again gets it.
  // Counted in loads rather than in requests, because the flip has to land
  // between them: the script of a load is fetched after that load's page, so a
  // server that healed while answering for the page would heal the very load it
  // was supposed to break.
  let mixedLoadsLeft = 0;
  let healing = false;
  const healAfterOneMixedLoad = (): void => {
    mixedLoadsLeft = 1;
    healing = true;
  };
  const STAMPED = new Set(['index.html', 'route.html', 'sw.js']);
  const BUNDLE_STAMPED = new Set(['app.js', 'route.js']);

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
      const bundleStamp = BUNDLE_STAMPED.has(relative);
      if (relative === 'index.html' && mixedLoadsLeft > 0) {
        mixedLoadsLeft -= 1;
      } else if (relative === 'index.html' && servedBundle !== null && healing) {
        healing = false;
        servedBundle = null;
      }
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
          if (bundleStamp) {
            // Only the bundle's own placeholder. The shell's placeholder also
            // appears in the bundle, as the string the build module compares
            // against to answer "was this ever published", and stamping that
            // would make every published page report itself as unpublished.
            return fileAt(filePath)
              .text()
              .then(
                (body) =>
                  new Response(body.replaceAll('__BUNDLE_BUILD_ID__', servedBundle ?? servedBuild), {
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
  const tripBody = await loadTripBody();
  const SHEET_CALLS = await sheetCallsFromSource();

  // Which of this run's rows are BAHN mode, read off the fixture rather than
  // hardcoded: the DOM carries no mode marker, so this is the only honest way
  // to count "distinct BAHN rows rendered" for the laziness assertion below
  // without silently drifting from whatever mvg-departures.json says.
  const bahnLineLabels = new Set(
    (mvgBody as Array<{ transportType?: unknown; label?: unknown }>)
      .filter((row) => row.transportType === 'BAHN')
      .map((row) => String(row.label ?? '')),
  );

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
      void handleRequestPaused(cdp as Cdp, params as FetchRequestPausedEvent, localOrigin, mvgBody, planBody, stoptimesBody, tripBody);
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

    // Both layout measurements are taken twice: once in this machine's fonts and
    // once in a deliberately wider one. A phone renders `system-ui` as Roboto,
    // which is wider than what runs here, and a budget derived from a
    // measurement taken on this machine is therefore a budget for the wrong
    // font. Verdana is wider than Roboto, so a layout that survives it survives
    // the phone; that is the whole point of forcing it rather than guessing.
    const measureContainment = async (): Promise<{ worstOverflowPx: number; worstClass: string; rows: number }> =>
      io.evalJs(`(() => {
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

    const containment = await measureContainment();
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

    const measureOverflow = async (): Promise<{ scrollWidth: number; clientWidth: number; worstRight: number; worstSelector: string }> =>
      io.evalJs(`(() => {
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

    const overflow = await measureOverflow();
    const overflowPass = overflow.scrollWidth === 390 && overflow.clientWidth === 390 && overflow.worstRight <= 391;
    record(
      'no horizontal overflow (scrollWidth=clientWidth=390, worst right <=391px)',
      overflowPass,
      `scrollWidth=${overflow.scrollWidth} clientWidth=${overflow.clientWidth} worstRight=${overflow.worstRight}px (${overflow.worstSelector})`,
    );

    // ------------------------ the sticky bar's strip: may scroll, must say so

    // The controls strip is the one element allowed to overflow, because the
    // horizon, the destination and the start time do not fit side by side on a
    // phone and a control that cannot be reached is worse than one that has to
    // be scrolled to. That licence is only defensible if the strip admits it,
    // so the rule here is "may scroll, must show the fade when it does": the
    // overflowing side carries a mask, and the side already reached does not.
    const stripState = async (): Promise<{
      scrolls: boolean;
      slack: number;
      scrollLeft: number;
      fadeStart: boolean;
      fadeEnd: boolean;
      masked: boolean;
    }> =>
      io.evalJs(`(() => {
      const strip = document.querySelector('.bar-controls');
      if (strip === null) return { scrolls: false, slack: -1, scrollLeft: -1, fadeStart: false, fadeEnd: false, masked: false };
      const style = window.getComputedStyle(strip);
      const mask = style.maskImage || style.webkitMaskImage || 'none';
      const slack = strip.scrollWidth - strip.clientWidth;
      return {
        scrolls: slack > 1,
        slack: Math.round(slack * 100) / 100,
        scrollLeft: Math.round(strip.scrollLeft * 100) / 100,
        fadeStart: strip.classList.contains('fade-start'),
        fadeEnd: strip.classList.contains('fade-end'),
        masked: mask !== 'none' && mask !== '',
      };
    })()`);

    const stripAtStart = await stripState();
    let stripPass: boolean;
    let stripNote: string;
    if (!stripAtStart.scrolls) {
      // Nothing hidden, so nothing to advertise: a fade here would dim a
      // control for no reason.
      stripPass = !stripAtStart.fadeStart && !stripAtStart.fadeEnd && !stripAtStart.masked;
      stripNote = `strip fits (slack=${stripAtStart.slack}px), no fade: masked=${stripAtStart.masked}`;
    } else {
      await io.evalJs(`(() => {
        const strip = document.querySelector('.bar-controls');
        strip.scrollLeft = strip.scrollWidth - strip.clientWidth;
        return true;
      })()`);
      await sleep(120);
      const stripAtEnd = await stripState();
      await io.evalJs(`(() => { document.querySelector('.bar-controls').scrollLeft = 0; return true; })()`);
      await sleep(120);
      const stripBack = await stripState();
      stripPass =
        stripAtStart.fadeEnd &&
        !stripAtStart.fadeStart &&
        stripAtStart.masked &&
        stripAtEnd.fadeStart &&
        !stripAtEnd.fadeEnd &&
        stripAtEnd.masked &&
        stripBack.fadeEnd &&
        !stripBack.fadeStart;
      stripNote =
        `slack=${stripAtStart.slack}px; at left: fade-end=${stripAtStart.fadeEnd} fade-start=${stripAtStart.fadeStart} masked=${stripAtStart.masked}; ` +
        `at right: fade-end=${stripAtEnd.fadeEnd} fade-start=${stripAtEnd.fadeStart} masked=${stripAtEnd.masked}; ` +
        `back at left: fade-end=${stripBack.fadeEnd} fade-start=${stripBack.fadeStart}`;
    }
    record('the controls strip may scroll, and shows the fade when it does', stripPass, stripNote);

    // ------------------------------------ the corner badge: platform and state

    // The platform number and the live mark were two grid tracks and are now
    // one badge in the row's corner. Both of them overflowed as tracks, for the
    // same reason: a track is a promise about width and the font that has to
    // keep it is not the font this machine has. Out of the flow it costs the row
    // nothing, so what is left to prove is that it is really out of the flow,
    // really in the corner, and that its digit is really centred in it.
    const badge = await io.evalJs<{
      rows: number;
      badges: number;
      positioned: number;
      flush: number;
      inside: number;
      centred: number;
      worstEdge: number;
      worstCentre: number;
      clipped: number;
      gridMismatch: string;
    }>(`(() => {
      const rows = document.querySelectorAll('li.row');
      let badges = 0, positioned = 0, flush = 0, inside = 0, centred = 0, clipped = 0;
      let worstEdge = 0, worstCentre = 0, gridMismatch = '';
      for (const row of rows) {
        const mark = row.querySelector(':scope > .platform');
        if (mark === null) continue;
        badges += 1;
        const style = window.getComputedStyle(mark);
        if (style.position === 'absolute') positioned += 1;
        const rr = row.getBoundingClientRect();
        const br = mark.getBoundingClientRect();
        // Flush into the corner: its top edge on the row's top edge and its
        // right edge on the row's right edge, with nothing between.
        const edge = Math.max(Math.abs(br.top - rr.top), Math.abs(br.right - rr.right));
        if (edge <= 1) flush += 1;
        if (edge > worstEdge) worstEdge = edge;
        const out = Math.max(rr.left - br.left, rr.top - br.top, br.right - rr.right, br.bottom - rr.bottom);
        if (out <= 0.5) inside += 1;
        if (mark.scrollWidth > mark.clientWidth + 0.5 || mark.scrollHeight > mark.clientHeight + 0.5) clipped += 1;
        // The text's own box rather than the element's, which is the only way
        // to see a baseline that has drifted: a glyph can sit low inside a box
        // that is itself perfectly placed.
        const text = mark.firstChild;
        if (text === null || text.nodeType !== 3) { centred += 1; continue; }
        const range = document.createRange();
        range.selectNodeContents(mark);
        const tr = range.getBoundingClientRect();
        const dx = Math.abs((tr.left + tr.right) / 2 - (br.left + br.right) / 2);
        const dy = Math.abs((tr.top + tr.bottom) / 2 - (br.top + br.bottom) / 2);
        const off = Math.max(dx, dy);
        if (off <= 1) centred += 1;
        if (off > worstCentre) worstCentre = off;
        // A grid places only the children that are in the flow, so the number
        // of tracks and the number of such children must agree. If the badge
        // still had a track of its own, this is where it would show.
        const tracks = window.getComputedStyle(row).gridTemplateColumns.trim().split(/\\s+/).length;
        let placed = 0;
        for (const child of row.children) if (window.getComputedStyle(child).position !== 'absolute') placed += 1;
        if (tracks !== placed && gridMismatch === '') gridMismatch = tracks + ' tracks for ' + placed + ' placed children';
      }
      return {
        rows: rows.length, badges, positioned, flush, inside, centred, clipped,
        worstEdge: Math.round(worstEdge * 100) / 100,
        worstCentre: Math.round(worstCentre * 100) / 100,
        gridMismatch,
      };
    })()`);
    record(
      'every row carries the corner badge, flush to the row\'s top and right',
      badge.badges === badge.rows && badge.positioned === badge.badges && badge.flush === badge.badges && badge.inside === badge.badges,
      `${badge.badges} badges on ${badge.rows} rows, ${badge.positioned} out of flow, ${badge.flush} flush (worst edge gap ${badge.worstEdge}px), ${badge.inside} inside their row`,
    );
    record(
      'the badge text is centred in it and nothing is clipped',
      badge.badges > 0 && badge.centred === badge.badges && badge.clipped === 0,
      `${badge.centred} of ${badge.badges} centred (worst offset ${badge.worstCentre}px), ${badge.clipped} clipped`,
    );
    record(
      'the row grid has no track for the platform or the state',
      badge.badges > 0 && badge.gridMismatch === '',
      badge.gridMismatch === '' ? 'every row has exactly one track per placed child' : badge.gridMismatch,
    );

    // ------------------------------------------- the countdown column's width

    // The column used to be budgeted for the widest form the headline has, an
    // hour count and two minutes at the far end of the longest horizon. That
    // form is rare and the common one is one or two digits, so two thirds of
    // the column sat blank on almost every row, as a gutter down the left of
    // every board. It is budgeted for two digits now and the long forms are
    // drawn small enough to fit it.
    //
    // Which makes this two assertions rather than one, and the second matters
    // more. A column too narrow for the form being drawn does not ellipsise a
    // number, it cuts the leading digit off a right-aligned cell, and "1:23"
    // without its "1" reads as a completely different time.
    const COMMON_COUNTDOWN = '88';
    // The widest the headline can be, and the other shape it takes at that
    // range. Past ten hours it rounds to whole hours, so nothing five
    // characters long reaches the column any more, and both of these are drawn
    // by the one step-down class.
    const LONG_COUNTDOWN = '9:59';
    const LONGEST_COUNTDOWN = '23h';
    /** How much wider than two digits the column may be before it is a gutter again. */
    const COLUMN_SLACK_PX = 4;
    interface MinutesMeasurement {
      found: boolean;
      clientWidth: number;
      common: number;
      long: number;
      longest: number;
      sizes: string;
    }
    const measureMinutes = async (): Promise<MinutesMeasurement> =>
      io.evalJs(`(() => {
      const cell = document.querySelector('li.row .minutes');
      if (cell === null) return { found: false, clientWidth: 0, common: 0, long: 0, longest: 0, sizes: '' };
      // Cloned and re-classed rather than styled from scratch. The sizes under
      // test are the stylesheet's own, so the probe has to inherit the same
      // cascade the real cell has, step-down classes and all; a probe built
      // from getComputedStyle would measure what this file believes instead of
      // what the page draws, and that is exactly how the previous version of
      // this assertion passed while the column was seventeen pixels too narrow.
      const probe = (extra, text) => {
        const node = cell.cloneNode(false);
        if (extra !== '') node.classList.add(extra);
        node.textContent = text;
        node.style.position = 'absolute';
        node.style.visibility = 'hidden';
        node.style.whiteSpace = 'pre';
        node.style.width = 'auto';
        cell.parentElement.append(node);
        const width = node.getBoundingClientRect().width;
        const size = window.getComputedStyle(node).fontSize;
        node.remove();
        return { width, size };
      };
      const common = probe('', ${JSON.stringify(COMMON_COUNTDOWN)});
      const long = probe('minutes-long', ${JSON.stringify(LONG_COUNTDOWN)});
      const longest = probe('minutes-long', ${JSON.stringify(LONGEST_COUNTDOWN)});
      return {
        found: true,
        clientWidth: cell.clientWidth,
        common: common.width,
        long: long.width,
        longest: longest.width,
        sizes: common.size + '/' + long.size + '/' + longest.size,
      };
    })()`);

    const recordMinutes = (label: string, m: MinutesMeasurement): void => {
      record(
        `the countdown column is two digits wide${label}`,
        m.found && m.clientWidth + 0.5 >= m.common && m.clientWidth <= m.common + COLUMN_SLACK_PX,
        m.found
          ? `column=${m.clientWidth.toFixed(2)} "${COMMON_COUNTDOWN}"=${m.common.toFixed(2)} spare=${(m.clientWidth - m.common).toFixed(2)}px`
          : 'no li.row .minutes found',
      );
      record(
        `the long countdown forms fit that column${label}`,
        m.found && m.long <= m.clientWidth + 0.5 && m.longest <= m.clientWidth + 0.5,
        m.found
          ? `column=${m.clientWidth.toFixed(2)} "${LONG_COUNTDOWN}"=${m.long.toFixed(2)} "${LONGEST_COUNTDOWN}"=${m.longest.toFixed(2)} at ${m.sizes}`
          : 'no li.row .minutes found',
      );
    };

    recordMinutes('', await measureMinutes());

    // ------------------------------------- destination width on a planned row

    // The destination is the row's subject and it is the column that yields
    // first, so it is the one worth measuring rather than trusting. Measured
    // against a sample rendered in the cell's own font rather than against a
    // character count, because "twelve characters" is a different number of
    // pixels in every font the page might be served in, and the font the phone
    // uses is not the one here. Twelve characters of a long station name is the
    // point at which the name still says which station it is.
    const SAMPLE = 'Flughafen Mü';
    const measureDestination = async (): Promise<{
      found: boolean;
      clientWidth: number;
      scrollWidth: number;
      sampleWidth: number;
      text: string;
    }> =>
      io.evalJs(`(() => {
      const row = document.querySelector('li.row:has(a.route)');
      const cell = row === null ? null : row.querySelector('.destination');
      if (cell === null) return { found: false, clientWidth: 0, scrollWidth: 0, sampleWidth: 0, text: '' };
      const probe = document.createElement('span');
      const style = window.getComputedStyle(cell);
      probe.style.position = 'absolute';
      probe.style.visibility = 'hidden';
      probe.style.whiteSpace = 'pre';
      // Longhands rather than the shorthand, for the reason spelled out in the
      // countdown's probe above. This cell's shorthand does serialise today,
      // and it would stop the day anything gives the destination a font feature
      // the shorthand cannot carry, silently and in the passing direction.
      probe.style.fontFamily = style.fontFamily;
      probe.style.fontSize = style.fontSize;
      probe.style.fontWeight = style.fontWeight;
      probe.style.fontStyle = style.fontStyle;
      probe.style.fontStretch = style.fontStretch;
      probe.style.fontVariantNumeric = style.fontVariantNumeric;
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

    const recordDestination = (
      label: string,
      measured: { found: boolean; clientWidth: number; scrollWidth: number; sampleWidth: number; text: string },
    ): void => {
      record(
        label,
        measured.found && measured.clientWidth >= measured.sampleWidth,
        measured.found
          ? `clientWidth=${measured.clientWidth.toFixed(2)} needs=${measured.sampleWidth.toFixed(2)} for "${SAMPLE}", own text "${measured.text}" wants ${measured.scrollWidth}`
          : 'no li.row with an a.route and a .destination found',
      );
    };

    recordDestination(`destination on a row with a journey fits "${SAMPLE}"`, await measureDestination());

    // ------------------------------------------- the same layout, wider font

    // Injected rather than set through CDP: there is no protocol call that
    // changes the page's font, and the emulated-media call does not do it. A
    // stylesheet with `!important` on everything is crude and exact, which is
    // what is wanted.
    await io.evalJs(`(() => {
      const style = document.createElement('style');
      style.id = 'e2e-wide-font';
      style.textContent = '* { font-family: Verdana, sans-serif !important; }';
      document.head.append(style);
      return true;
    })()`);
    await sleep(250);

    const wideContainment = await measureContainment();
    record(
      'containment holds in a wider font than this machine has',
      wideContainment.worstOverflowPx <= 1,
      `Verdana: worst overflow ${wideContainment.worstOverflowPx}px in .${wideContainment.worstClass} across ${wideContainment.rows} rows`,
    );
    const wideOverflow = await measureOverflow();
    record(
      'no horizontal overflow in a wider font than this machine has',
      wideOverflow.scrollWidth === 390 && wideOverflow.clientWidth === 390 && wideOverflow.worstRight <= 391,
      `Verdana: scrollWidth=${wideOverflow.scrollWidth} clientWidth=${wideOverflow.clientWidth} worstRight=${wideOverflow.worstRight}px (${wideOverflow.worstSelector})`,
    );
    recordDestination(
      `destination still fits "${SAMPLE}" in a wider font than this machine has`,
      await measureDestination(),
    );
    recordMinutes(' in a wider font than this machine has', await measureMinutes());

    await io.evalJs(`(() => { document.getElementById('e2e-wide-font')?.remove(); return true; })()`);
    await sleep(250);

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
      // Not the first row's: on a phone a row only draws its second line when
      // there is something on it that the countdown does not already say, so the
      // first on-time row has no times cell at all. Whichever row does.
      { label: 'times', expr: "document.querySelector('li.row .times')" },
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

    // ----------------------------------------------- assertions: state survives data

    // What a reader is in the middle of doing must survive the thirty-second
    // refresh. All four of these were broken by the same thing: the render
    // replaced the whole of `#app`, which detaches every element in it, and a
    // detached element loses its scroll offsets, its focus and its identity.
    // The assertions below scroll a strip, scroll the page, open a popover and
    // put focus in it, then make the fixture say something new and ask for a
    // refresh, and insist that none of the four moved.

    /** Shift every fixture departure, so the next refresh genuinely redraws. */
    const nudgeFixture = (deltaMs: number): void => {
      for (const row of mvgBody as Array<Record<string, unknown>>) {
        for (const field of ['plannedDepartureTime', 'realtimeDepartureTime']) {
          const value = row[field];
          if (typeof value === 'number') row[field] = value + deltaMs;
        }
      }
    };

    // The popover first: opening it taps a control, and a tap scrolls whatever
    // it has to in order to reach one, which would make the scroll readings
    // below measurements of the harness rather than of the page.
    await io.tap('.board .filter-button');
    const popoverOpened = await io.waitFor("document.querySelector('.filter-popover') !== null", 2000);
    const marked = await io.evalJs<boolean>(`(() => {
      const popover = document.querySelector('.filter-popover');
      if (!popover) return false;
      popover.dataset.probe = 'kept';
      const box = popover.querySelector('input[type=checkbox]') || popover.querySelector('input');
      if (box) box.focus();
      return true;
    })()`);

    const before = await io.evalJs<{ strip: number; slack: number; page: number; pageSlack: number; focus: string; rows: string }>(`(() => {
      const strip = document.querySelector('.bar-controls');
      strip.scrollLeft = strip.scrollWidth;
      // Deliberately not the bottom of the page. A refresh changes what the
      // rows say and so, by a pixel or ten, how tall the document is, and a
      // reader parked at the very end is moved by the browser's own clamp
      // rather than by anything this page did.
      window.scrollTo(0, 60);
      const active = document.activeElement;
      return {
        strip: strip.scrollLeft,
        slack: strip.scrollWidth - strip.clientWidth,
        page: window.scrollY,
        pageSlack: document.documentElement.scrollHeight - window.innerHeight,
        focus: active ? (active.className || active.tagName) + '/' + (active.closest('.filter-popover') ? 'in-popover' : 'elsewhere') : 'none',
        rows: [...document.querySelectorAll('li.row .times')].map((node) => node.textContent).join('|'),
      };
    })()`);

    nudgeFixture(60_000);
    // The refresh the page runs every thirty seconds, asked for now. Clicked
    // rather than tapped for the same reason the popover was opened first: a
    // tap would scroll to reach the button and take the measurement with it.
    await io.evalJs("document.querySelector('.refresh').click(); void 0");
    const changed = await io.waitFor(
      `[...document.querySelectorAll('li.row .times')].map((node) => node.textContent).join('|') !== ${JSON.stringify(before.rows)}`,
      10_000,
    );
    await sleep(250);

    const after = await io.evalJs<{ strip: number; page: number; focus: string; popover: boolean; sameNode: boolean; rows: string }>(`(() => {
      const strip = document.querySelector('.bar-controls');
      const popover = document.querySelector('.filter-popover');
      const active = document.activeElement;
      return {
        strip: strip.scrollLeft,
        page: window.scrollY,
        focus: active ? (active.className || active.tagName) + '/' + (active.closest('.filter-popover') ? 'in-popover' : 'elsewhere') : 'none',
        popover: popover !== null,
        sameNode: popover !== null && popover.dataset.probe === 'kept',
        rows: [...document.querySelectorAll('li.row .times')].map((node) => node.textContent).join('|'),
      };
    })()`);

    record(
      'a refresh with new data leaves the controls strip where the reader scrolled it',
      changed && before.slack > 1 && before.strip > 1 && after.strip === before.strip,
      `scrollLeft ${before.strip} -> ${after.strip} of ${before.slack}px of slack; rows changed=${changed}`,
    );
    record(
      'a refresh with new data leaves the page where the reader scrolled it',
      changed && before.page > 1 && after.page === before.page,
      `scrollY ${before.page} -> ${after.page} with ${before.pageSlack}px of page to scroll`,
    );
    record(
      'an open filter popover survives a refresh as the same element',
      popoverOpened && marked && after.popover && after.sameNode,
      `opened=${popoverOpened} still open=${after.popover} same node=${after.sameNode}`,
    );
    record(
      'focus inside the popover survives a refresh',
      marked && after.focus === before.focus && after.focus.endsWith('in-popover'),
      `focus ${before.focus} -> ${after.focus}`,
    );

    // Put the fixture and the page back the way the rest of the run expects
    // them: the same departures, no popover, scrolled to the top.
    nudgeFixture(-60_000);
    await io.evalJs("document.querySelector('.board .filter-button').click(); window.scrollTo(0, 0); void 0");
    await io.waitFor("document.querySelector('.filter-popover') === null", 2000);
    // And a refresh to go with it: the plan fixture answers for the departure
    // times it was written against, so rows left on the nudged times carry no
    // journey and the assertions below would be looking for one.
    await io.evalJs("document.querySelector('.refresh').click(); void 0");
    await io.waitFor("document.querySelector('li.row:has(a.route)') !== null", 15_000);

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

    // ------------------------------------------ onward calls: src/page/calls.ts
    //
    // Where a departure goes after it leaves. `bahnLineLabels` came off the
    // fixture itself rather than a hardcoded line name, so a `.badge` matching
    // one of them is a BAHN row by construction, no matter which line the
    // fixture happens to carry.
    const bahnLabelsJson = JSON.stringify([...bahnLineLabels]);
    const bahnRowsExpr = `[...document.querySelectorAll('li.row')].filter((row) => ${bahnLabelsJson}.includes(row.querySelector(':scope > .badge')?.textContent ?? ''))`;
    const bahnRowExpr = `(${bahnRowsExpr})[0]`;

    await io.closeSheetIfOpen();
    const bahnRowCount = await io.evalJs<number>(`(${bahnRowsExpr}).length`);
    const tripCountAfterRender = tripRequestsFulfilled();
    let lazinessPass: boolean;
    let lazinessNote: string;
    if (bahnRowCount === 0) {
      // Nothing on screen asks about a run unless it is opened, hovered, or a
      // BAHN row, and none of those happened yet at this point in the run.
      lazinessPass = tripCountAfterRender === 0;
      lazinessNote = `no BAHN rows on screen; /trip requests so far=${tripCountAfterRender}`;
    } else {
      // Measured: this fixture's stop is queried by two boards on the primary
      // profile (Nordweg and Talbogen), and the fixture answers every
      // /departures query alike, so the same RE72 run appears as two separate
      // `li.row` elements, each asking calls.ts on its own. Both resolve to the
      // same aggregator tripId, and src/trip.ts's `tripCalls` caches by tripId
      // across the whole page rather than by row, so the network sees one
      // /trip request for two rows that asked. Fewer requests than BAHN rows is
      // that cache working as intended, not a leak; what would be a leak is
      // more requests than rows, or zero.
      lazinessPass = tripCountAfterRender >= 1 && tripCountAfterRender <= bahnRowCount;
      lazinessNote = `${bahnRowCount} BAHN row(s) on screen; /trip requests so far=${tripCountAfterRender}`;
    }
    record('no run is looked up before anything asks', lazinessPass, lazinessNote);

    const callsRowTap = await io.tapOpensSheet(bahnRowExpr);
    if (!callsRowTap.opened) throw new Error('no BAHN row sheet opened for the onward-calls assertions');
    await io.waitFor("document.querySelectorAll('.tip-sheet .tip-call-list li.tip-call').length > 0", 5000);
    // This is also the one row transitous-plan.json gives a matching itinerary
    // under the RE72 line (see that fixture's own comment), so the sheet's
    // journey section and its calls section can each still be mid-resolve for
    // a moment after the sheet opens; either one landing replaces the sheet
    // body (same mechanism as `callsSignature` above), which raced the "more"
    // tap below and made it land on a button already detached from the page.
    // Waiting out both spinners first, `.tip-planning` covers each of them
    // since rowSheet's own wait and callsSection's share the class, is what
    // the rest of this section assumes.
    await io.waitFor("document.querySelector('.tip-sheet .tip-planning') === null", 5000);

    const firstCalls = await io.evalJs<Array<{ name: string; time: string }>>(`(() => {
      return [...document.querySelectorAll('.tip-sheet .tip-call-list li.tip-call')].map((item) => ({
        name: item.querySelector('.tip-call-name')?.textContent ?? '',
        time: item.querySelector('.tip-call-time')?.textContent ?? '',
      }));
    })()`);
    // The trip fixture's own origin name (transitous-trip.json's leg.from.name),
    // which callsAfter is supposed to drop because it is where this row's
    // reader is already standing.
    const tripOriginName = 'Nordweg';
    record(
      "a row's sheet says where the train goes",
      firstCalls.length >= 2 &&
        firstCalls.every((call) => call.name.length > 0 && /^\d{1,2}:\d{2}$/.test(call.time)) &&
        firstCalls[0]?.name !== tripOriginName,
      `${firstCalls.length} calls; first three: ${firstCalls
        .slice(0, 3)
        .map((call) => `${call.name} ${call.time}`)
        .join(', ')}`,
    );

    // Sheet order, fit, and collapse: all three read this same sheet before the
    // "more" tap below touches it, and that placement is load-bearing rather
    // than cosmetic. `expandCalls` (src/page/calls.ts) marks a row's key
    // expanded in a module-level Set that nothing here ever clears, so once
    // this row's "more" is tapped its calls list stays expanded, on this open
    // sheet and on every sheet this key opens again, for the rest of the run.
    // A collapsed-list assertion after that tap would not be testing the
    // collapse; it would be testing whatever was left over from testing the
    // expansion. This is also the one row transitous-plan.json now gives a
    // matching RE72 itinerary (see that fixture's comment), so it is the one
    // sheet in this fixture set that carries both a journey and a calls list at
    // once, which is what the ordering and fit checks below need.

    const sheetOrder = await io.evalJs<string[]>(`(() => {
      const sheet = document.querySelector('.tip-sheet');
      if (!sheet) return [];
      return [...sheet.querySelectorAll('.tip-journey, .tip-calls')].map((node) => node.className);
    })()`);
    const journeyPresent = await io.evalJs<boolean>(
      "document.querySelector('.tip-sheet .tip-body.tip-journey') !== null",
    );
    const callsPresent = await io.evalJs<boolean>("document.querySelector('.tip-sheet .tip-calls') !== null");
    const journeyIndex = sheetOrder.findIndex((cls) => cls.includes('tip-journey'));
    const callsIndex = sheetOrder.findIndex((cls) => cls.includes('tip-calls'));
    record(
      'the journey comes before the calls in the sheet',
      journeyPresent && callsPresent && journeyIndex !== -1 && callsIndex !== -1 && journeyIndex < callsIndex,
      `order: [${sheetOrder.join(', ')}]`,
    );

    // journeyBottom against sheetBottom rather than against the viewport: the
    // sheet is what caps itself at 85vh and scrolls internally (page/theme.css),
    // so a journey that fits the sheet is a journey the reader sees without
    // touching it, whatever the sheet's own position on the screen is. The
    // 844px viewport check is the belt to that suspenders, in case a future
    // change centres a sheet taller than the phone itself.
    //
    // Worth knowing what this one does and does not catch. Reordering the sheet
    // does not change how tall it is, and at this fixture's size the whole thing
    // is well under the 85vh cap, so the cap never binds here and reordering
    // alone cannot make this assertion fail. The assertion above it, on document
    // order, is the one that reproduces the report; this is the guard against
    // the sheet growing until the journey no longer fits, which is the shape the
    // bug took on real data, where a journey carries alternatives and a call
    // list carries a real run.
    const fit = await io.evalJs<{
      scrollTop: number;
      journeyTop: number;
      journeyBottom: number;
      sheetTop: number;
      sheetBottom: number;
      clientHeight: number;
      scrollHeight: number;
    } | null>(`(() => {
      const sheet = document.querySelector('.tip-sheet');
      const journey = document.querySelector('.tip-sheet .tip-body.tip-journey');
      if (!sheet || !journey) return null;
      const sheetRect = sheet.getBoundingClientRect();
      const journeyRect = journey.getBoundingClientRect();
      return {
        scrollTop: sheet.scrollTop,
        journeyTop: journeyRect.top,
        journeyBottom: journeyRect.bottom,
        sheetTop: sheetRect.top,
        sheetBottom: sheetRect.bottom,
        clientHeight: sheet.clientHeight,
        scrollHeight: sheet.scrollHeight,
      };
    })()`);
    record(
      'the journey is on screen without scrolling the sheet',
      fit !== null && fit.scrollTop === 0 && fit.journeyBottom <= fit.sheetBottom + 1 && fit.sheetBottom <= 844 + 1,
      fit === null
        ? 'no .tip-sheet or .tip-sheet .tip-body.tip-journey found'
        : `journey top=${Math.round(fit.journeyTop)} bottom=${Math.round(fit.journeyBottom)}; ` +
          `sheet top=${Math.round(fit.sheetTop)} bottom=${Math.round(fit.sheetBottom)} clientHeight=${fit.clientHeight} scrollHeight=${fit.scrollHeight}`,
    );

    const collapse = await io.evalJs<{ count: number; moreText: string | null }>(`(() => {
      const sheet = document.querySelector('.tip-sheet');
      if (!sheet) return { count: 0, moreText: null };
      return {
        count: sheet.querySelectorAll('.tip-call-list li.tip-call').length,
        moreText: sheet.querySelector('button.tip-more')?.textContent ?? null,
      };
    })()`);
    record(
      'the calls list is collapsed until asked',
      collapse.count === SHEET_CALLS && collapse.moreText !== null,
      `count=${collapse.count} (SHEET_CALLS=${SHEET_CALLS}) button text="${collapse.moreText ?? ''}"`,
    );

    // Marked on `.tip-sheet` itself, not `.tip-sheet-body`: expanding the list
    // changes the sheet's signature (src/page/board.ts's `callsSignature`), and
    // a changed signature rebuilds the body from scratch (src/page/tip.ts's
    // `rebindTip` does `open.body.replaceChildren(build())`). The outer sheet
    // is what stays the same node across that, the same one assertion 5 above
    // proves survives a render; the body underneath it does not, and is not
    // supposed to.
    await io.evalJs(
      `(() => { const sheet = document.querySelector('.tip-sheet'); if (sheet) sheet.__e2eCallsMarker = 'calls-marker'; return null; })()`,
    );
    const moreLabel = await io.evalJs<string>("document.querySelector('.tip-sheet button.tip-more')?.textContent ?? ''");
    const callsBefore = await io.evalJs<number>("document.querySelectorAll('.tip-sheet .tip-call-list li.tip-call').length");
    // Scrolled into view first: a call list long enough to need "and N more"
    // is long enough to push the button below the fold of a sheet that
    // scrolls, and a tap at its rect's coordinates lands on nothing if the
    // sheet has not been scrolled there. A synthetic touch tap on this
    // particular button, inside a container CDP had just scrolled
    // programmatically, did not reliably turn into a click; a real mouse
    // click at the same point did, every time it was tried, so this falls
    // back to one exactly the way the anchor taps above fall back from a
    // blocked touch tap to a mouse click.
    await io.scrollIntoViewExpr("document.querySelector('.tip-sheet button.tip-more')");
    await sleep(60);
    const moreRect = await io.rectOfExpr("document.querySelector('.tip-sheet button.tip-more')");
    if (moreRect === null) throw new Error('button.tip-more not found before expanding the call list');
    const moreX = moreRect.left + moreRect.width / 2;
    const moreY = moreRect.top + moreRect.height / 2;
    await io.dispatchTap(moreX, moreY);
    let expandedListTapped = await io.waitFor("document.querySelector('.tip-sheet button.tip-more') === null", 800);
    if (!expandedListTapped) {
      await cdp.send('Input.dispatchMouseEvent', { type: 'mousePressed', x: moreX, y: moreY, button: 'left', clickCount: 1 });
      await cdp.send('Input.dispatchMouseEvent', { type: 'mouseReleased', x: moreX, y: moreY, button: 'left', clickCount: 1 });
      expandedListTapped = await io.waitFor("document.querySelector('.tip-sheet button.tip-more') === null", 800);
    }
    const sheetSameNode = await io.evalJs<boolean>(
      "document.querySelector('.tip-sheet')?.__e2eCallsMarker === 'calls-marker'",
    );
    const sheetStillOpen = await io.evalJs<boolean>("document.querySelector('.tip-sheet') !== null");
    const moreGone = await io.evalJs<boolean>("document.querySelector('.tip-sheet button.tip-more') === null");
    const callsAfterExpand = await io.evalJs<number>(
      "document.querySelectorAll('.tip-sheet .tip-call-list li.tip-call').length",
    );
    record(
      'the rest of the list is one tap away',
      /^and \d+ more$/.test(moreLabel) && (sheetSameNode || sheetStillOpen) && moreGone && callsAfterExpand > callsBefore,
      `"${moreLabel}" tapped; sheet same node=${sheetSameNode} still open=${sheetStillOpen} tip-more gone=${moreGone} calls ${callsBefore} -> ${callsAfterExpand}`,
    );

    const sheetOverflow = await io.evalJs<{ scrollWidth: number; worstOverPx: number; worstClass: string }>(`(() => {
      const sheet = document.querySelector('.tip-sheet');
      const de = document.documentElement;
      if (!sheet) return { scrollWidth: de.scrollWidth, worstOverPx: -1, worstClass: '(no .tip-sheet)' };
      const sheetRect = sheet.getBoundingClientRect();
      let worst = 0; let worstClass = '';
      for (const element of sheet.querySelectorAll('*')) {
        const r = element.getBoundingClientRect();
        if (r.width === 0 && r.height === 0) continue; // a truly empty box cannot overflow
        const over = r.right - sheetRect.right;
        if (over > worst) { worst = over; worstClass = element.className || element.tagName; }
      }
      return { scrollWidth: de.scrollWidth, worstOverPx: Math.round(worst * 100) / 100, worstClass };
    })()`);
    record(
      'a sheet that says where the train goes still fits the phone',
      sheetOverflow.scrollWidth <= 390 && sheetOverflow.worstOverPx <= 1,
      `scrollWidth=${sheetOverflow.scrollWidth} worst right overshoot=${sheetOverflow.worstOverPx}px in .${sheetOverflow.worstClass}`,
    );

    await io.closeSheetIfOpen();

    if (bahnRowCount > 0) {
      const viaText = await io.evalJs<string>(`(${bahnRowExpr})?.querySelector('.row-via')?.textContent ?? ''`);
      const rowBudgetWithVia = await measureContainment();
      record(
        'a regional row says where it goes on the row itself',
        /^via .+/.test(viaText) && rowBudgetWithVia.worstOverflowPx <= 1,
        `via text "${viaText}"; row containment worst overflow ${rowBudgetWithVia.worstOverflowPx}px in .${rowBudgetWithVia.worstClass} across ${rowBudgetWithVia.rows} rows`,
      );
    } else {
      // mvg-departures.json's RE72 row is this fixture's only BAHN-mode entry;
      // if that ever stops being true, the row's own "via" hint goes untested
      // here rather than being asserted against a row invented for the purpose.
      console.log('[e2e] no BAHN rows in this fixture; the row-via assertion was skipped');
    }

    // board.ts's renderRow appends `.row-via` to `.row-times`, a sibling of the
    // route anchor under `.row-main`'s neighbour, not a child of `.route` and
    // not a competitor for its grid track. This is the same claim the sheet's
    // journey-before-calls order makes about the sheet: a later addition to a
    // row must not narrow or displace what was already there. RE72 is both this
    // fixture's one BAHN row (so the only one that can carry `.row-via`) and,
    // since transitous-plan.json's third itinerary, one with a filled `.route`
    // too, so the strong form below (equal widths) is what actually runs here;
    // the weaker form stays in as the fallback this assertion degrades to if a
    // future fixture change ever separates those two facts again.
    if (bahnRowCount > 0) {
      const viaSlotCheck = await io.evalJs<{
        bahnRouteWidth: number | null;
        bahnRouteLeft: number | null;
        bahnRouteRight: number | null;
        otherRouteWidth: number | null;
        viaInsideRoute: boolean | null;
        viaOverlapsRoute: boolean | null;
        bahnHasVia: boolean;
        bahnRouteFilled: boolean;
      }>(`(() => {
        const bahnRow = ${bahnRowExpr};
        const otherRow = ${plannedRowExpr};
        const bahnRoute = bahnRow ? bahnRow.querySelector('.route') : null;
        const bahnVia = bahnRow ? bahnRow.querySelector('.row-via') : null;
        const otherRoute = otherRow ? otherRow.querySelector('.route') : null;
        const bahnRouteRect = bahnRoute ? bahnRoute.getBoundingClientRect() : null;
        const viaRect = bahnVia ? bahnVia.getBoundingClientRect() : null;
        let viaOverlapsRoute = null;
        if (bahnRouteRect && viaRect) {
          viaOverlapsRoute = viaRect.left < bahnRouteRect.right && viaRect.right > bahnRouteRect.left;
        }
        return {
          bahnRouteWidth: bahnRouteRect ? bahnRouteRect.width : null,
          bahnRouteLeft: bahnRouteRect ? bahnRouteRect.left : null,
          bahnRouteRight: bahnRouteRect ? bahnRouteRect.right : null,
          otherRouteWidth: otherRoute ? otherRoute.getBoundingClientRect().width : null,
          viaInsideRoute: bahnRoute && bahnVia ? bahnRoute.contains(bahnVia) : null,
          viaOverlapsRoute,
          bahnHasVia: bahnVia !== null,
          bahnRouteFilled: bahnRoute !== null && !bahnRoute.classList.contains('slot-empty'),
        };
      })()`);

      if (viaSlotCheck.bahnHasVia && viaSlotCheck.bahnRouteFilled && viaSlotCheck.otherRouteWidth !== null) {
        const widthDiff = Math.abs((viaSlotCheck.bahnRouteWidth ?? 0) - (viaSlotCheck.otherRouteWidth ?? 0));
        record(
          "a regional row's via text does not take the journey's slot",
          viaSlotCheck.viaInsideRoute === false && widthDiff <= 1,
          `route width with via=${viaSlotCheck.bahnRouteWidth}px, without via=${viaSlotCheck.otherRouteWidth}px; ` +
            `.row-via inside .route=${viaSlotCheck.viaInsideRoute}`,
        );
      } else {
        // No row in this fixture carries both a filled `.route` and a
        // `.row-via` at once (the fixture edit above is what makes RE72 carry
        // both; if that ever regresses, fall back to the weaker claim: the via
        // text is never nested inside the route slot and never shares its
        // horizontal extent, so it cannot be squeezing it even without a
        // same-board row to compare widths against).
        record(
          "a regional row's via text does not take the journey's slot",
          viaSlotCheck.viaInsideRoute !== true && viaSlotCheck.viaOverlapsRoute !== true,
          `no row with both a filled route and a via hint; falling back to the weaker claim: ` +
            `.row-via inside .route=${viaSlotCheck.viaInsideRoute}, overlaps .route=${viaSlotCheck.viaOverlapsRoute}`,
        );
      }
    } else {
      console.log("[e2e] no BAHN rows in this fixture; the via-vs-route-slot assertion was skipped");
    }

    // ------------------------------------------------ timing, under a phone's
    //
    // Measured here rather than quoted from the machine this runs on, with the
    // processor slowed to a quarter and the network held to a slow mobile
    // connection, because the numbers that matter are the ones on a phone.
    //
    // Two limits worth stating rather than burying. The fixtures are fulfilled
    // through the debugging protocol, so the emulated bandwidth does not apply
    // to them the way it would to a real response; what the delay below models
    // is the round trip, which is the part that decides whether asking in
    // parallel was worth doing. And the processor throttle is a multiplier on
    // this machine's processor, not a model of any particular phone.

    await cdp.send('Emulation.setCPUThrottlingRate', { rate: 4 });
    await cdp.send('Network.emulateNetworkConditions', {
      offline: false,
      latency: 150,
      downloadThroughput: Math.round((1.6 * 1024 * 1024) / 8),
      uploadThroughput: Math.round((750 * 1024) / 8),
      connectionType: 'cellular4g',
    });
    upstreamDelayMs = 300;

    await cdp.send('Page.reload', { ignoreCache: true });
    const rowsAt = Date.now();
    const gotRows = await io.waitFor("document.querySelectorAll('li.row').length > 0", 40_000, 50);
    const toRows = Date.now() - rowsAt;
    // Waited on the page's own record rather than on a route anchor appearing.
    // Anchors show up before the planner answers, because the plans from the
    // last visit are restored from storage and drawn as stale, which is the
    // right thing for a reader and a trap for a stopwatch.
    const gotRoutes = await io.waitFor(
      'typeof window.__transitTiming === "function" && window.__transitTiming() !== null',
      40_000,
      50,
    );
    const toRoutes = Date.now() - rowsAt;

    // The page's own record, which is what a reader sees in the filter popover
    // and what makes the comparison below possible from a single run.
    const run = await io.evalJs<{
      toRowsMs: number | null;
      toRoutesMs: number | null;
      planMs: number | null;
      renderMs: number | null;
      boards: Array<{ title: string; departuresMs: number; pages: number; shared: number }>;
    } | null>(`(() => {
      const w = window;
      return w.__transitTiming ? w.__transitTiming() : null;
    })()`);

    const boardMs = (run?.boards ?? []).map((board) => board.departuresMs);
    const slowest = boardMs.length === 0 ? 0 : Math.max(...boardMs);
    const sequential = boardMs.reduce((sum, ms) => sum + ms, 0);
    const shared = (run?.boards ?? []).reduce((sum, board) => sum + board.shared, 0);
    record(
      'a board that repeats a stop costs no second request',
      shared >= 1,
      `${shared} of the run's stop requests were answered by one already in flight`,
    );
    record(
      'timing under 4x CPU and a slow mobile connection',
      gotRows && gotRoutes,
      `to rows ${toRows} ms, to routes ${toRoutes} ms; boards [${boardMs.join(', ')}] ms, ` +
        `slowest ${slowest} ms, same work in sequence would be ${sequential} ms, ${shared} request(s) shared, ` +
        `plan ${run?.planMs ?? '-'} ms, render ${run?.renderMs ?? '-'} ms`,
    );

    upstreamDelayMs = 0;
    await cdp.send('Emulation.setCPUThrottlingRate', { rate: 1 });
    await cdp.send('Network.emulateNetworkConditions', {
      offline: false,
      latency: 0,
      downloadThroughput: -1,
      uploadThroughput: -1,
    });

    // ------------------------------------------- assertion: the route page

    // The expanded view was a page and became a render function that the page
    // and the in-app overlay both call, and its styles moved out of the page's
    // own document into the shared one. Both of those are refactors that break
    // the page silently: every other assertion here reads the href of a link to
    // it and none of them ever loaded it. So this one does.

    const routeHref = await io.evalJs<string | null>(
      "(() => { const a = document.querySelector('a.route') ?? document.querySelector('.tip-open'); return a ? a.getAttribute('href') : null; })()",
    );
    if (routeHref === null) {
      record('the route page still renders a journey it is handed', false, 'no route link on the board to follow');
    } else {
      await cdp.send('Page.navigate', { url: `${localOrigin}/${routeHref}` });
      const rendered = await io.waitFor("document.querySelector('.route-option') !== null", 10_000, 50);
      const styled = await io.evalJs<{ head: string; option: string; title: string }>(`(() => {
        const head = document.querySelector('.route-page-head');
        const option = document.querySelector('.route-option');
        return {
          head: head === null ? '' : window.getComputedStyle(head).position,
          option: option === null ? '' : window.getComputedStyle(option).borderTopWidth,
          title: document.title,
        };
      })()`);
      record(
        'the route page still renders a journey it is handed',
        rendered && styled.head === 'sticky' && styled.option !== '0px',
        `journey drawn=${rendered}, its head is ${styled.head || 'absent'}, its card border is ${styled.option || 'absent'}, title "${styled.title}"`,
      );
    }

    // ------------------------------------ assertion: the installed app's overlay

    // Installed to a home screen the page runs standalone, and standalone has
    // no tabs. Every "open" on this page was written for a browser, where a new
    // tab is free and leaves the board exactly where it was; in the installed
    // app the same link has nowhere to go. So in standalone the expanded view is
    // drawn over the board instead, with a history entry behind it so that the
    // system back gesture closes it without the page having to guess.
    //
    // Emulated by overriding the media query before the document runs, which is
    // the only way: there is no protocol call that puts a page in standalone,
    // and the page asks the question once, on the tap.

    const standaloneShim = await cdp.send<{ identifier: string }>('Page.addScriptToEvaluateOnNewDocument', {
      source: `(() => {
        const real = window.matchMedia.bind(window);
        window.matchMedia = (query) =>
          String(query).includes('display-mode: standalone')
            ? { matches: true, media: query, addListener() {}, removeListener() {}, addEventListener() {}, removeEventListener() {}, dispatchEvent() { return false; } }
            : real(query);
      })()`,
    });
    await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });
    await io.waitFor("document.querySelectorAll('li.row').length > 0", 15_000);
    await io.waitFor("document.querySelector('li.row a.route') !== null", 20_000, 100);

    const sheetOpened = await io.tapOpensSheet("document.querySelector('li.row:has(a.route)')");
    record(
      'standalone: a row with a journey still opens its sheet',
      sheetOpened.opened,
      sheetOpened.opened ? `opened in ${sheetOpened.ms}ms` : 'no .tip-sheet within 1s',
    );

    const trailingLabel = await io.evalJs<string>(
      `(document.querySelector('.tip-sheet .tip-open')?.textContent ?? '').trim()`,
    );
    record(
      'standalone: the trailing link does not promise a tab',
      trailingLabel === 'Open full view',
      `the link reads "${trailingLabel}"`,
    );

    // Whichever "open" the sheet is showing. The alternatives list carries the
    // short one and is what the reader taps most, so it is preferred.
    const openExpr =
      "(document.querySelector('.tip-sheet .tip-alternative-open') ?? document.querySelector('.tip-sheet .tip-open'))";
    const historyBefore = await io.evalJs<number>('history.length');
    const tabsBeforeOverlay = seenTargets.filter((entry) => entry.type === 'page').length;
    const openRect = await io.rectOfExpr(openExpr);
    if (openRect === null) {
      record('standalone: tapping "open" draws the journey over the board', false, 'no open link in the sheet');
    } else {
      await io.dispatchTap(openRect.left + openRect.width / 2, openRect.top + openRect.height / 2);
      const overlayUp = await io.waitFor("document.querySelector('.route-overlay') !== null", 3000, 25);
      const historyAfter = await io.evalJs<number>('history.length');
      const drewJourney = await io.evalJs<boolean>(
        "document.querySelector('.route-overlay .route-option') !== null || document.querySelector('.route-overlay .empty') !== null",
      );
      const newTabs = seenTargets.filter((entry) => entry.type === 'page').length;
      record(
        'standalone: tapping "open" draws the journey over the board',
        overlayUp && drewJourney,
        `overlay=${overlayUp} journey drawn=${drewJourney}`,
      );
      record(
        'standalone: opening pushes exactly one history entry',
        historyAfter === historyBefore + 1,
        `history.length ${historyBefore} -> ${historyAfter}`,
      );
      record(
        'standalone: nothing is opened in a tab',
        newTabs === tabsBeforeOverlay,
        `${newTabs - tabsBeforeOverlay} target(s) created by the tap`,
      );

      // Tapped rather than driven through the history, because the control has
      // to be reachable by a finger as well as correct: the overlay is drawn
      // over a sheet that has its own backdrop, and a stacking order that put
      // the sheet on top would leave a view that renders perfectly and cannot
      // be dismissed. Going back through the history would have passed.
      const backRect = await io.rectOfExpr("document.querySelector('.route-overlay .route-back')");
      if (backRect === null) {
        record('standalone: the overlay\'s back control closes it and leaves the sheet', false, 'no .route-back in the overlay');
      } else {
        await io.dispatchTap(backRect.left + backRect.width / 2, backRect.top + backRect.height / 2);
        const overlayGone = await io.waitFor("document.querySelector('.route-overlay') === null", 3000, 25);
        const sheetSurvived = await io.evalJs<boolean>("document.querySelector('.tip-sheet') !== null");
        const historyBack = await io.evalJs<number>('history.length');
        record(
          'standalone: the overlay\'s back control closes it and leaves the sheet',
          overlayGone && sheetSurvived,
          `tapped .route-back: overlay gone=${overlayGone} sheet still open=${sheetSurvived} history.length=${historyBack}`,
        );
      }
    }

    await cdp.send('Page.removeScriptToEvaluateOnNewDocument', { identifier: standaloneShim.identifier });
    await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });
    await io.waitFor("document.querySelectorAll('li.row').length > 0", 15_000);
    const browserLabel = await io.evalJs<string>(
      `(() => {
        const row = document.querySelector('li.row:has(a.route)');
        if (row === null) return 'no planned row';
        return document.querySelector('.tip-open')?.textContent?.trim() ?? 'sheet closed';
      })()`,
    );
    record(
      'in a tab the behaviour is unchanged',
      browserLabel === 'sheet closed' || browserLabel === 'Open in a new tab',
      `with no standalone shim the sheet's link reads "${browserLabel}"`,
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

    // ------------------------------------------- assertions: the mixed shell
    //
    // The failure this pair was written for reached a reader's phone. The old
    // worker refreshed its cache one file at a time, so a client could hold a
    // script from one build beside a page and a stylesheet from another, and a
    // board drawn by one build under the layout of another does not degrade
    // politely: rows ran off the side of the screen, the state word spilled out
    // of a badge that had nowhere to sit, and the whole page panned sideways.
    // Nothing on the server was wrong, which is why no check against the server
    // saw it for two deploys.
    //
    // The worker no longer writes anything outside its install, and the shell's
    // files carry the version in their URL, so the mixture cannot form any more.
    // This is the third line of defence, for a mixture that got into a cache
    // before either of those existed: the page and the script each carry a build
    // id, and a page that finds they disagree repairs itself.

    const mixedState = async (): Promise<{ build: string | null; rows: number; navigation: string; marker: boolean }> =>
      io.evalJs(`(() => {
        const nav = performance.getEntriesByType('navigation')[0];
        return {
          build: window.__BUILD__ ?? null,
          rows: document.querySelectorAll('li.row').length,
          navigation: nav ? nav.type : 'unknown',
          marker: window.__e2eMarker === true,
        };
      })()`);

    // Case one: the server has a consistent build and the mixture is only in the
    // client, which is the real case. The first load is served mixed, the repair
    // asks again, and the second load is whole.
    await io.evalJs('sessionStorage.clear(); void 0');
    publish('ccccccc-20260303', 'cccccccccccc');
    publishBundle('ddddddd-20260404');
    healAfterOneMixedLoad();
    await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });
    const healedRows = await io.waitFor("document.querySelectorAll('li.row').length > 0", 25_000, 100);
    const healed = await mixedState();
    const healedOverflow = await measureOverflow();
    record(
      'a mixed shell reloads itself onto one build',
      healedRows && healed.build === 'ccccccc-20260303' && healed.navigation === 'reload' && healedOverflow.worstRight <= 391,
      `rows=${healed.rows} window.__BUILD__=${String(healed.build)} navigationType=${healed.navigation} ` +
        `worstRight=${healedOverflow.worstRight}px (${healedOverflow.worstSelector})`,
    );

    // Case two: the disagreement survives the reload, which means it is the
    // server's and not a cache's. The page must reload once, give up, and draw
    // itself rather than reloading for ever. A marker set after the rows appear
    // is the loop detector: another reload would wipe it.
    await io.evalJs('sessionStorage.clear(); void 0');
    publish('eeeeeee-20260505', 'eeeeeeeeeeee');
    publishBundle('fffffff-20260606');
    await cdp.send('Page.navigate', { url: `${localOrigin}/index.html` });
    const stuckRows = await io.waitFor("document.querySelectorAll('li.row').length > 0", 25_000, 100);
    await io.evalJs('window.__e2eMarker = true; void 0');
    await sleep(3_000);
    const stuck = await mixedState();
    const stuckOverflow = await measureOverflow();
    record(
      'a disagreement the reload cannot fix stops rather than loops',
      stuckRows && stuck.navigation === 'reload' && stuck.marker && stuck.rows > 0 && stuckOverflow.worstRight <= 391,
      `rows=${stuck.rows} markerSurvived=${stuck.marker} navigationType=${stuck.navigation} ` +
        `window.__BUILD__=${String(stuck.build)} worstRight=${stuckOverflow.worstRight}px`,
    );

    publishBundle(null);

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

export class PageIo {
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

/**
 * Rebuild the page bundles the static server is about to publish.
 *
 * The same command `bun run build` runs, spelled out here rather than shelled
 * out to the package script, so this cannot silently start testing whatever a
 * renamed script happens to do.
 */
export function buildPage(): void {
  const root = `${HERE}/../..`;
  for (const [entry, out] of [
    ['src/page.ts', 'page/app.js'],
    ['src/route.ts', 'page/route.js'],
  ] as const) {
    const built = Bun.spawnSync({
      cmd: ['bun', 'build', `${root}/${entry}`, '--target', 'browser', '--outfile', `${root}/${out}`],
      stdout: 'pipe',
      stderr: 'pipe',
    });
    if (built.exitCode !== 0) {
      throw new Error(`[e2e] building ${out} failed:\n${new TextDecoder().decode(built.stderr)}`);
    }
  }
}

export async function readDevToolsUrl(proc: Bun.Subprocess): Promise<string> {
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

export async function firstPageTarget(port: number): Promise<DevToolsTarget> {
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

export async function killChromeTree(userDataDir: string, proc: Bun.Subprocess): Promise<void> {
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

export interface FetchRequestPausedEvent {
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

/**
 * A delay added to every upstream answer, in milliseconds.
 *
 * Zero for the correctness assertions, where a fixture that answers instantly is
 * exactly what makes them repeatable. Set for the timing section, where an
 * instant upstream would hide the only thing worth measuring: whether the page
 * waits for its requests one after another or all at once.
 */
let upstreamDelayMs = 0;

/**
 * How many `/trip?tripId=...` requests this run has fulfilled.
 *
 * The whole point of `src/page/calls.ts` is that nothing asks the aggregator
 * about a vehicle's run until something on screen needs to know, so this is
 * the harness's only window onto whether that held: counted here, at the one
 * place every such request actually lands, rather than guessed at from the
 * page's own state.
 */
let tripRequestCount = 0;

/** Read-only outside this module; only `handleRequestPaused` increments it. */
export function tripRequestsFulfilled(): number {
  return tripRequestCount;
}

async function fulfillJson(cdp: Cdp, requestId: string, body: unknown): Promise<void> {
  if (upstreamDelayMs > 0) await sleep(upstreamDelayMs);
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

export async function handleRequestPaused(
  cdp: Cdp,
  event: FetchRequestPausedEvent,
  localOrigin: string,
  mvgBody: unknown[],
  planBody: unknown,
  stoptimesBody: unknown,
  tripBody: unknown,
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
      if (parsed.pathname.endsWith('/trip')) {
        tripRequestCount += 1;
        await fulfillJson(cdp, requestId, tripBody);
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

// Only when this file *is* the command. The screenshot-only entry point beside
// it imports the server, the Chrome start and the fixtures from here, and an
// import that ran the whole assertion suite as a side effect would make that
// impossible. `import.meta.main` is true for the file bun was invoked with and
// false for anything it pulled in.
if (import.meta.main) void main();
