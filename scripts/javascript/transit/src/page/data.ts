import { chain } from '../backends/chain.ts';
import { BEYOND_HORIZON_EXTENSION_MINUTES } from '../config.ts';
import { share, withLimit } from '../inflight.ts';
import { configureCalls } from './calls.ts';
import { recordBoard } from './timing.ts';
import { createMvgBackend, MVG_DEFAULT_BASE_URL } from '../backends/mvg.ts';
import { createTransitousBackend, TRANSITOUS_DEFAULT_BASE_URL } from '../backends/transitous.ts';
import type { Backend } from '../backends/types.ts';
import { attachConnections } from '../connect.ts';
import { applyFilters, mergeBoards, normaliseLine } from '../filter.ts';
import { applyVia, matchRows } from '../via.ts';
import type { Board, BoardConfig, ConnectionConfig, Departure, Message } from '../model.ts';
import type { BoardStatus, ExportedBoard, ExportedConfig, ExportedProfile } from './types.ts';

// Everything the page fetches, and the rules about where it fetches it from.

/**
 * How far ahead the realtime source is worth asking for.
 *
 * Its page limit is a hundred rows applied before the category filter, so a
 * busy interchange returns about forty minutes per request. Asking it for
 * twelve or twenty-four hours would mean dozens of round trips for data that is
 * pure timetable that far out anyway: nothing is delayed by a known amount ten
 * hours in advance. Inside this window the answer is live and worth the pages;
 * beyond it the timetable source answers the whole remainder in one request.
 */
export const REALTIME_HORIZON_MINUTES = 180;

/**
 * How long a timetable answer stays good. It is a published schedule rather
 * than a measurement, so it does not change between two glances at a phone, and
 * re-fetching it on every thirty-second refresh would spend most of the page's
 * request budget on data that did not move.
 */
const TIMETABLE_CACHE_MS = 10 * 60_000;

interface CacheEntry {
  at: number;
  rows: Departure[];
}

const timetableCache = new Map<string, CacheEntry>();

/**
 * How many upstream requests of each kind may be in the air at once.
 *
 * Boards are fetched together rather than one after another, which is the whole
 * speed-up, and without a cap "together" means every stop of every board of
 * every prefetched profile at the same instant. That is how a public
 * aggregator's rate limit gets tripped, and a rate-limited board looks to a
 * reader exactly like a broken one.
 */
const LIVE_CONCURRENCY = 4;
const TIMETABLE_CONCURRENCY = 4;

/** How many stop requests this run answered from one already in flight. */
let sharedHits = 0;

/** Read and reset the shared-request count, for the timing line. */
export function takeSharedHits(): number {
  const count = sharedHits;
  sharedHits = 0;
  return count;
}

/**
 * A private copy of a shared answer.
 *
 * Two boards that share a stop share one request, and then each one filters,
 * tags and annotates the rows it got. Those are writes: a stop tag, an onward
 * connection. Handing both boards the same objects would make the second board's
 * tags appear on the first, which is the kind of bug that only shows up on the
 * one profile that happens to list a stop twice.
 */
function copies(rows: Departure[]): Departure[] {
  return rows.map((row) => ({ ...row }));
}

export interface Backends {
  /** Realtime first, timetable as a fallback, for the near window. */
  live: Backend;
  /** Timetable only, for everything past the realtime horizon. */
  timetable: Backend;
  outcomes: ReadonlyMap<string, { backend: string }>;
}

export function makeBackends(config: ExportedConfig, onProgress?: (backend: string, stop: string, page: number) => void): Backends {
  const shared = {
    transportTypes: config.defaults.transport_types,
    ...(onProgress === undefined
      ? {}
      : { onProgress: (event: { backend: string; stop: string; page: number }) => onProgress(event.backend, event.stop, event.page) }),
  };
  const mvg = createMvgBackend({ ...shared, baseUrl: config.backends.mvg_base_url || MVG_DEFAULT_BASE_URL });
  const transitous = createTransitousBackend({
    ...shared,
    baseUrl: config.backends.transitous_base_url || TRANSITOUS_DEFAULT_BASE_URL,
  });
  const chained = chain(mvg, transitous);
  return { live: chained, timetable: transitous, outcomes: chained.outcomes };
}

export function toBoardConfig(board: ExportedBoard): BoardConfig {
  const config: BoardConfig = { title: board.title, stops: board.stops, walkMinutes: board.walk_minutes };
  if (board.modes !== null) config.modes = board.modes;
  if (board.lines !== null) config.lines = board.lines;
  if (board.direction !== null) config.direction = board.direction;
  if (board.via !== null && board.via !== undefined) config.via = board.via;
  if (board.destinations !== null) config.destinations = board.destinations;
  if (board.walk_minutes_by_stop !== null) config.walkMinutesByStop = board.walk_minutes_by_stop;
  if (board.stop_labels !== null) config.stopLabels = board.stop_labels;
  if (board.connection !== null) {
    const connection: ConnectionConfig = {
      stop: board.connection.stop,
      lines: board.connection.lines,
      rideMinutes: board.connection.ride_minutes,
      transferMinutes: board.connection.transfer_minutes,
    };
    if (board.connection.direction !== null) connection.direction = board.connection.direction;
    config.connection = connection;
  }
  return config;
}

/** The same rule as the CLI's: the configured label, else the id's last field. */
export function stopTagOf(stop: string, labels?: Record<string, string>): string {
  const label = labels?.[stop];
  if (label !== undefined && label.length > 0) return label;
  const fields = stop.split(':');
  const last = fields[fields.length - 1];
  return last !== undefined && last.length > 0 ? last : stop;
}

/**
 * The identity of one scheduled vehicle, for stitching two sources together.
 *
 * The seam between the live window and the timetable window is where the same
 * departure can arrive twice, once from each source, and the two copies will
 * not be byte-identical: one carries a delay and the other does not. Matching
 * on the *planned* minute is what makes them the same row, since that is the
 * one field a timetable and a live feed must agree on.
 */
function seamKey(dep: Departure): string {
  return [normaliseLine(dep.line), Math.floor(dep.planned / 60_000), dep.direction ?? '', dep.stop].join('|');
}

/**
 * The platform identifiers the primary backend has named at each stop, most
 * used first.
 *
 * The aggregator does not carry every parent stop. Where it does not, the only
 * way to reach that station's rows is to ask its platforms by name, and the
 * only place those names exist is on the rows the primary backend already
 * returned. So every row that carries one is recorded here as it goes past, and
 * every later request for that stop hands the list on as a hint. It is a
 * process-wide memo rather than per fetch because it is a fact about the
 * station rather than about this minute.
 */
const platformsSeen = new Map<string, Map<string, number>>();

function notePlatforms(rows: readonly Departure[]): void {
  for (const row of rows) {
    const id = row.stopPoint;
    if (id === undefined || id.length === 0) continue;
    const counts = platformsSeen.get(row.stop) ?? new Map<string, number>();
    counts.set(id, (counts.get(id) ?? 0) + 1);
    platformsSeen.set(row.stop, counts);
  }
}

/** The platforms known at a stop, most used first. */
export function platformHints(stop: string): string[] {
  const counts = platformsSeen.get(stop);
  if (counts === undefined) return [];
  return [...counts.entries()].sort((a, b) => b[1] - a[1]).map(([id]) => id);
}

// The ordering rule itself is `platformIdsOf` in origin.ts; this memo only
// accumulates across fetches, which a single list of rows cannot do.

/** Forget the platform memo, for a test that wants a fresh page. */
export function clearPlatformHints(): void {
  platformsSeen.clear();
}

/**
 * Fill in a platform the row's own feed did not publish.
 *
 * The primary feed leaves `platform` empty for some services at some stations,
 * rapid transit most often, and the aggregator's row for the very same run at
 * the very same stop carries the track. One request per stop, cached and shared
 * with every other use of that stop's aggregator rows, and only made when there
 * is actually a row missing a platform.
 *
 * Written to `platformGuess` rather than to `platform`; see the field.
 */
async function borrowPlatforms(
  rows: Departure[],
  stop: string,
  backend: Backend,
  window: { fromMs: number; toMs: number },
): Promise<void> {
  const missing = rows.filter((row) => row.stop === stop && row.platform === null && row.platformGuess === undefined);
  if (missing.length === 0) return;
  let aggregator: Departure[];
  try {
    aggregator = await timetableRows(backend, stop, window, undefined);
  } catch {
    // A board without platform badges is a board; a board that failed to draw
    // because a second feed was down is not.
    return;
  }
  if (aggregator.length === 0) return;
  for (const row of missing) {
    for (const candidate of matchRows(row, aggregator)) {
      if (candidate.platform === null) continue;
      row.platformGuess = candidate.platform;
      break;
    }
  }
}

async function timetableRows(
  backend: Backend,
  stop: string,
  window: { fromMs: number; toMs: number },
  modes: BoardConfig['modes'],
): Promise<Departure[]> {
  const hints = platformHints(stop);
  // The hints are part of the key: a request made before this stop's platforms
  // were known asked a different question from one made after, and the first
  // answer must not be handed back for the second.
  const key = [stop, Math.floor(window.fromMs / 60_000), Math.floor(window.toMs / 60_000), (modes ?? []).join(','), hints.join(',')].join('|');
  const hit = timetableCache.get(key);
  const now = Date.now();
  if (hit !== undefined && now - hit.at < TIMETABLE_CACHE_MS) return hit.rows;
  const rows = await share(`timetable|${key}`, () =>
    withLimit('timetable', TIMETABLE_CONCURRENCY, () =>
      backend.departures(stop, window, {
        ...(modes === undefined ? {} : { transportTypes: modes }),
        ...(hints.length === 0 ? {} : { platformIds: hints }),
      }),
    ),
  );
  notePlatforms(rows);
  timetableCache.set(key, { at: now, rows });
  return rows;
}

/**
 * One stop's departures across the whole requested window, from one source or
 * two. The live source answers the near part and the timetable source answers
 * the rest; the two are stitched with the live copy winning, because where they
 * disagree the live one is the one that knows about today.
 */
async function stopDepartures(
  backends: Backends,
  stop: string,
  window: { fromMs: number; toMs: number },
  board: BoardConfig,
): Promise<Departure[]> {
  const modes = board.modes;
  const call = modes === undefined ? undefined : { transportTypes: modes };
  const liveEnd = Math.min(window.toMs, window.fromMs + REALTIME_HORIZON_MINUTES * 60_000);
  // Keyed by what is being asked, not by who is asking. Two boards at the same
  // stop over the same window want the same rows; the filters that make them
  // different boards are applied afterwards, here on the page.
  const liveKey = ['live', stop, Math.floor(window.fromMs / 60_000), Math.floor(liveEnd / 60_000), (modes ?? []).join(',')].join('|');
  const live = copies(
    await share(
      liveKey,
      () => withLimit('live', LIVE_CONCURRENCY, () => backends.live.departures(stop, { fromMs: window.fromMs, toMs: liveEnd }, call)),
      () => {
        sharedHits += 1;
      },
    ),
  );
  notePlatforms(live);
  if (window.toMs <= liveEnd) return live;

  let later: Departure[] = [];
  try {
    later = copies(await timetableRows(backends.timetable, stop, { fromMs: liveEnd, toMs: window.toMs }, modes));
  } catch {
    // A long horizon that cannot be filled is still a usable board: the near
    // window is the part anyone acts on.
    return live;
  }
  const seen = new Set(live.map(seamKey));
  const merged = [...live];
  for (const row of later) if (!seen.has(seamKey(row))) merged.push(row);
  merged.sort((a, b) => a.realtime - b.realtime);
  return merged;
}

/**
 * Wrap a status sink so that a board which has answered cannot go back to
 * loading.
 *
 * A status is not a counter. There is no later event that takes a "loading"
 * back off the screen: the only thing that replaces it is the board's own
 * answer, and the board has already given that. So one progress event arriving
 * after a board is done leaves that board reporting itself as loading for the
 * rest of the visit, under a page number from somebody else's request. That is
 * not a hypothesis. The first board on a reader's phone sat at "page 1" under a
 * full list of departures until the page was closed.
 */
export function settling(onStatus: (boardIndex: number, status: BoardStatus) => void): (boardIndex: number, status: BoardStatus) => void {
  const settled = new Set<number>();
  return (index, status) => {
    if (status.kind === 'loading') {
      if (settled.has(index)) return;
    } else {
      settled.add(index);
    }
    onStatus(index, status);
  };
}

export interface FetchProfileOptions {
  config: ExportedConfig;
  profile: ExportedProfile;
  startMs: number;
  horizonMinutes: number;
  /** Reports each board's progress so the page can draw a skeleton with a page count. */
  onStatus: (boardIndex: number, status: BoardStatus) => void;
  /** Called when a sheet's onward calls land, so the open sheet can draw them. */
  onCallsLoaded?: () => void;
}

export interface FetchProfileResult {
  boards: Board[];
  /** Every backend that actually answered, for the bar's provenance line. */
  backends: string[];
}

export async function fetchProfile(options: FetchProfileOptions): Promise<FetchProfileResult> {
  const { config, profile, startMs, horizonMinutes, onStatus } = options;
  const window = { fromMs: startMs, toMs: startMs + horizonMinutes * 60_000 };
  // How far past the end of the window anything has to look; see
  // `BEYOND_HORIZON_EXTENSION_MINUTES`. Two things reach that far and the
  // board's own stops are not among them: what the reader is shown is the
  // window they picked, and rows past it were fetched for a while and read by
  // nothing, which is a doubled request count for an option nobody took up.
  const reachThroughMs = window.toMs + BEYOND_HORIZON_EXTENSION_MINUTES * 60_000;

  // Which board a stop belongs to, so progress can be reported while every board
  // is being fetched at once. A stop that two boards share reports to the first
  // of them, which is a cosmetic choice: the page number it draws is the same
  // number either way, because it is the same request.
  const boardOfStop = new Map<string, number>();
  for (let index = 0; index < profile.boards.length; index += 1) {
    const exported = profile.boards[index];
    for (const stop of exported?.stops ?? []) if (!boardOfStop.has(stop)) boardOfStop.set(stop, index);
    // The interchange a board asks about belongs to that board too. It used to
    // belong to no board, and an unrecognised stop was charged to board zero,
    // which is how the first board on the screen came to sit at "page 1" for
    // ever: a later board's onward lookup reported progress under its name long
    // after it had finished, and nothing said it was finished a second time.
    const via = exported?.connection?.stop;
    if (via !== undefined && !boardOfStop.has(via)) boardOfStop.set(via, index);
  }
  const report = settling(onStatus);
  const pages = new Map<number, number>();
  const backends = makeBackends(config, (backend, stop, page) => {
    const index = boardOfStop.get(stop);
    // A stop no board on this profile asked for. It cannot be attributed, and
    // attributing it to the first board is what the bug above was.
    if (index === undefined) return;
    pages.set(index, Math.max(pages.get(index) ?? 0, page));
    report(index, { kind: 'loading', backend, page });
  });

  // What the sheets ask when a reader wants to know where a train goes. Pointed
  // at this fetch's backends and window rather than set up once, because both
  // are built per fetch and the horizon is the reader's to change.
  configureCalls({
    ...(config.backends.transitous_base_url ? { baseUrl: config.backends.transitous_base_url } : {}),
    rows: (stop) => timetableRows(backends.timetable, stop, window, undefined),
    onLoaded: options.onCallsLoaded ?? ((): void => {}),
  });

  const used = new Set<string>();

  // Every board at once. They are independent questions and the reader is
  // waiting for the slowest one either way, so asking them in turn only added
  // the others' time to it. The concurrency cap lives one level down, around
  // the requests themselves, where it can count what is actually in the air.
  const built = await Promise.all(
    profile.boards.map(async (exported, index): Promise<Board | null> => {
    if (exported === undefined) return null;
    const boardConfig = toBoardConfig(exported);
    const boardStarted = Date.now();
    const sharedBefore = takeSharedHits();
    report(index, { kind: 'loading', backend: null, page: 0 });
    try {
      const multiStop = boardConfig.stops.length > 1;
      const perStop: Departure[][] = [];
      for (const stop of boardConfig.stops) {
        let rows = applyFilters(await stopDepartures(backends, stop, window, boardConfig), boardConfig);
        // After the cheap filters, never before. Every row this asks about costs
        // a request about that vehicle's own run, so the modes, the lines and the
        // letter get to throw away what they can first.
        if (boardConfig.via !== undefined) {
          rows = await applyVia(rows, {
            via: boardConfig.via,
            aggregatorRows: () => timetableRows(backends.timetable, stop, window, boardConfig.modes),
          });
        }
        if (multiStop) for (const row of rows) row.stopTag = stopTagOf(row.stop, boardConfig.stopLabels);
        await borrowPlatforms(rows, stop, backends.timetable, window);
        perStop.push(rows);
      }
      const departures = mergeBoards(perStop);

      if (boardConfig.connection !== undefined) {
        const connection = boardConfig.connection;
        // The onward window has to start where the last catchable change would:
        // a rider on the final row of this board still needs the ride and the
        // transfer added before anything at the interchange is useful. Its end
        // reaches past the window rather than stopping with it: the last row on
        // the board still needs a real onward departure to point at, and the one
        // it needs leaves after the window the reader picked has closed.
        const reach = (connection.rideMinutes + connection.transferMinutes) * 60_000;
        try {
          const onward = await stopDepartures(
            backends,
            connection.stop,
            { fromMs: window.fromMs + reach, toMs: reachThroughMs + reach },
            {
              title: '',
              stops: [connection.stop],
              walkMinutes: 0,
              ...(connection.direction === undefined ? {} : { direction: connection.direction }),
              lines: connection.lines,
            },
          );
          attachConnections(departures, onward, connection);
        } catch {
          // No onward data is a board with empty connection slots, not a failed
          // board: the departures themselves are what the reader came for.
          for (const row of departures) row.connection = null;
        }
      }

      const names = new Set<string>();
      for (const stop of boardConfig.stops) names.add(backends.outcomes.get(stop)?.backend ?? config.defaults.backend);
      for (const name of names) used.add(name);
      const board: Board = {
        title: boardConfig.title,
        stops: boardConfig.stops,
        backend: names.size === 1 ? ([...names][0] ?? config.defaults.backend) : 'mixed',
        departures,
        walkMinutes: boardConfig.walkMinutes,
      };
      board.planThroughMs = reachThroughMs;
      if (boardConfig.walkMinutesByStop !== undefined) board.walkMinutesByStop = boardConfig.walkMinutesByStop;
      if (boardConfig.stopLabels !== undefined) board.stopLabels = boardConfig.stopLabels;
      if (boardConfig.connection !== undefined) board.connection = boardConfig.connection;
      report(index, { kind: 'ready' });
      recordBoard(profile.key, {
        title: boardConfig.title,
        departuresMs: Date.now() - boardStarted,
        pages: pages.get(index) ?? 0,
        shared: sharedBefore + takeSharedHits(),
      });
      return board;
    } catch (error) {
      report(index, { kind: 'error', detail: error instanceof Error ? error.message : String(error) });
      recordBoard(profile.key, {
        title: boardConfig.title,
        departuresMs: Date.now() - boardStarted,
        pages: pages.get(index) ?? 0,
        shared: sharedBefore + takeSharedHits(),
      });
      return {
        title: boardConfig.title,
        stops: boardConfig.stops,
        backend: config.defaults.backend,
        departures: [],
        walkMinutes: boardConfig.walkMinutes,
      };
    }
    }),
  );

  return { boards: built.filter((board): board is Board => board !== null), backends: [...used].sort() };
}

export async function fetchMessages(config: ExportedConfig): Promise<Message[]> {
  const backends = makeBackends(config);
  return backends.live.messages();
}
