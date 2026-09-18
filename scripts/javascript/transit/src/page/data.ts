import { chain } from '../backends/chain.ts';
import { createMvgBackend, MVG_DEFAULT_BASE_URL } from '../backends/mvg.ts';
import { createTransitousBackend, TRANSITOUS_DEFAULT_BASE_URL } from '../backends/transitous.ts';
import type { Backend } from '../backends/types.ts';
import { attachConnections } from '../connect.ts';
import { applyFilters, mergeBoards, normaliseLine } from '../filter.ts';
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

async function timetableRows(
  backend: Backend,
  stop: string,
  window: { fromMs: number; toMs: number },
  modes: BoardConfig['modes'],
): Promise<Departure[]> {
  const key = [stop, Math.floor(window.fromMs / 60_000), Math.floor(window.toMs / 60_000), (modes ?? []).join(',')].join('|');
  const hit = timetableCache.get(key);
  const now = Date.now();
  if (hit !== undefined && now - hit.at < TIMETABLE_CACHE_MS) return hit.rows;
  const rows = await backend.departures(stop, window, modes === undefined ? undefined : { transportTypes: modes });
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
  const live = await backends.live.departures(stop, { fromMs: window.fromMs, toMs: liveEnd }, call);
  if (window.toMs <= liveEnd) return live;

  let later: Departure[] = [];
  try {
    later = await timetableRows(backends.timetable, stop, { fromMs: liveEnd, toMs: window.toMs }, modes);
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

export interface FetchProfileOptions {
  config: ExportedConfig;
  profile: ExportedProfile;
  startMs: number;
  horizonMinutes: number;
  /** Reports each board's progress so the page can draw a skeleton with a page count. */
  onStatus: (boardIndex: number, status: BoardStatus) => void;
}

export interface FetchProfileResult {
  boards: Board[];
  /** Every backend that actually answered, for the bar's provenance line. */
  backends: string[];
}

export async function fetchProfile(options: FetchProfileOptions): Promise<FetchProfileResult> {
  const { config, profile, startMs, horizonMinutes, onStatus } = options;
  const window = { fromMs: startMs, toMs: startMs + horizonMinutes * 60_000 };

  let currentBoard = 0;
  const backends = makeBackends(config, (backend, _stop, page) => {
    onStatus(currentBoard, { kind: 'loading', backend, page });
  });

  const built: Board[] = [];
  const used = new Set<string>();

  for (let index = 0; index < profile.boards.length; index += 1) {
    currentBoard = index;
    const exported = profile.boards[index];
    if (exported === undefined) continue;
    const boardConfig = toBoardConfig(exported);
    onStatus(index, { kind: 'loading', backend: null, page: 0 });
    try {
      const multiStop = boardConfig.stops.length > 1;
      const perStop: Departure[][] = [];
      for (const stop of boardConfig.stops) {
        const rows = applyFilters(await stopDepartures(backends, stop, window, boardConfig), boardConfig);
        if (multiStop) for (const row of rows) row.stopTag = stopTagOf(row.stop, boardConfig.stopLabels);
        perStop.push(rows);
      }
      const departures = mergeBoards(perStop);

      if (boardConfig.connection !== undefined) {
        const connection = boardConfig.connection;
        // The onward window has to start where the last catchable change would:
        // a rider on the final row of this board still needs the ride and the
        // transfer added before anything at the interchange is useful.
        const reach = (connection.rideMinutes + connection.transferMinutes) * 60_000;
        try {
          const onward = await stopDepartures(backends, connection.stop, { fromMs: window.fromMs + reach, toMs: window.toMs + reach }, {
            title: '',
            stops: [connection.stop],
            walkMinutes: 0,
            ...(connection.direction === undefined ? {} : { direction: connection.direction }),
            lines: connection.lines,
          });
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
      if (boardConfig.walkMinutesByStop !== undefined) board.walkMinutesByStop = boardConfig.walkMinutesByStop;
      if (boardConfig.stopLabels !== undefined) board.stopLabels = boardConfig.stopLabels;
      if (boardConfig.connection !== undefined) board.connection = boardConfig.connection;
      built.push(board);
      onStatus(index, { kind: 'ready' });
    } catch (error) {
      built.push({
        title: boardConfig.title,
        stops: boardConfig.stops,
        backend: config.defaults.backend,
        departures: [],
        walkMinutes: boardConfig.walkMinutes,
      });
      onStatus(index, { kind: 'error', detail: error instanceof Error ? error.message : String(error) });
    }
  }

  return { boards: built, backends: [...used].sort() };
}

export async function fetchMessages(config: ExportedConfig): Promise<Message[]> {
  const backends = makeBackends(config);
  return backends.live.messages();
}
