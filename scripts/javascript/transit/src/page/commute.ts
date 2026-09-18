import { normaliseLine } from '../filter.ts';
import type { Board, Departure } from '../model.ts';
import { planBoard, type PlannedRow, type PlanDestination, type RouteOption } from '../plan.ts';
import type { OriginCache, OriginLevel } from '../origin.ts';
import { idbGet, idbSet, STORE_ORIGINS } from './idb.ts';
import type { ExportedConfig, ExportedProfile } from './types.ts';

// The commute view: for a board that opts in, which of its departures actually
// gets you there soonest.
//
// Plans are held in memory and deliberately never written to the offline cache,
// unlike the departures. A stale departure is obvious, because its countdown is
// wrong and the row says how old the data is. A stale arrival time is not: it
// looks exactly like a fresh one and it is the number a reader acts on. So the
// commute view simply has nothing to show until it has asked.

/** One board's plans, plus how its stop had to be named to get them. */
export interface BoardRoutes {
  rows: Map<string, PlannedRow>;
  origin: OriginLevel | null;
}

/** Plans for one profile, by board index. */
export type ProfileRoutes = Map<number, BoardRoutes>;

/**
 * The resolved stop identifiers, kept in IndexedDB.
 *
 * Unlike the plans, this is safe to persist and worth persisting: it is a fact
 * about which identifiers the aggregator carries, not a time that goes stale,
 * and without it every cold start pays a probe per planned board before it can
 * plan anything.
 */
const originCache: OriginCache = {
  get: (key) => idbGet<string[]>(STORE_ORIGINS, key),
  set: (key, value) => idbSet(STORE_ORIGINS, key, value),
};

/**
 * The identity of one departure across a re-fetch.
 *
 * The planned time rather than the expected one, because the expected time
 * moves while the vehicle does not, and a plan looked up by a moving key would
 * come back empty every time the delay changed.
 */
export function rowKey(dep: Departure): string {
  return `${normaliseLine(dep.line)}|${dep.planned}|${dep.stop}`;
}

/** The destination a profile key names, as the planner wants it. */
export function destinationOf(config: ExportedConfig, key: string | null): PlanDestination | null {
  if (key === null) return null;
  const place = (config.places ?? []).find((entry) => entry.name === key);
  return place === undefined ? null : { lat: place.lat, lon: place.lon };
}

/**
 * Where a line usually puts you down on the way to this destination.
 *
 * Worth computing because the useful signal is not "get off at X", which is the
 * same every day, but "this one is better off at Y". The usual exit is the most
 * common one across the board's own rows for that line, so it needs no
 * configuration and adapts when the timetable does.
 */
export function usualExits(planned: Map<string, PlannedRow>): Map<string, string> {
  const counts = new Map<string, Map<string, number>>();
  for (const row of planned.values()) {
    const option = row.best;
    if (option === null) continue;
    const line = normaliseLine(row.departure.line);
    const perLine = counts.get(line) ?? new Map<string, number>();
    perLine.set(option.exitStop, (perLine.get(option.exitStop) ?? 0) + 1);
    counts.set(line, perLine);
  }
  const usual = new Map<string, string>();
  for (const [line, perLine] of counts) {
    let bestStop = '';
    let bestCount = -1;
    for (const [stop, count] of perLine) {
      if (count > bestCount) {
        bestStop = stop;
        bestCount = count;
      }
    }
    if (bestStop !== '') usual.set(line, bestStop);
  }
  return usual;
}

/** The arrival a row is sorted by, with rows that have no plan kept at the end. */
export function arrivalOf(dep: Departure, planned: Map<string, PlannedRow> | undefined): number {
  const option: RouteOption | null | undefined = planned?.get(rowKey(dep))?.best;
  return option === undefined || option === null ? Number.POSITIVE_INFINITY : option.arrival;
}

export interface PlanProfileOptions {
  config: ExportedConfig;
  profile: ExportedProfile;
  boards: Board[];
  destinationKey: string | null;
  startMs: number;
  earlyBufferMinutes?: number;
}

/**
 * Plan every board of a profile that opted in. One request sequence per board,
 * which is why the opt-in exists: a profile of five boards would otherwise put
 * five journey plans on a free service every thirty seconds, for boards that
 * nobody plans a journey from.
 */
export async function planProfile(options: PlanProfileOptions): Promise<ProfileRoutes> {
  const destination = destinationOf(options.config, options.destinationKey);
  const routes: ProfileRoutes = new Map();
  if (destination === null) return routes;

  for (let index = 0; index < options.boards.length; index += 1) {
    const board = options.boards[index];
    const exported = options.profile.boards[index];
    if (board === undefined || exported === undefined || !exported.commute) continue;
    const stop = board.stops[0];
    if (stop === undefined) continue;
    try {
      let origin: OriginLevel | null = null;
      const planned = await planBoard({
        stop,
        destination,
        rows: board.departures,
        startMs: options.startMs,
        baseUrl: options.config.backends.transitous_base_url,
        originCache,
        onOrigin: (resolved) => {
          origin = resolved.level;
        },
        ...(options.earlyBufferMinutes === undefined ? {} : { earlyBufferMinutes: options.earlyBufferMinutes }),
      });
      const byKey = new Map<string, PlannedRow>();
      for (const row of planned) byKey.set(rowKey(row.departure), row);
      routes.set(index, { rows: byKey, origin });
    } catch {
      // A board with no plan is a board without the commute slot, not a broken
      // board. The departures are still correct and still the main thing.
    }
  }
  return routes;
}
