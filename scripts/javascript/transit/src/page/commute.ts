import { normaliseLine } from '../filter.ts';
import type { Board, Departure } from '../model.ts';
import { planBoard, type PlannedRow, type PlanTarget, type RouteOption } from '../plan.ts';
import { destinationLabel, planTargets, type DestinationPlace } from '../targets.ts';
import type { OriginCache, OriginLevel } from '../origin.ts';
import { idbGet, idbSet, STORE_ORIGINS, STORE_ROUTES } from './idb.ts';
import type { ExportedConfig, ExportedPlace, ExportedProfile } from './types.ts';

// The commute view: for a board that opts in, which of its departures actually
// gets you there soonest.
//
// Plans are held in memory and mirrored to IndexedDB, and the mirror is the
// delicate part. A stale departure is obvious, because its countdown is wrong
// and the row says how old the data is; a stale arrival time is not, because it
// looks exactly like a fresh one and it is the number a reader acts on. So a
// restored plan is never presented as current: it is dimmed and labelled with
// its age until a fresh one lands, which is the whole of `ProfileRoutes.stale`.
// Without the mirror a cold start shows every row with an empty journey slot
// for a second or two, which reads as "no journey" rather than "not yet asked".

/** One board's plans, plus how its stop had to be named to get them. */
export interface BoardRoutes {
  rows: Map<string, PlannedRow>;
  origin: OriginLevel | null;
  /** The place these journeys end at, which a board may fix for itself. */
  destinationKey?: string;
  /**
   * When each row's journey was worked out, by row key.
   *
   * Per row rather than per board because a run does not answer for every row
   * every time. A journey search is a search: the aggregator returns what it
   * found in the window it looked at, and a row at the edge of that window
   * drops out of one answer and comes back in the next. Measured on the real
   * page, one row in fifty lost its journey for forty-one seconds that way.
   * Carrying the previous answer forward is what stops that being a blank slot,
   * and dating it per row is what lets the view say which ones are getting old.
   */
  plannedAt?: Map<string, number>;
}

/** How old a carried-over journey may be before it is shown as ageing. */
export const ROUTE_AGEING_MS = 3 * 60_000;

/** Plans for one profile: per board, plus how old and how trustworthy they are. */
export interface ProfileRoutes {
  boards: Map<number, BoardRoutes>;
  /** When these journeys were planned, epoch milliseconds. */
  at: number;
  /** The place they were planned towards, so a changed picker invalidates them. */
  destinationKey: string;
  /** True while they came from the cache and nothing fresh has landed yet. */
  stale: boolean;
  /** What question these answer; an unchanged one is never asked twice. */
  key: string;
}

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

/**
 * How far a plan for this board must search, past what it renders.
 *
 * The planner's own rule is to search as far as the last row it was handed,
 * which is precisely the wrong answer for the row at the edge of the horizon:
 * the change it needs to make leaves after it does, so a search that stops
 * where it leaves finds nothing and the last minutes of every horizon come back
 * blank. The board carries the further point it has to reach.
 */
function coverThroughOf(board: Board): number | undefined {
  return board.planThroughMs;
}

/** The place a profile key names, or null when the configuration has none. */
export function placeOf(config: ExportedConfig, key: string | null): ExportedPlace | null {
  if (key === null) return null;
  return (config.places ?? []).find((entry) => entry.name === key) ?? null;
}

/** What the picker and the journey slots call a destination. */
export function destinationNameOf(config: ExportedConfig, key: string | null): string {
  const place = placeOf(config, key);
  return place === null ? (key ?? '') : destinationLabel(place as DestinationPlace);
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
  /** Cancels a run whose question has been replaced; see `replanProfile`. */
  signal?: AbortSignal;
  profileKey: string;
  profile: ExportedProfile;
  boards: Board[];
  destinationKey: string | null;
  startMs: number;
  /**
   * How far ahead the boards were asked about.
   *
   * Nothing here reads it: a search covers the rows it was handed, and the
   * board carries how far past them it has to reach. It is part of the
   * question when the planning server is the one answering, because there one
   * request answers for the boards and the journeys together.
   */
  horizonMinutes?: number;
  earlyBufferMinutes?: number;
  walkWeight?: number;
  /** The plans already on screen, so an unchanged question is not asked again. */
  previous?: ProfileRoutes | undefined;
  /** Told how many of this profile's planned boards have answered so far. */
  onProgress?: (done: number, total: number) => void;
}

/**
 * What makes one planning run different from another.
 *
 * A page that refreshes twice a minute must not re-plan twice a minute, and the
 * planner's own cache only stops the requests, not the recombination. Everything
 * that can change an answer is in this key and nothing else is: the row count
 * is there because a row scrolling into the horizon deserves a plan, and the
 * delays are deliberately absent because a journey is looked up by its
 * timetabled minute and a delay does not move it.
 */
export function planKey(options: PlanProfileOptions): string {
  const counts = options.boards.map((board) => board.departures.length).join(',');
  const fixed = options.profile.boards.map((board) => board.destination ?? '').join(',');
  return [
    options.destinationKey ?? '',
    fixed,
    Math.floor(options.startMs / 60_000),
    options.earlyBufferMinutes ?? '',
    options.walkWeight ?? '',
    counts,
  ].join('|');
}

/** Where one profile's plans live between visits. */
function routesKey(profileKey: string): string {
  return `routes:${profileKey}`;
}

/**
 * The plans from the last visit, for the moment before the fresh ones land.
 * Marked stale, which is what makes the page dim them and say how old they are.
 */
export async function cachedRoutes(profileKey: string, destinationKey: string | null): Promise<ProfileRoutes | null> {
  const stored = await idbGet<ProfileRoutes>(STORE_ROUTES, routesKey(profileKey));
  if (stored === null || !(stored.boards instanceof Map)) return null;
  // A profile whose boards each fix their own destination has no picker and so
  // no key here; what it stored is still the answer to the same question,
  // because that question is in the configuration rather than on screen.
  if (stored.destinationKey !== (destinationKey ?? '')) return null;
  return { ...stored, stale: true };
}

/**
 * The new answer for one board, with the previous one carried underneath it.
 *
 * Everything the new run found wins. Everything it did not answer for keeps
 * what it had, with the date it had, so a row whose journey dropped out of one
 * search keeps showing the last journey anybody actually found for it rather
 * than an empty slot. Nothing is carried across a change of destination: those
 * are answers to a different question.
 */
export function mergeBoardRoutes(previous: BoardRoutes | undefined, fresh: BoardRoutes, at: number): BoardRoutes {
  const plannedAt = new Map<string, number>();
  const rows = new Map<string, PlannedRow>();
  if (previous !== undefined && (previous.destinationKey ?? '') === (fresh.destinationKey ?? '')) {
    for (const [key, row] of previous.rows) {
      rows.set(key, row);
      plannedAt.set(key, previous.plannedAt?.get(key) ?? firstPlannedAt(previous, at));
    }
  }
  for (const [key, row] of fresh.rows) {
    const carried = rows.get(key);
    // A search that offers nothing for a row it answered for a minute ago is a
    // search that looked and missed, not news that the journey has gone: the
    // aggregator answers for what it found in the window it looked at. So the
    // older answer stays, wearing its age, and only a fresh answer that
    // actually found something replaces it. Everything else about the row, the
    // departure itself included, comes from the fresh run.
    if (carried !== undefined && row.best === null && carried.best !== null) {
      rows.set(key, { ...carried, departure: row.departure });
      continue;
    }
    rows.set(key, row);
    plannedAt.set(key, at);
  }
  return { rows, origin: fresh.origin, plannedAt, ...(fresh.destinationKey === undefined ? {} : { destinationKey: fresh.destinationKey }) };
}

/** One board's share of a planning run: what came back, or that nothing did. */
export interface BoardResult {
  index: number;
  routes: BoardRoutes | null;
}

/**
 * The boards of a whole run, merged onto the run before it.
 *
 * Whatever was on screen is the floor, not the thing being replaced. A board
 * this run could not answer for keeps what it had; a board it did answer for
 * keeps the rows the answer left out. What is not carried is a board that is
 * now heading somewhere else: those journeys answer another question, and
 * showing them under a new destination would be a lie rather than a stale
 * truth. `wanted` is every board the run set out to plan, with where it was
 * planning to, which is what makes that judgement possible for a board the run
 * never got an answer for.
 *
 * Exported because a run does not always happen here. When the page is being
 * answered by the planning server, the searching happens there and the
 * carrying still has to happen on the phone, against the phone's own previous
 * answer, and two implementations of this rule would drift apart within a week.
 */
export function carryBoards(
  previous: ProfileRoutes | undefined,
  wanted: ReadonlyMap<number, string>,
  results: Iterable<BoardResult>,
  at: number,
): Map<number, BoardRoutes> {
  const boards = new Map<number, BoardRoutes>();
  if (previous !== undefined) {
    for (const [index, board] of previous.boards) {
      if (wanted.get(index) !== (board.destinationKey ?? '')) continue;
      boards.set(index, board);
    }
  }
  for (const result of results) {
    if (result.routes === null) continue;
    boards.set(result.index, mergeBoardRoutes(previous?.boards.get(result.index), result.routes, at));
  }
  return boards;
}

/** When a previous board's rows were planned, for entries that predate the dating. */
function firstPlannedAt(previous: BoardRoutes, fallback: number): number {
  for (const value of previous.plannedAt?.values() ?? []) return value;
  return fallback;
}

/** One board this run will search for, and the question it will ask about it. */
export interface PlanJob {
  index: number;
  board: Board;
  /** The identifier the planner is given, which is not always the departures' one. */
  stop: string;
  destinationKey: string;
  targets: readonly PlanTarget[];
}

/**
 * Which boards of a profile get a journey search, and towards what.
 *
 * A board may name its own destination, and then the picker does not apply to
 * it: a departure hall is laid out by where a platform goes, so "which way am I
 * heading" is a fact about the board rather than a question for the reader.
 *
 * Exported because the planning server has to answer the same question without
 * running the search: what it sends back has to say which boards it set out to
 * plan, so that the page can carry its own previous answers forward for the
 * ones that came back empty.
 */
export function planJobs(
  options: Pick<PlanProfileOptions, 'config' | 'profile' | 'boards' | 'destinationKey'>,
): PlanJob[] {
  /**
   * The places to ask about for one destination key, worked out once per key.
   *
   * A coordinate place is also a profile's doorstep, so its own boards' stops
   * become targets too; see `planTargets` for why that is not redundant.
   */
  const targetCache = new Map<string, readonly PlanTarget[]>();
  const targetsFor = (destinationKey: string): readonly PlanTarget[] => {
    const known = targetCache.get(destinationKey);
    if (known !== undefined) return known;
    const place = placeOf(options.config, destinationKey);
    const profile = options.config.profiles.find((entry) => entry.key === destinationKey);
    const targets =
      place === null
        ? []
        : planTargets(place as DestinationPlace, (profile?.boards ?? []).map((board) => ({
            stops: board.stops,
            walkMinutes: board.walk_minutes,
            walkMinutesByStop: board.walk_minutes_by_stop,
          })));
    targetCache.set(destinationKey, targets);
    return targets;
  };

  const jobs: PlanJob[] = [];
  for (let index = 0; index < options.boards.length; index += 1) {
    const board = options.boards[index];
    const exported = options.profile.boards[index];
    if (board === undefined || exported === undefined || !exported.commute) continue;
    const destinationKey = exported.destination ?? options.destinationKey;
    if (destinationKey === null) continue;
    const targets = targetsFor(destinationKey);
    if (targets.length === 0) continue;
    const stop = exported.plan_stop ?? board.stops[0];
    if (stop === undefined) continue;
    jobs.push({ index, board, stop, destinationKey, targets });
  }
  return jobs;
}

/**
 * Plan every board of a profile that opted in, all at once.
 *
 * In parallel rather than one after another: the boards are independent
 * questions to the same service, and planning them in sequence made the journey
 * slots of the last board on a profile arrive a second and a half after the
 * first board's. The opt-in still exists, because a plan is a much heavier
 * question than a departure board and most boards are "is there a bus soon".
 */
export async function planProfile(options: PlanProfileOptions): Promise<ProfileRoutes | null> {
  const previous = options.previous;
  const key = planKey(options);
  // Asked and answered within this same minute: hand back what is on screen.
  if (previous !== undefined && !previous.stale && previous.key === key) return previous;

  const jobs = planJobs(options);
  if (jobs.length === 0) return null;

  let done = 0;
  options.onProgress?.(0, jobs.length);
  const planModes = options.config.defaults.plan_modes;
  // The run's own cancellation, folded into whatever timeout the request layer
  // already set. Replacing the signal outright would throw that timeout away,
  // which is the kind of quiet loss that only shows up as a page that hangs.
  const signal = options.signal;
  const fetchImpl =
    signal === undefined
      ? undefined
      : (url: string, init?: RequestInit): Promise<Response> => {
          const own = init?.signal ?? null;
          const merged =
            own === null ? signal : typeof AbortSignal.any === 'function' ? AbortSignal.any([own, signal]) : signal;
          return fetch(url, { ...init, signal: merged });
        };
  const results = await Promise.all(
    jobs.map(async ({ index, board, stop, destinationKey, targets }) => {
      try {
        let origin: OriginLevel | null = null;
        const coverThroughMs = coverThroughOf(board);
        const planned = await planBoard({
          stop,
          targets,
          rows: board.departures,
          startMs: options.startMs,
          baseUrl: options.config.backends.transitous_base_url,
          originCache,
          ...(coverThroughMs === undefined ? {} : { coverThroughMs }),
          ...(planModes === undefined ? {} : { planModes }),
          ...(options.walkWeight === undefined ? {} : { walkWeight: options.walkWeight }),
          onOrigin: (resolved) => {
            origin = resolved.level;
          },
          ...(options.earlyBufferMinutes === undefined ? {} : { earlyBufferMinutes: options.earlyBufferMinutes }),
          ...(fetchImpl === undefined ? {} : { fetchImpl }),
        });
        const byKey = new Map<string, PlannedRow>();
        for (const row of planned) byKey.set(rowKey(row.departure), row);
        return { index, routes: { rows: byKey, origin, destinationKey } as BoardRoutes };
      } catch {
        // A board with no plan is a board without the commute slot, not a broken
        // board. The departures are still correct and still the main thing, and
        // whatever journeys it had are kept: a search that failed says nothing
        // about whether the journeys it found a minute ago still exist.
        return { index, routes: null };
      } finally {
        done += 1;
        options.onProgress?.(done, jobs.length);
      }
    }),
  );

  const at = Date.now();
  const wanted = new Map(jobs.map((job) => [job.index, job.destinationKey]));
  const boards = carryBoards(previous, wanted, results, at);
  if (boards.size === 0) return null;

  const routes: ProfileRoutes = {
    boards,
    at,
    destinationKey: options.destinationKey ?? '',
    stale: false,
    key,
  };
  void idbSet(STORE_ROUTES, routesKey(options.profileKey), routes);
  return routes;
}
