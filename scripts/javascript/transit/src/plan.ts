import { resetInflight, share, withLimit } from './inflight.ts';
import {
  isCityBusAgency,
  MODE_MAP,
  normaliseLineName,
  toAggregatorId,
  toRawId,
  TRANSITOUS_DEFAULT_BASE_URL,
} from './backends/transitous.ts';
import { DEFAULT_PLAN_MODES, DEFAULT_WALK_WEIGHT } from './config.ts';
import { normaliseLine, type WalkSource } from './filter.ts';
import { resolveOrigin, type OriginCache, type OriginLevel, type ResolvedOrigin } from './origin.ts';
import { envOverride, fetchJson, HttpError, type FetchLike } from './http.ts';
import { departureJson, toIso } from './json.ts';
import { SCHEMA_VERSION, type Board, type Departure } from './model.ts';

// The commute view. A departure board says when vehicles leave; it does not say
// which of them puts you at your destination soonest, and at a stop served by
// four lines that is the question actually being asked. This module plans from
// a board's stop to one destination and hands back, per board row, the journeys
// that begin with that row.
//
// What the aggregator's `/plan` endpoint actually answers, measured against the
// live service rather than remembered:
//
//   GET /plan?fromPlace=<stop id or "lat,lon">&toPlace=<same>&time=<ISO>
//             &arriveBy=false&numItineraries=<n>&pageCursor=<cursor>
//
//   { itineraries: [...], direct: [...], from, to,
//     nextPageCursor: "LATER|<unix seconds>", previousPageCursor: "EARLIER|...",
//     requestParameters: {}, debugOutput: {...} }
//
// Each itinerary is `{ id, startTime, endTime, duration, transfers, legs }` with
// ISO instants, and each leg `{ mode, from, to, startTime, endTime, duration,
// realTime, scheduled, scheduledStartTime, scheduledEndTime, legGeometry }`,
// plus `distance` and `steps` on a street leg and `routeShortName`, `headsign`,
// `tripId`, `agencyName`, `routeColor`, `cancelled` and much else on a transit
// one. Five things about it were worth writing down:
//
//   - `numItineraries` is honoured and widens the search window rather than
//     merely truncating a fixed one, so one request usually covers a whole
//     board and the cursor walk below is the exception rather than the rule.
//   - A cursor page ignores that count and returns whatever its own window
//     found, which was three times the asked-for number in the case measured.
//   - `requestParameters` comes back empty and an unknown query parameter is
//     accepted silently, so a misspelled parameter fails by being ignored. Do
//     not read a 200 as confirmation that a parameter was understood.
//   - A leg's `duration` is computed from live times, so a delayed vehicle that
//     makes up time en route produces a transit leg of duration zero. Nothing
//     here reads `duration`; the instants are the truth.
//   - `parentId` on a leg's stop is present or absent for no reason a caller can
//     predict: the same shape of platform id carried one in some itineraries and
//     not in others, including for the very stop the query named. Stop identity
//     is therefore decided on the identifier itself, never on `parentId`.
//
// A bad stop id answers 404 with `{"error": "..."}`, which `fetchJson` turns
// into an `HttpError` like any other failure.

/**
 * How many cursor pages one plan may walk. Same reasoning as the departure
 * backends' own cap: a free service with no rate-limit signal gets a bounded
 * walk, not an unbounded one, and there is no back-pressure to react to if the
 * walk goes wrong. `numItineraries` already makes the first page wide, so this
 * is the safety net rather than the mechanism.
 */
export const PLAN_MAX_PAGES = 4;

/** Itineraries asked for per request. Widens the window; see the note above. */
export const ITINERARIES_PER_REQUEST = 25;

/**
 * Walking pace, metres per minute, and the single place this package converts a
 * distance into a time.
 *
 * Transfer walks are derived from the leg's `distance` at this pace rather than
 * taken from the planner's own walking-leg `duration`, because that duration is
 * not a walk. Measured over twenty street legs on the live service, edge walks
 * (doorstep to first stop, last stop to doorstep) come back at a consistent 64
 * to 73 metres per minute, but the walks *between* two transit legs do not: the
 * same service answered 185 metres in two minutes at one interchange and 148
 * metres in five at another. The difference is the minimum transfer time baked
 * into the timetable for that station, which is a property of the station and
 * not of the rider, and it is what makes a change look impossible when it is
 * merely inconvenient. Deriving from distance throws that padding away and puts
 * the slack somewhere a reader can see it, in the early buffer below.
 */
export const WALK_METRES_PER_MINUTE = 70;

/**
 * How far before the feasible change time an onward departure may leave and
 * still be offered, marked tight. Those are the changes that come off only if
 * the first vehicle runs a little early or the walk is quicker than the pace
 * above assumes. They are worth showing because a rider can decide to run for
 * one; they are never recommended.
 */
export const DEFAULT_EARLY_BUFFER_MINUTES = 3;

/**
 * How long a plan is reused. A page refreshing every thirty seconds must not
 * re-plan every time, and the answer does not move faster than this: the
 * itineraries are keyed to the minute the plan started from, and within one
 * minute nothing about them changes but the delays.
 */
export const PLAN_CACHE_MS = 60_000;

/**
 * How many options one row carries. Recombining exits with onward departures
 * produces far more journeys than anyone will read, and everything past the
 * first few arrives later than something already in the list.
 */
export const MAX_OPTIONS_PER_ROW = 4;

/** The mode a walking leg carries, so one field answers "is this a vehicle". */
export const WALK_MODE = 'WALK';

/**
 * One leg of a journey, timetabled or on foot. `from` and `to` are stop names,
 * for display.
 *
 * Walks are legs rather than an annotation on the vehicle legs around them
 * because they are the part of a journey a rider is choosing between: "off at
 * the far station and fourteen minutes on foot" and "off at the near one and
 * six" are two different offers, and a list that shows only the vehicles hides
 * the difference exactly where it is being decided. The final walk to the
 * destination is a leg for the same reason.
 */
export interface RouteLeg {
  kind: 'transit' | 'walk';
  /** Line label; empty on a walk. */
  line: string;
  /** Vehicle category, or `WALK_MODE`. */
  mode: string;
  from: string;
  to: string;
  departure: number;
  arrival: number;
}

export interface RouteOption {
  /** Where the first leg puts you down. */
  exitStop: string;
  exitStopName: string;
  legs: RouteLeg[];
  /** Arrival at the destination, epoch ms. */
  arrival: number;
  transfers: number;
  /** True when this only works if the first leg runs early or the change is quick. */
  tight: boolean;
  /**
   * How many minutes short the tight change is: what the first vehicle would
   * have to make up, or the walk would have to save. Zero when not tight. It is
   * the difference between "run for it" and "do not bother" and it costs one
   * subtraction to keep, so it is kept rather than recomputed from nothing.
   */
  tightBy: number;
  /**
   * Minutes on foot over the whole journey: every change plus the final walk.
   * Fractional, because the walks are derived from distances; round it for
   * display and leave the arithmetic alone.
   */
  walkMinutes: number;
  /** Which destination target this journey ends at, for display. */
  destinationName: string;
}

/** The transit legs of a journey, which is what a compact slot names. */
export function transitLegs(option: RouteOption): RouteLeg[] {
  return option.legs.filter((leg) => leg.kind === 'transit');
}

/**
 * How an option is ranked: its arrival, with every walked minute charged at
 * `walkWeight` ridden minutes.
 *
 * Arrival alone is the wrong objective and the failure is not subtle. A journey
 * that arrives two minutes sooner after a fourteen-minute walk beats one that
 * arrives two minutes later after six, and the planner will keep recommending
 * the first because by its own measure it wins. Charging a walked minute more
 * than a ridden one is the smallest change that makes the ranking agree with
 * what a rider would pick, and it is a preference rather than a fact, which is
 * why it is configurable and adjustable on the page.
 */
export function optionScore(option: RouteOption, walkWeight: number): number {
  return option.arrival + (walkWeight - 1) * option.walkMinutes * 60_000;
}

/**
 * One board row with the journeys that start with it. `best` is the soonest
 * arrival among the options that are not tight, and is null when every option
 * is tight, when the departure itself is cancelled, or when nothing was found
 * at all; `options` always holds the full list, tight ones included, sorted by
 * arrival.
 */
export interface PlannedRow {
  departure: Departure;
  best: RouteOption | null;
  options: RouteOption[];
  /**
   * Why this row has no journey, when it has none.
   *
   * Absent on a row that got one. It is a phrase rather than a code because its
   * only reader is a person holding a phone at a stop, and the question it
   * answers is "is this board broken or is this train simply not the way".
   */
  miss?: string;
}

/** Where a plan ends: a stop the aggregator knows, or a bare coordinate. */
export type PlanDestination = { lat: number; lon: number } | { id: string };

/**
 * One place a journey may end, and how long the walk from the last vehicle to
 * it takes.
 *
 * A plan is made to several of these at once, and that is the point rather than
 * a convenience. The planner answers with a Pareto set over arrival, changes
 * and departure, so an option that arrives later with a much shorter walk is
 * dominated and never returned: asked for a coordinate, it offered a fast train
 * to a far station and a fourteen-minute walk, and never mentioned the slower
 * train to the near station six minutes from the door, because that option
 * arrives later and no measure it optimises knows about the walk. Asking a
 * second time with the near station itself as the destination makes that option
 * exist, and only then can it be ranked.
 *
 * `walkMinutes` is what the configuration says the walk from that stop is; null
 * means take the planner's own final street leg, which is the honest answer for
 * a coordinate it routed to itself.
 */
export interface PlanTarget {
  place: PlanDestination;
  /** What this place is called, for the final walk leg and the slot. */
  name: string;
  walkMinutes: number | null;
}

export interface PlanBoardOptions {
  stop: string;
  /** Every place this board's riders may be heading for; see `PlanTarget`. */
  targets: readonly PlanTarget[];
  rows: Departure[];
  startMs: number;
  /**
   * How far out a journey search must reach, epoch milliseconds. Absent means
   * the old rule: the latest `realtime` among `rows`. A caller that fetched
   * further than it renders (a board's beyond-the-horizon rows, in particular)
   * passes that further point here, because otherwise a row near the end of
   * what is shown would be planned with a search window that stops right where
   * its own onward change would have to leave from, and never find it.
   */
  coverThroughMs?: number;
  earlyBufferMinutes?: number;
  /** What a walked minute costs in ridden minutes; defaults to `DEFAULT_WALK_WEIGHT`. */
  walkWeight?: number;
  /** Transit modes a journey may use; defaults to `DEFAULT_PLAN_MODES`. */
  planModes?: readonly string[];
  baseUrl?: string;
  fetchImpl?: FetchLike;
  onDebug?: (line: string) => void;
  /** Where the resolved origin is kept between runs, when the caller has one. */
  originCache?: OriginCache;
  /**
   * Told which step of the origin chain answered, so the caller can say so.
   * A plan made from a coordinate or a single platform is a slightly different
   * claim from one made from the stop itself, and the difference is worth
   * being able to see when a route looks wrong.
   */
  onOrigin?: (resolved: ResolvedOrigin) => void;
}

interface RawPlanPlace {
  name?: string;
  stopId?: string;
  track?: string;
  scheduledTrack?: string;
  arrival?: string;
  scheduledArrival?: string;
}

interface RawPlanLeg {
  mode?: string;
  from?: RawPlanPlace;
  to?: RawPlanPlace;
  startTime?: string;
  endTime?: string;
  scheduledStartTime?: string;
  distance?: number;
  tripId?: string;
  routeShortName?: string;
  headsign?: string;
  agencyName?: string;
  /** Stops this leg calls at between its own ends; null on a street leg. */
  intermediateStops?: RawPlanPlace[] | null;
}

interface RawItinerary {
  startTime?: string;
  endTime?: string;
  legs?: RawPlanLeg[];
}

interface RawPlan {
  itineraries?: RawItinerary[];
  nextPageCursor?: string;
}

/**
 * A transit leg as this module keeps it: the public shape plus the identifiers
 * the arithmetic needs and the street distance leading away from it, which is
 * how the walk to the next leg is recovered once the street legs are dropped.
 */
/** Somewhere a rider could get off a leg: the stop, its name, and when. */
interface ExitPoint {
  stop: string;
  name: string;
  arrival: number;
}

interface ParsedLeg extends RouteLeg {
  fromStop: string;
  toStop: string;
  toName: string;
  /**
   * The stops this leg calls at on the way, which are exits too.
   *
   * Without these a row can only ever be told to get off where the planner's
   * own itinerary for that exact departure alighted, and the planner returns a
   * Pareto set: "leave at 22:21 and change at the first interchange" is
   * dominated by "leave at 22:41 and change at the same interchange onto the
   * same onward train", so it is never returned, and the only itinerary left
   * beginning with the 22:21 vehicle was a much worse one changing far away.
   * The row then got that absurd advice. Every stop the vehicle calls at is a
   * candidate exit, and the tails observed from the later departure are what
   * the earlier row is actually looking for.
   */
  intermediate: ExitPoint[];
  /** Timetabled departure, which is the reliable half of the match key below. */
  scheduledDeparture: number;
  /** Platform this leg is boarded from, when the planner names one. */
  fromTrack: string | null;
  /**
   * Where this vehicle is going, as the planner's feed spells it.
   *
   * Kept because the platform cannot be trusted to identify a trip; see
   * `sameVehicle`.
   */
  headsign: string | null;
  walkMetresAfter: number;
}

/**
 * One itinerary reduced to its timetabled legs. Every itinerary the planner
 * returns ends at the destination, and that invariant is what lets the tail of
 * one itinerary be spliced onto the head of another below: any suffix of any
 * itinerary still ends where the rider is going.
 */
interface ParsedItinerary {
  legs: ParsedLeg[];
  /** Itinerary end, which includes the final walk to the destination. */
  arrival: number;
  /** Where this itinerary was planned to, and what that place is called. */
  destinationName: string;
  /** The last walk, from the final alighting stop to the destination. */
  finalWalkMs: number;
}

/** A suffix of some itinerary, indexed by where it is boarded. */
interface OnwardChain {
  boardStop: string;
  /** The name of the stop it is boarded at, for the walk leg leading into it. */
  boardName: string;
  departure: number;
  legs: RouteLeg[];
  arrival: number;
  /** Minutes on foot inside this tail, the final walk included. */
  walkMinutes: number;
  destinationName: string;
  transfers: number;
}

function parseIso(value: unknown): number {
  if (typeof value !== 'string') return Number.NaN;
  return Date.parse(value);
}

/**
 * Whether two identifiers name the same place: the same stop, or one of them a
 * platform of the other. The aggregator answers with platform-level ids while a
 * board is configured with the parent, so an equality test finds nothing. The
 * colon boundary is load-bearing, since without it a parent would also claim
 * every stop whose number merely begins with the parent's.
 */
export function sameStopArea(a: string, b: string): boolean {
  const left = toRawId(a);
  const right = toRawId(b);
  if (left.length === 0 || right.length === 0) return false;
  return left === right || left.startsWith(`${right}:`) || right.startsWith(`${left}:`);
}

/**
 * A leg is timetabled exactly when it carries a trip id. Listing the street
 * modes instead would be a guess: the mode vocabulary is open-ended, it already
 * carries categories this package never asks for, and a category nobody
 * anticipated would be silently treated as a walk.
 */
function isTransitLeg(leg: RawPlanLeg): boolean {
  return typeof leg.tripId === 'string' && leg.tripId.length > 0;
}

/**
 * The planner names vehicle categories the same way the departures endpoint
 * does, so the same map applies, including the split of one bus category into
 * two by operating agency. A category the map does not know is passed through
 * as it arrived: here the mode is display text, not a filter, so an unfamiliar
 * one is better shown than dropped.
 */
function legMode(leg: RawPlanLeg): string {
  const mapped = MODE_MAP[String(leg.mode ?? '')];
  if (mapped === undefined) return String(leg.mode ?? '');
  if (mapped === 'BUS' && !isCityBusAgency(leg.agencyName)) return 'REGIONAL_BUS';
  return mapped;
}

/**
 * The stops a leg calls at on the way, as exit points.
 *
 * A call with no identifier or no arrival time is dropped rather than guessed
 * at: an exit is advice to leave a vehicle at a named place at a named minute,
 * and half of that is not advice.
 */
function parseIntermediate(leg: RawPlanLeg): ExitPoint[] {
  const out: ExitPoint[] = [];
  for (const place of Array.isArray(leg.intermediateStops) ? leg.intermediateStops : []) {
    const stop = toRawId(String(place?.stopId ?? ''));
    const arrival = parseIso(place?.arrival ?? place?.scheduledArrival);
    if (stop.length === 0 || !Number.isFinite(arrival)) continue;
    out.push({ stop, name: String(place?.name ?? '').trim(), arrival });
  }
  return out;
}

function parseItinerary(raw: RawItinerary, target: PlanTarget): ParsedItinerary | null {
  const planned = parseIso(raw.endTime);
  if (!Number.isFinite(planned)) return null;

  const legs: ParsedLeg[] = [];
  let pendingWalkMetres = 0;
  for (const leg of Array.isArray(raw.legs) ? raw.legs : []) {
    if (!isTransitLeg(leg)) {
      pendingWalkMetres += typeof leg.distance === 'number' && Number.isFinite(leg.distance) ? leg.distance : 0;
      continue;
    }
    const departure = parseIso(leg.startTime);
    const legArrival = parseIso(leg.endTime);
    if (!Number.isFinite(departure) || !Number.isFinite(legArrival)) return null;
    const scheduled = parseIso(leg.scheduledStartTime);
    const fromStop = toRawId(String(leg.from?.stopId ?? ''));
    const toStop = toRawId(String(leg.to?.stopId ?? ''));
    if (fromStop.length === 0 || toStop.length === 0) return null;
    const fromName = String(leg.from?.name ?? '').trim();
    const track = String(leg.from?.track ?? leg.from?.scheduledTrack ?? '').trim();
    const toName = String(leg.to?.name ?? '').trim();
    const headsign = String(leg.headsign ?? '').trim();

    const previous = legs[legs.length - 1];
    // A street leg preceding this one is the walk away from the previous
    // timetabled leg. A leading one belongs to nothing here and is dropped with
    // the counter; the first leg's own start is what decides whether the
    // itinerary begins at the board's stop at all.
    if (previous !== undefined) previous.walkMetresAfter = pendingWalkMetres;
    pendingWalkMetres = 0;

    legs.push({
      kind: 'transit',
      line: normaliseLineName(leg.routeShortName),
      mode: legMode(leg),
      from: fromName,
      to: toName,
      departure,
      arrival: legArrival,
      fromStop,
      toStop,
      toName,
      scheduledDeparture: Number.isFinite(scheduled) ? scheduled : departure,
      fromTrack: track.length > 0 ? track : null,
      headsign: headsign.length > 0 ? headsign : null,
      intermediate: parseIntermediate(leg),
      walkMetresAfter: 0,
    });
  }

  if (legs.length === 0) return null;
  // The final walk is the configuration's when the target is a stop whose walk
  // somebody has measured, and the planner's own street leg otherwise. A stop
  // the reader themselves is travelling to has a walk of zero, which is the
  // same rule with the same answer.
  const last = legs[legs.length - 1] as ParsedLeg;
  const finalWalkMs = target.walkMinutes === null ? Math.max(0, planned - last.arrival) : target.walkMinutes * 60_000;
  return { legs, arrival: last.arrival + finalWalkMs, destinationName: target.name, finalWalkMs };
}

/** A walking leg between two named places. */
function walkLeg(from: string, to: string, departure: number, walkMs: number): RouteLeg {
  return { kind: 'walk', line: '', mode: WALK_MODE, from, to, departure, arrival: departure + walkMs };
}

/** How long a walk of this many metres takes, in milliseconds. */
function walkMsFor(metres: number): number {
  return (metres / WALK_METRES_PER_MINUTE) * 60_000;
}

/** The minutes on foot in a list of legs. */
function walkMinutesIn(legs: readonly RouteLeg[]): number {
  let ms = 0;
  for (const leg of legs) if (leg.kind === 'walk') ms += leg.arrival - leg.departure;
  return ms / 60_000;
}

interface CacheEntry {
  at: number;
  itineraries: ParsedItinerary[];
}

// Keyed by origin, destination and the minute the plan starts from, so a page
// refreshing twice a minute plans once. The early buffer is deliberately not
// part of the key: it changes only how the itineraries are recombined, which is
// arithmetic on data already in hand.
const planCache = new Map<string, CacheEntry>();

/**
 * How many journey searches may be in the air at once.
 *
 * A planned board asks one search per destination target per row window, and a
 * profile can have several planned boards. Unbounded, a long horizon turns one
 * refresh into dozens of simultaneous searches against a free public service.
 */
const PLAN_CONCURRENCY = 4;

function cacheGet(key: string, now: number): ParsedItinerary[] | null {
  const entry = planCache.get(key);
  if (entry === undefined) return null;
  if (now - entry.at >= PLAN_CACHE_MS) {
    planCache.delete(key);
    return null;
  }
  return entry.itineraries;
}

function cacheSet(key: string, now: number, itineraries: ParsedItinerary[]): void {
  for (const [otherKey, entry] of planCache) {
    if (now - entry.at >= PLAN_CACHE_MS) planCache.delete(otherKey);
  }
  planCache.set(key, { at: now, itineraries });
}

/**
 * Origins the planner has answered 404 for.
 *
 * This is remembered without an expiry, unlike the itineraries, because it is a
 * property of the aggregator's data rather than of the moment: a parent stop
 * identifier the aggregator carries only at platform level will not start
 * existing a minute later. Without it a board configured with such a stop asks
 * the same unanswerable question on every refresh, twice a minute for as long
 * as the tab is open, which is a rude thing to do to a free and unauthenticated
 * service. The caller still gets the same error every time; it just stops being
 * a request.
 */
const unknownOrigins = new Map<string, HttpError>();

// Keyed on the resolved origin, not on the stop as configured: the resolution
// chain exists precisely because those two are not always the same identifier.

/** Drop everything cached. Tests use it so one case cannot answer another. */
export function clearPlanCache(): void {
  planCache.clear();
  unknownOrigins.clear();
  resetInflight();
}

/**
 * The platform identifiers this board's own rows depart from, most used first.
 *
 * Ordering by count matters: a journey plan is asked from one place, and the
 * platform most of the board's departures leave from is the one most of its
 * journeys start at. Rows that carry no platform identifier contribute nothing
 * rather than a guess.
 */
function platformsOf(rows: Departure[]): string[] {
  const counts = new Map<string, number>();
  for (const row of rows) {
    const id = row.stopPoint;
    if (id === undefined || id.length === 0) continue;
    counts.set(id, (counts.get(id) ?? 0) + 1);
  }
  return [...counts.entries()].sort((a, b) => b[1] - a[1]).map(([id]) => id);
}

function placeParam(place: PlanDestination): string {
  return 'id' in place ? toAggregatorId(place.id) : `${place.lat},${place.lon}`;
}

/**
 * How wide a single search window is asked for, in seconds.
 *
 * Bounded at both ends. The floor is the service's own default, because a board
 * whose last row is three minutes away still wants more than three minutes of
 * itineraries to recombine tails from. The ceiling exists because a board may
 * run a twenty-four hour horizon and asking a free service to search a day in
 * one request is not a reasonable thing to do; past the ceiling the cursor walk
 * takes over, which is what it is for.
 */
export const PLAN_SEARCH_WINDOW_MIN_SECONDS = 900;
export const PLAN_SEARCH_WINDOW_MAX_SECONDS = 4 * 3600;

/**
 * The identifier to send as `toPlace` for one target.
 *
 * A stop target goes through the same resolution chain as the origin, for the
 * same reason: the national feed does not always publish the parent of a stop
 * area, and a destination named by its parent identifier is refused exactly as
 * an origin would be. It gets no platform identifiers to try, because nothing
 * here has departure rows for the far end of the journey, so the chain is the
 * parent and then the station's coordinate.
 */
async function resolveTarget(target: PlanTarget, options: PlanBoardOptions): Promise<{ place: string; target: PlanTarget }> {
  if (!('id' in target.place)) return { place: placeParam(target.place), target };
  const resolved = await resolveOrigin({
    stop: target.place.id,
    ...(options.baseUrl === undefined ? {} : { baseUrl: options.baseUrl }),
    ...(options.fetchImpl === undefined ? {} : { fetchImpl: options.fetchImpl }),
    ...(options.originCache === undefined ? {} : { cache: options.originCache }),
    ...(options.onDebug === undefined ? {} : { onDebug: options.onDebug }),
  });
  // The configured walk is the walk *from that stop*, so it only applies while
  // the plan really ends at that stop. When the chain has to fall back to the
  // station's coordinate the planner alights wherever it likes and walks from
  // there, and overriding that with a figure measured from somewhere else is
  // how a fourteen minute walk gets reported as none at all. Measured: a bus
  // stop at the doorstep, walk zero, whose parent identifier the aggregator
  // carries only as a position, was handed an S-Bahn arriving a quarter of an
  // hour away and reported it as arriving at the door.
  if (resolved.level === 'coordinate') return { place: resolved.place, target: { ...target, walkMinutes: null } };
  return { place: resolved.place, target };
}

function searchWindowSeconds(startMs: number, coverThroughMs: number): number {
  const wanted = Math.ceil((coverThroughMs - startMs) / 1000);
  if (!Number.isFinite(wanted)) return PLAN_SEARCH_WINDOW_MIN_SECONDS;
  return Math.min(PLAN_SEARCH_WINDOW_MAX_SECONDS, Math.max(PLAN_SEARCH_WINDOW_MIN_SECONDS, wanted));
}

/**
 * Walk the planner's cursor pages until the itineraries reach the last row the
 * caller cares about, or the cap stops the walk.
 *
 * Coverage is measured on itineraries that actually start at the board's stop.
 * Measuring it on every itinerary would let one that walks off to a neighbouring
 * stop first, and therefore matches no row, declare the board covered.
 */
async function fetchItineraries(
  fromPlace: string,
  toPlace: string,
  target: PlanTarget,
  startMs: number,
  coverThroughMs: number,
  boardStop: string,
  baseUrl: string,
  planModes: readonly string[],
  fetchImpl: FetchLike | undefined,
  onDebug: ((line: string) => void) | undefined,
): Promise<ParsedItinerary[]> {
  const out: ParsedItinerary[] = [];
  let cursor: string | null = null;
  // One wide window instead of a walk. Measured against the live service: a
  // request with `searchWindow=10800` answered in 357 ms with itineraries
  // spanning 184 minutes of departures, where the same request without it
  // needed two cursor pages and 620 ms to cover less. The cursor walk below is
  // kept as the safety net for a board whose horizon outruns even this window.
  const windowSeconds = searchWindowSeconds(startMs, coverThroughMs);

  for (let page = 0; page < PLAN_MAX_PAGES; page += 1) {
    const base =
      `fromPlace=${encodeURIComponent(fromPlace)}&toPlace=${encodeURIComponent(toPlace)}` +
      `&numItineraries=${ITINERARIES_PER_REQUEST}` +
      `&searchWindow=${windowSeconds}` +
      // Sent on every page, cursor pages included: the cursor carries a position
      // in the search, not the search's own parameters.
      `&transitModes=${encodeURIComponent(planModes.join(','))}`;
    const query =
      cursor === null
        ? `${base}&time=${encodeURIComponent(new Date(startMs).toISOString())}&arriveBy=false`
        : `${base}&pageCursor=${encodeURIComponent(cursor)}`;
    const url = `${baseUrl}/plan?${query}`;
    onDebug?.(`GET ${url}`);
    const body: RawPlan = await fetchJson<RawPlan>(url, fetchImpl ? { fetchImpl } : {});
    const batch = Array.isArray(body.itineraries) ? body.itineraries : [];
    onDebug?.(`plan page ${page + 1} itineraries=${batch.length}`);

    let latestAtStop = Number.NEGATIVE_INFINITY;
    for (const raw of batch) {
      const parsed = parseItinerary(raw, target);
      if (parsed === null) continue;
      out.push(parsed);
      const first = parsed.legs[0];
      if (first !== undefined && sameStopArea(first.fromStop, boardStop) && first.departure > latestAtStop) {
        latestAtStop = first.departure;
      }
    }

    if (batch.length === 0) break;
    if (latestAtStop >= coverThroughMs) break;
    const next: string = typeof body.nextPageCursor === 'string' ? body.nextPageCursor : '';
    if (next.length === 0 || next === cursor) break;
    cursor = next;
  }

  return out;
}

/**
 * Everything the recombination needs, built once from every itinerary on every
 * page: the journey tails available at each boarding stop, and the street
 * distance between each pair of platforms anyone was observed walking between.
 *
 * Tails are collected from *all* itineraries, including those whose first leg
 * matches no row on the board. That itinerary is useless as a whole journey and
 * its tail is not: it is the same onward service, and which vehicle brought the
 * rider to the interchange has no bearing on what leaves it.
 */
interface OnwardIndex {
  /** Boarding stop id to the tails that start there. */
  chains: Map<string, OnwardChain[]>;
  /** Alighting stop id to boarding stop id to the shortest distance seen. */
  walks: Map<string, Map<string, number>>;
}

function buildOnwardIndex(itineraries: ParsedItinerary[]): OnwardIndex {
  const chains = new Map<string, OnwardChain[]>();
  const walks = new Map<string, Map<string, number>>();

  for (const itinerary of itineraries) {
    for (let i = 1; i < itinerary.legs.length; i += 1) {
      const previous = itinerary.legs[i - 1] as ParsedLeg;
      const leg = itinerary.legs[i] as ParsedLeg;

      let fromPrevious = walks.get(previous.toStop);
      if (fromPrevious === undefined) {
        fromPrevious = new Map<string, number>();
        walks.set(previous.toStop, fromPrevious);
      }
      // The shortest observed path between two platforms is the best estimate
      // of the distance; conservatism belongs in the early buffer, where it is
      // named, and not smuggled into a measurement.
      const known = fromPrevious.get(leg.fromStop);
      if (known === undefined || previous.walkMetresAfter < known) {
        fromPrevious.set(leg.fromStop, previous.walkMetresAfter);
      }

      let atStop = chains.get(leg.fromStop);
      if (atStop === undefined) {
        atStop = [];
        chains.set(leg.fromStop, atStop);
      }
      const legs = tailLegs(itinerary, i);
      atStop.push({
        boardStop: leg.fromStop,
        boardName: leg.from,
        departure: leg.departure,
        legs,
        arrival: itinerary.arrival,
        walkMinutes: walkMinutesIn(legs),
        destinationName: itinerary.destinationName,
        transfers: itinerary.legs.length - 1 - i,
      });
    }
  }

  return { chains, walks };
}

/**
 * An itinerary from its `index`-th timetabled leg onwards, with the walks
 * between the legs and the final walk to the destination put back in.
 *
 * The walks between legs are derived from the street distance rather than
 * copied from the planner's own street-leg duration, for the reason given at
 * `WALK_METRES_PER_MINUTE`: that duration carries the station's minimum
 * transfer time, which is padding and not walking.
 */
function tailLegs(itinerary: ParsedItinerary, index: number): RouteLeg[] {
  const out: RouteLeg[] = [];
  for (let i = index; i < itinerary.legs.length; i += 1) {
    const leg = itinerary.legs[i] as ParsedLeg;
    out.push(publicLeg(leg));
    const next = itinerary.legs[i + 1];
    if (next === undefined) break;
    if (leg.walkMetresAfter > 0) out.push(walkLeg(leg.to, next.from, leg.arrival, walkMsFor(leg.walkMetresAfter)));
  }
  const last = itinerary.legs[itinerary.legs.length - 1] as ParsedLeg;
  if (itinerary.finalWalkMs > 0) {
    out.push(walkLeg(last.to, itinerary.destinationName, last.arrival, itinerary.finalWalkMs));
  }
  return out;
}

function publicLeg(leg: ParsedLeg): RouteLeg {
  return {
    kind: 'transit',
    line: leg.line,
    mode: leg.mode,
    from: leg.from,
    to: leg.to,
    departure: leg.departure,
    arrival: leg.arrival,
  };
}

/**
 * Whether a board row and an itinerary leg are the same vehicle.
 *
 * Line and minute alone are not unique, and the exception is exactly the case
 * that matters. A stop whose two directions share one parent identifier can run
 * the same line at the same minute each way: an S-Bahn line was observed leaving
 * for the city and leaving for the country in the same minute from one station,
 * and matching on line and minute alone handed the outbound row the inbound
 * journey. That is the worst answer this package can give, because it is
 * plausible and wrong, and it tells someone to board a train going the other
 * way.
 *
 * The platform used to be the whole of the answer and it is not good enough.
 * At a large station the two feeds do not spell a platform the same way: the
 * departure board says platform 6 and the planner says track 86 for the same
 * physical edge, and the planner says 6 for it on the next train. That is not a
 * disagreement about facts, it is two identifier schemes for one piece of
 * concrete, and holding a row to it cost the busiest board on the page most of
 * its journeys. Measured on one afternoon: every regional departure towards the
 * city and half the rapid-transit ones had an itinerary waiting for them and
 * were refused it.
 *
 * So the platform is believed when it agrees, and where it does not the
 * question falls back to where the vehicle says it is going. A headsign is what
 * separates the two directions of a line at one minute, which is the collision
 * the platform was brought in to settle in the first place, and both feeds
 * publish it.
 */
function sameVehicle(row: Departure, leg: ParsedLeg): boolean {
  if (row.platform !== null && leg.fromTrack !== null && samePlatformLabel(row.platform, leg.fromTrack)) return true;
  if (row.platform === null || leg.fromTrack === null) return true;
  return sameDestinationLabel(row.destination, leg.headsign);
}

function samePlatformLabel(rowPlatform: string, legTrack: string): boolean {
  return rowPlatform.trim().toLowerCase() === legTrack.trim().toLowerCase();
}

/**
 * Whether two feeds are naming the same place as a destination.
 *
 * Folded rather than compared, because the two spell a terminus differently
 * often enough that equality would answer no to trains that are plainly the
 * same: punctuation, the bracketed district a long-distance feed likes to add,
 * and the spacing around it.
 */
function sameDestinationLabel(rowDestination: string, headsign: string | null): boolean {
  if (headsign === null) return false;
  const fold = (value: string): string =>
    value
      .toLowerCase()
      .replace(/\(.*?\)/g, ' ')
      .replace(/[^a-z0-9äöüß]+/g, ' ')
      .trim();
  const a = fold(rowDestination);
  const b = fold(headsign);
  if (a.length === 0 || b.length === 0) return false;
  return a === b || a.startsWith(b) || b.startsWith(a);
}

/** A line label and a departure minute, folded into one comparable string. */
function matchKey(line: string, epochMs: number): string {
  return `${normaliseLine(line)}|${Math.floor(epochMs / 60_000)}`;
}

/**
 * A key that makes two spellings of the same journey one journey.
 *
 * Only the vehicles, because the walks are what differ between two spellings of
 * the same ride: getting off one stop earlier and walking to the same platform
 * is the same journey, and it is the walk that decides which spelling wins.
 * The destination is in the key because two targets are two journeys even when
 * every vehicle agrees.
 */
function optionKey(option: RouteOption): string {
  const vehicles = transitLegs(option).map((leg) => `${normaliseLine(leg.line)}@${leg.departure}`);
  return `${option.destinationName}|${vehicles.join('>')}`;
}

/**
 * The route an option represents, as opposed to the particular ride: where it
 * puts the rider down off the first vehicle, and which lines carry them on
 * from there.
 *
 * "Sequence of onward lines" means the transit legs after the first one, in
 * order, transit legs only. The first transit leg is always this row's own
 * departure, so it is fixed across every option of a row and carries no
 * information here; a walk leg is not a line and is skipped, since two
 * options that differ only in which platform they use for the identical
 * change are still the identical change. `destinationName` is folded in
 * too: two options that agree on exit and onward lines but end at different
 * configured targets are not one route, they are two.
 */
/**
 * Which of two rides of the same route represents that route.
 *
 * Earliest arrival, except that a comfortable ride beats a tight one however
 * much earlier the tight one lands. Arrival alone was the obvious rule and it
 * quietly cost rows their recommendation: a tight option is never recommended,
 * by design, because it is offered so a rider can choose to gamble and not so
 * the tool can gamble for them. So a group whose earliest ride was tight
 * elected a representative that could never be recommended, and the row came
 * back with no journey at all while a perfectly good ride of the same route two
 * minutes later sat unshown. One representative per route is the rule; which
 * one it is has to follow the same courtesy as everything else here.
 */
function beatsWithinRoute(option: RouteOption, incumbent: RouteOption): boolean {
  if (option.tight !== incumbent.tight) return !option.tight;
  return option.arrival < incumbent.arrival;
}

function routeKey(option: RouteOption): string {
  const onward = transitLegs(option)
    .slice(1)
    .map((leg) => normaliseLine(leg.line));
  return `${option.destinationName}|${option.exitStop}|${onward.join('>')}`;
}

/**
 * The journeys that begin with `first`.
 *
 * The recombination is the point of this function. The planner returns only
 * itineraries it considers feasible, and it judges feasibility with the padded
 * transfer time described at `WALK_METRES_PER_MINUTE`, so taking its
 * itineraries at face value would never surface a change it had already ruled
 * out. Splicing the observed tails onto this leg instead, and judging the change
 * with a walk derived from distance, recovers exactly those.
 */
function optionsFor(
  first: ParsedLeg,
  ownItinerary: ParsedItinerary,
  index: OnwardIndex,
  earlyBufferMs: number,
  walkWeight: number,
): RouteOption[] {
  const found = new Map<string, RouteOption>();

  const add = (option: RouteOption): void => {
    const key = optionKey(option);
    const existing = found.get(key);
    if (existing === undefined) {
      found.set(key, option);
      return;
    }
    // The same onward journey can be reached by getting off at more than one
    // stop, and that is one journey rather than several. The kinder verdict
    // wins first: if any observed walk makes the change comfortable, calling it
    // tight would be an artefact of the indexing. Otherwise the later exit
    // wins, which means less walking and more time on a vehicle the rider is
    // already sitting on.
    if (existing.tight !== option.tight) {
      if (existing.tight) found.set(key, option);
      return;
    }
    // Then the one that walks less, which is the whole reason two exits onto
    // one onward service are worth telling apart.
    if (option.walkMinutes !== existing.walkMinutes) {
      if (option.walkMinutes < existing.walkMinutes) found.set(key, option);
      return;
    }
    if (exitArrival(option) > exitArrival(existing)) found.set(key, option);
  };

  // A journey with no change at all: ride this leg, then walk. It has no tail
  // to splice and so cannot come out of the index.
  if (ownItinerary.legs.length === 1) {
    const legs: RouteLeg[] = [publicLeg(first)];
    if (ownItinerary.finalWalkMs > 0) {
      legs.push(walkLeg(first.toName, ownItinerary.destinationName, first.arrival, ownItinerary.finalWalkMs));
    }
    add({
      exitStop: first.toStop,
      exitStopName: first.toName,
      legs,
      arrival: ownItinerary.arrival,
      transfers: 0,
      tight: false,
      tightBy: 0,
      walkMinutes: walkMinutesIn(legs),
      destinationName: ownItinerary.destinationName,
    });
  }

  // Every stop this leg calls at is somewhere a rider can get off, not just the
  // one the planner's own itinerary happened to alight at.
  const exits: ExitPoint[] = [...first.intermediate, { stop: first.toStop, name: first.toName, arrival: first.arrival }];

  for (const exit of exits) {
    // Boarding points reachable from this exit: the same platform, at no cost,
    // plus every platform somebody was observed walking to *from this exact
    // platform*. Deliberately an exact match and not a same-stop-area one: a
    // chain boarding elsewhere in the station is only catchable if the walk to
    // it has been measured, and treating an unmeasured interchange as instant
    // is how a plausible and wrong recommendation gets made.
    const reachable = new Map<string, number>([[exit.stop, 0]]);
    for (const [boardStop, metres] of index.walks.get(exit.stop) ?? []) {
      const known = reachable.get(boardStop);
      if (known === undefined || metres < known) reachable.set(boardStop, metres);
    }

    for (const [boardStop, metres] of reachable) {
      const walkMs = walkMsFor(metres);
      const feasible = exit.arrival + walkMs;
      for (const chain of index.chains.get(boardStop) ?? []) {
        if (chain.departure < feasible - earlyBufferMs) continue;
        // Riding the leg only as far as the exit: same vehicle, shorter ride.
        const ridden: RouteLeg = { ...publicLeg(first), to: exit.name, arrival: exit.arrival };
        const legs: RouteLeg[] =
          metres > 0
            ? [ridden, walkLeg(exit.name, chain.boardName, exit.arrival, walkMs), ...chain.legs]
            : [ridden, ...chain.legs];
        add({
          exitStop: exit.stop,
          exitStopName: exit.name,
          legs,
          arrival: chain.arrival,
          transfers: chain.transfers + 1,
          tight: chain.departure < feasible,
          tightBy: Math.max(0, Math.ceil((feasible - chain.departure) / 60_000)),
          walkMinutes: walkMinutesIn(legs),
          destinationName: chain.destinationName,
        });
      }
    }
  }

  return [...found.values()].sort(orderBy(walkWeight)).slice(0, MAX_OPTIONS_PER_ROW);
}

/**
 * How options are ordered: by score, then by the number of changes, then by the
 * verdict, then by staying on the first vehicle longer. Used both inside one
 * itinerary's recombination and across the whole row, so the list a reader sees
 * and the list `best` is picked from are ordered by the same rule.
 */
function orderBy(walkWeight: number): (a: RouteOption, b: RouteOption) => number {
  return (a, b) =>
    optionScore(a, walkWeight) - optionScore(b, walkWeight) ||
    a.transfers - b.transfers ||
    Number(a.tight) - Number(b.tight) ||
    exitArrival(b) - exitArrival(a);
}

/** When the rider leaves the first vehicle, which is the first leg's own end. */
function exitArrival(option: RouteOption): number {
  return option.legs[0]?.arrival ?? Number.NEGATIVE_INFINITY;
}

/**
 * Plan every row of one board to one destination.
 *
 * An itinerary belongs to a row when its first timetabled leg carries the same
 * line and leaves in the same minute. Not the trip id: the departure board and
 * the planner do not share one. The board's rows come from whichever backend
 * answered, which may be the primary one, and its trip identifiers are its own;
 * even on the aggregator, a board row is a stop time and an itinerary leg is a
 * trip, and nothing in either response links them. Line and minute do link
 * them, because no line runs twice from one stop in one minute.
 *
 * Which minute, though, is the whole difficulty. Matching on the expected
 * departure loses every delayed row: the board and the planner carry separate
 * live feeds, the two disagree by a minute or more whenever anything is late,
 * and against the primary backend that silently emptied every delayed row of
 * its journeys while the punctual ones planned fine. The timetabled minute is
 * the half both sides agree on, because it is the same timetable, so it is
 * tried first and the expected minute only backs it up for a row whose backend
 * publishes no scheduled time of its own.
 */
/**
 * The phrase a row with no journey carries.
 *
 * The cases are genuinely different. Nobody offered a journey starting with this
 * departure, which on a board with several lines is usually the honest answer:
 * that train is not the way to go. Somebody did offer one and it was refused
 * here, which is this file's fault and used to be invisible. A journey was
 * matched and produced nothing worth showing. The departure is cancelled, which
 * is a refusal on purpose.
 */
function missFor(row: Departure, matched: Set<Departure>, refused: Set<Departure>): string {
  if (row.cancelled) return 'this departure is cancelled, so it is not recommended';
  if (matched.has(row)) return 'the journey that starts here had no onward connection worth offering';
  if (refused.has(row)) return 'a journey for this line and minute was offered and did not match this row';
  return 'no itinerary the planner returned starts with this departure';
}

export async function planBoard(options: PlanBoardOptions): Promise<PlannedRow[]> {
  const rows = options.rows;
  if (rows.length === 0) return [];

  const baseUrl = (options.baseUrl ?? envOverride('TRANSITOUS_BASE_URL') ?? TRANSITOUS_DEFAULT_BASE_URL).replace(
    /\/+$/,
    '',
  );
  // Which identifier the aggregator will accept for this stop, which is not
  // always the one the board is configured with. Cached, so this costs one
  // small request the first time a stop is planned and nothing afterwards.
  const resolved = await resolveOrigin({
    stop: options.stop,
    platformIds: platformsOf(rows),
    baseUrl: options.baseUrl,
    ...(options.fetchImpl === undefined ? {} : { fetchImpl: options.fetchImpl }),
    ...(options.originCache === undefined ? {} : { cache: options.originCache }),
    ...(options.onDebug === undefined ? {} : { onDebug: options.onDebug }),
  });
  options.onOrigin?.(resolved);
  const fromPlace = resolved.place;
  const planModes = options.planModes ?? DEFAULT_PLAN_MODES;
  const walkWeight = options.walkWeight ?? DEFAULT_WALK_WEIGHT;
  const startMinute = Math.floor(options.startMs / 60_000);

  const known = unknownOrigins.get(fromPlace);
  if (known !== undefined) throw known;

  // The caller's own figure wins when it gives one; otherwise the last row this
  // function was handed is the only thing it can go on, which is the behaviour
  // this option exists to let a caller override.
  let coverThrough = options.coverThroughMs;
  if (coverThrough === undefined) {
    coverThrough = Number.NEGATIVE_INFINITY;
    for (const row of rows) if (row.realtime > coverThrough) coverThrough = row.realtime;
  }

  // One search per destination, all at once. They are independent questions to
  // the same service and running them one after another would multiply the
  // slowest part of a refresh by the number of places the reader might be
  // going.
  //
  // Settled rather than all: a destination stop the aggregator does not carry
  // is a target to drop, not a reason to leave the board with no journeys at
  // all. A profile's stops are ordinary configured identifiers and the
  // aggregator's coverage of parent stops is exactly the thing the origin chain
  // exists to work around, so this happens in practice and not in theory.
  const perTarget = await Promise.allSettled(
    options.targets.map(async (target) => {
      const resolvedTarget = await resolveTarget(target, options);
      const toPlace = resolvedTarget.place;
      // The modes are part of the key: two searches over different modes are two
      // different searches, and one must not answer the other. The cover-through
      // minute is too, for the same reason: the same origin, destination and
      // start minute asked with two different cover-through points are two
      // different questions, one of them widening the search past where the
      // other one stopped, and a cached answer to the narrower question must
      // never be handed back for the wider one.
      const cacheKey = `${fromPlace}|${toPlace}|${startMinute}|${planModes.join(',')}|${Math.floor(coverThrough / 60_000)}`;
      const cached = cacheGet(cacheKey, options.startMs);
      if (cached !== null) return cached;
      let fetched: ParsedItinerary[];
      try {
        // Shared by promise as well as cached by result. Boards are planned
        // together, so two boards heading to the same place from the same stop
        // ask this identical question in the same instant and the cache, which
        // is only written when an answer comes back, cannot help either of them.
        fetched = await share(`plan|${cacheKey}`, () =>
          withLimit('plan', PLAN_CONCURRENCY, () =>
            fetchItineraries(
              fromPlace,
              toPlace,
              resolvedTarget.target,
              options.startMs,
              coverThrough,
              options.stop,
              baseUrl,
              planModes,
              options.fetchImpl,
              options.onDebug,
            ),
          ),
        );
      } catch (error) {
        // Not recorded against the origin here: a 404 names one of the two ends
        // and this function cannot tell which. That verdict is reached below,
        // once every destination has failed and the origin is what they had in
        // common.
        options.onDebug?.(`plan to ${toPlace} failed: ${error instanceof Error ? error.message : String(error)}`);
        throw error;
      }
      cacheSet(cacheKey, options.startMs, fetched);
      return fetched;
    }),
  );
  const answered = perTarget.filter((result) => result.status === 'fulfilled');
  if (answered.length === 0) {
    const first = perTarget[0];
    if (first !== undefined && first.status === 'rejected') {
      // Every destination failed, so the origin is the thing in common and a
      // 404 is worth remembering against it. One failed destination out of
      // several says nothing about the origin and must not be recorded here:
      // doing so would make one unreachable station poison every plan from
      // this stop for the life of the process.
      const error: unknown = first.reason;
      if (error instanceof HttpError && error.status === 404) unknownOrigins.set(fromPlace, error);
      throw error;
    }
    return rows.map((row) => ({ departure: row, best: null, options: [] }));
  }
  const itineraries = answered.flatMap((result) => result.value);

  const index = buildOnwardIndex(itineraries);
  const earlyBufferMs = (options.earlyBufferMinutes ?? DEFAULT_EARLY_BUFFER_MINUTES) * 60_000;

  const planned = new Map<Departure, RouteOption[]>();
  for (const row of rows) planned.set(row, []);

  const byScheduled = new Map<string, Departure[]>();
  const byExpected = new Map<string, Departure[]>();
  const addRow = (into: Map<string, Departure[]>, key: string, row: Departure): void => {
    const bucket = into.get(key);
    if (bucket === undefined) into.set(key, [row]);
    else bucket.push(row);
  };
  for (const row of rows) {
    addRow(byScheduled, matchKey(row.line, row.planned), row);
    addRow(byExpected, matchKey(row.line, row.realtime), row);
  }

  // Why a row ended up with nothing, for the rows that do. An empty journey slot
  // is the page's most common complaint and the least diagnosable one, because
  // "no journey" covers two entirely different facts: the planner never offered
  // a journey starting with this departure, which is ordinary on a board whose
  // lines are not all worth taking, and the planner did offer one and it was
  // refused here, which is a bug every time. On a phone, at a stop, with no
  // console, they look identical.
  const refused = new Set<Departure>();
  const matched = new Set<Departure>();

  for (const itinerary of itineraries) {
    const first = itinerary.legs[0];
    if (first === undefined) continue;
    // An itinerary that walks to a different stop before boarding is a journey
    // from somewhere else; this board cannot put anyone on it.
    if (!sameStopArea(first.fromStop, options.stop)) continue;
    const sameMinute =
      byScheduled.get(matchKey(first.line, first.scheduledDeparture)) ??
      byExpected.get(matchKey(first.line, first.departure));
    if (sameMinute === undefined) continue;
    const matches = sameMinute.filter((row) => sameVehicle(row, first));
    if (matches.length === 0) {
      for (const row of sameMinute) refused.add(row);
      continue;
    }
    const candidates = optionsFor(first, itinerary, index, earlyBufferMs, walkWeight);
    for (const row of matches) {
      matched.add(row);
      (planned.get(row) as RouteOption[]).push(...candidates);
    }
  }

  return rows.map((row) => {
    const collected = planned.get(row) ?? [];
    const unique = new Map<string, RouteOption>();
    for (const option of collected) {
      const key = optionKey(option);
      const existing = unique.get(key);
      if (existing === undefined) {
        unique.set(key, option);
        continue;
      }
      // The same journey found twice: the kinder verdict first, then the one
      // that walks less.
      if (existing.tight !== option.tight) {
        if (existing.tight) unique.set(key, option);
        continue;
      }
      if (option.walkMinutes < existing.walkMinutes) unique.set(key, option);
    }
    // A later ride of the same route is not an alternative, it is the next row
    // on the board: same exit, same onward lines, just a later departure. This
    // groups the survivors above by route and keeps only the one that arrives
    // earliest in each group, so the cap below counts distinct routes rather
    // than repeats of one.
    const distinctRoutes = new Map<string, RouteOption>();
    for (const option of unique.values()) {
      const key = routeKey(option);
      const existing = distinctRoutes.get(key);
      if (existing === undefined || beatsWithinRoute(option, existing)) distinctRoutes.set(key, option);
    }
    const list = [...distinctRoutes.values()].sort(orderBy(walkWeight)).slice(0, MAX_OPTIONS_PER_ROW);
    // A tight option is never recommended: it is offered so a rider can choose
    // to gamble on it, not so the tool can gamble on their behalf.
    //
    // A cancelled departure gets no recommendation either, however good the
    // planner thinks it is. The planner works from the timetable and does not
    // always know the vehicle has been withdrawn, and a recommendation to take a
    // train that is not running is worse than no recommendation. The options are
    // still carried, because what the timetable said is worth having; what is
    // withdrawn is the endorsement.
    const best = row.cancelled ? null : (list.find((option) => !option.tight) ?? null);
    if (list.length > 0) return { departure: row, best, options: list };
    return { departure: row, best, options: list, miss: missFor(row, matched, refused) };
  });
}

// The machine-readable surface, in the same snake_case and ISO instants as the
// rest of the JSON contract. It lives here rather than in `src/json.ts` because
// it serialises this module's own vocabulary and nothing else consumes it.

export function routeLegJson(leg: RouteLeg): unknown {
  return {
    kind: leg.kind,
    line: leg.line,
    mode: leg.mode,
    from: leg.from,
    to: leg.to,
    departure: toIso(leg.departure),
    arrival: toIso(leg.arrival),
  };
}

export function routeOptionJson(option: RouteOption): unknown {
  return {
    exit_stop: option.exitStop,
    exit_stop_name: option.exitStopName,
    destination: option.destinationName,
    arrival: toIso(option.arrival),
    transfers: option.transfers,
    tight: option.tight,
    tight_by_minutes: option.tightBy,
    walk_minutes: Math.round(option.walkMinutes),
    legs: option.legs.map(routeLegJson),
  };
}

export function plannedRowJson(row: PlannedRow, board: WalkSource, now: number, multiStop: boolean): unknown {
  return {
    departure: departureJson(row.departure, board, now, multiStop),
    best: row.best === null ? null : routeOptionJson(row.best),
    options: row.options.map(routeOptionJson),
    // Only on a row that has none, and the same sentence the sheet shows. A
    // board whose journeys are missing is the complaint this package gets most
    // often, and answering it from the command line should not need a debugger.
    ...(row.miss === undefined ? {} : { miss: row.miss }),
  };
}

/** One board's planned rows, as the CLI assembles them. */
export interface PlannedBoard {
  board: Board;
  rows: PlannedRow[];
  /**
   * Which step of the origin chain answered for this board's stop, or null
   * when it could not be planned at all. Published because a plan made from a
   * coordinate or from one platform is a slightly weaker claim than one made
   * from the stop itself, and a reader comparing a surprising route against
   * their own knowledge deserves to know which it was.
   */
  origin?: OriginLevel | null;
}

/**
 * The `route --json` document. It names the destination by key and by name and
 * carries no coordinates: the coordinate was the input, the reader already has
 * it, and this package keeps geography out of anything it prints by habit.
 */
export function routeDocument(input: {
  profile: string | null;
  requestedProfile: string | null;
  destinationKey: string;
  destinationName: string;
  earlyBufferMinutes: number;
  walkWeight: number;
  boards: PlannedBoard[];
  now: number;
}): unknown {
  return {
    schema_version: SCHEMA_VERSION,
    profile: input.profile,
    requested_profile: input.requestedProfile,
    destination: { key: input.destinationKey, name: input.destinationName },
    early_buffer_minutes: input.earlyBufferMinutes,
    walk_weight: input.walkWeight,
    generated_at: toIso(input.now),
    boards: input.boards.map(({ board, rows, origin }) => ({
      title: board.title,
      origin_resolution: origin ?? null,
      stops: board.stops,
      walk_minutes: board.walkMinutes,
      walk_minutes_by_stop: board.walkMinutesByStop ?? null,
      stop_labels: board.stopLabels ?? null,
      backend: board.backend,
      rows: rows.map((row) => plannedRowJson(row, board, input.now, board.stops.length > 1)),
    })),
  };
}
