import {
  isCityBusAgency,
  MODE_MAP,
  normaliseLineName,
  toAggregatorId,
  toRawId,
  TRANSITOUS_DEFAULT_BASE_URL,
} from './backends/transitous.ts';
import { DEFAULT_PLAN_MODES } from './config.ts';
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

/** One timetabled leg of a journey. `from` and `to` are stop names, for display. */
export interface RouteLeg {
  line: string;
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
}

/** Where a plan ends: a stop the aggregator knows, or a bare coordinate. */
export type PlanDestination = { lat: number; lon: number } | { id: string };

export interface PlanBoardOptions {
  stop: string;
  destination: PlanDestination;
  rows: Departure[];
  startMs: number;
  earlyBufferMinutes?: number;
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
  agencyName?: string;
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
interface ParsedLeg extends RouteLeg {
  fromStop: string;
  toStop: string;
  toName: string;
  /** Timetabled departure, which is the reliable half of the match key below. */
  scheduledDeparture: number;
  /** Platform this leg is boarded from, when the planner names one. */
  fromTrack: string | null;
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
}

/** A suffix of some itinerary, indexed by where it is boarded. */
interface OnwardChain {
  boardStop: string;
  departure: number;
  legs: RouteLeg[];
  arrival: number;
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

function parseItinerary(raw: RawItinerary): ParsedItinerary | null {
  const arrival = parseIso(raw.endTime);
  if (!Number.isFinite(arrival)) return null;

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

    const previous = legs[legs.length - 1];
    // A street leg preceding this one is the walk away from the previous
    // timetabled leg. A leading one belongs to nothing here and is dropped with
    // the counter; the first leg's own start is what decides whether the
    // itinerary begins at the board's stop at all.
    if (previous !== undefined) previous.walkMetresAfter = pendingWalkMetres;
    pendingWalkMetres = 0;

    legs.push({
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
      walkMetresAfter: 0,
    });
  }

  if (legs.length === 0) return null;
  return { legs, arrival };
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

  for (let page = 0; page < PLAN_MAX_PAGES; page += 1) {
    const base =
      `fromPlace=${encodeURIComponent(fromPlace)}&toPlace=${encodeURIComponent(toPlace)}` +
      `&numItineraries=${ITINERARIES_PER_REQUEST}` +
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
      const parsed = parseItinerary(raw);
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
      atStop.push({
        boardStop: leg.fromStop,
        departure: leg.departure,
        legs: itinerary.legs.slice(i).map(publicLeg),
        arrival: itinerary.arrival,
      });
    }
  }

  return { chains, walks };
}

function publicLeg(leg: ParsedLeg): RouteLeg {
  return {
    line: leg.line,
    mode: leg.mode,
    from: leg.from,
    to: leg.to,
    departure: leg.departure,
    arrival: leg.arrival,
  };
}

/**
 * Whether a board row and an itinerary leg agree about the platform.
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
 * The platform separates them, and both sides publish one for exactly the kind
 * of stop where this happens. A row or a leg that names none is not held against
 * it: a missing platform is common on street stops, where a single direction per
 * identifier makes the collision impossible anyway.
 */
function samePlatform(rowPlatform: string | null, legTrack: string | null): boolean {
  if (rowPlatform === null || legTrack === null) return true;
  return rowPlatform.trim().toLowerCase() === legTrack.trim().toLowerCase();
}

/** A line label and a departure minute, folded into one comparable string. */
function matchKey(line: string, epochMs: number): string {
  return `${normaliseLine(line)}|${Math.floor(epochMs / 60_000)}`;
}

/** A key that makes two spellings of the same journey one journey. */
function optionKey(option: RouteOption): string {
  return option.legs.map((leg) => `${normaliseLine(leg.line)}@${leg.departure}`).join('>');
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
): RouteOption[] {
  const found = new Map<string, RouteOption>();

  const add = (option: RouteOption): void => {
    const key = optionKey(option);
    const existing = found.get(key);
    // A journey reached by two routes through the index is one journey, and the
    // kinder verdict wins: if any observed walk makes the change comfortable,
    // calling it tight would be an artefact of the indexing.
    if (existing === undefined || (existing.tight && !option.tight)) found.set(key, option);
  };

  // A journey with no change at all: ride this leg, then walk. It has no tail
  // to splice and so cannot come out of the index.
  if (ownItinerary.legs.length === 1) {
    add({
      exitStop: first.toStop,
      exitStopName: first.toName,
      legs: [publicLeg(first)],
      arrival: ownItinerary.arrival,
      transfers: 0,
      tight: false,
    });
  }

  // Boarding points reachable from where this leg puts the rider down: the same
  // platform, at no cost, plus every platform somebody was observed walking to.
  const reachable = new Map<string, number>([[first.toStop, 0]]);
  for (const [boardStop, metres] of index.walks.get(first.toStop) ?? []) {
    const known = reachable.get(boardStop);
    if (known === undefined || metres < known) reachable.set(boardStop, metres);
  }

  for (const [boardStop, metres] of reachable) {
    const walkMs = (metres / WALK_METRES_PER_MINUTE) * 60_000;
    const feasible = first.arrival + walkMs;
    for (const chain of index.chains.get(boardStop) ?? []) {
      if (chain.departure < feasible - earlyBufferMs) continue;
      add({
        exitStop: first.toStop,
        exitStopName: first.toName,
        legs: [publicLeg(first), ...chain.legs],
        arrival: chain.arrival,
        transfers: chain.legs.length,
        tight: chain.departure < feasible,
      });
    }
  }

  return [...found.values()]
    .sort((a, b) => a.arrival - b.arrival || a.transfers - b.transfers || Number(a.tight) - Number(b.tight))
    .slice(0, MAX_OPTIONS_PER_ROW);
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
  const toPlace = placeParam(options.destination);
  const planModes = options.planModes ?? DEFAULT_PLAN_MODES;
  const startMinute = Math.floor(options.startMs / 60_000);
  // The modes are part of the key: two searches over different modes are two
  // different searches, and one must not answer the other.
  const cacheKey = `${fromPlace}|${toPlace}|${startMinute}|${planModes.join(',')}`;

  const known = unknownOrigins.get(fromPlace);
  if (known !== undefined) throw known;

  let itineraries = cacheGet(cacheKey, options.startMs);
  if (itineraries === null) {
    let coverThrough = Number.NEGATIVE_INFINITY;
    for (const row of rows) if (row.realtime > coverThrough) coverThrough = row.realtime;
    try {
      itineraries = await fetchItineraries(
        fromPlace,
        toPlace,
        options.startMs,
        coverThrough,
        options.stop,
        baseUrl,
        planModes,
        options.fetchImpl,
        options.onDebug,
      );
    } catch (error) {
      // A 404 here means the aggregator has never heard of this origin, which
      // is worth remembering. Keyed on the *resolved* origin rather than the
      // configured stop, so a stop that failed as a parent identifier and then
      // resolved to a platform is asked again under the identifier that might
      // work. Any other failure is a bad moment rather than a bad identifier
      // and must stay retryable.
      if (error instanceof HttpError && error.status === 404) unknownOrigins.set(fromPlace, error);
      throw error;
    }
    cacheSet(cacheKey, options.startMs, itineraries);
  }

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
    const matches = sameMinute.filter((row) => samePlatform(row.platform, first.fromTrack));
    if (matches.length === 0) continue;
    const candidates = optionsFor(first, itinerary, index, earlyBufferMs);
    for (const row of matches) (planned.get(row) as RouteOption[]).push(...candidates);
  }

  return rows.map((row) => {
    const collected = planned.get(row) ?? [];
    const unique = new Map<string, RouteOption>();
    for (const option of collected) {
      const key = optionKey(option);
      const existing = unique.get(key);
      if (existing === undefined || (existing.tight && !option.tight)) unique.set(key, option);
    }
    const list = [...unique.values()]
      .sort((a, b) => a.arrival - b.arrival || a.transfers - b.transfers || Number(a.tight) - Number(b.tight))
      .slice(0, MAX_OPTIONS_PER_ROW);
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
    return { departure: row, best, options: list };
  });
}

// The machine-readable surface, in the same snake_case and ISO instants as the
// rest of the JSON contract. It lives here rather than in `src/json.ts` because
// it serialises this module's own vocabulary and nothing else consumes it.

export function routeLegJson(leg: RouteLeg): unknown {
  return {
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
    arrival: toIso(option.arrival),
    transfers: option.transfers,
    tight: option.tight,
    legs: option.legs.map(routeLegJson),
  };
}

export function plannedRowJson(row: PlannedRow, board: WalkSource, now: number, multiStop: boolean): unknown {
  return {
    departure: departureJson(row.departure, board, now, multiStop),
    best: row.best === null ? null : routeOptionJson(row.best),
    options: row.options.map(routeOptionJson),
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
  boards: PlannedBoard[];
  now: number;
}): unknown {
  return {
    schema_version: SCHEMA_VERSION,
    profile: input.profile,
    requested_profile: input.requestedProfile,
    destination: { key: input.destinationKey, name: input.destinationName },
    early_buffer_minutes: input.earlyBufferMinutes,
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
